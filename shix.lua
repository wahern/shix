-- ==========================================================================
-- shix.lua - Shell-based Unix API Bindings
-- --------------------------------------------------------------------------
-- Copyright (c) 2015  William Ahern
--
-- Permission is hereby granted, free of charge, to any person obtaining a
-- copy of this software and associated documentation files (the
-- "Software"), to deal in the Software without restriction, including
-- without limitation the rights to use, copy, modify, merge, publish,
-- distribute, sublicense, and/or sell copies of the Software, and to permit
-- persons to whom the Software is furnished to do so, subject to the
-- following conditions:
--
-- The above copyright notice and this permission notice shall be included
-- in all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
-- OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
-- MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN
-- NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
-- DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR
-- OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE
-- USE OR OTHER DEALINGS IN THE SOFTWARE.
-- --------------------------------------------------------------------------
-- DESCRIPTION
--
-- This Lua module exclusively leverages the shell and a small number of
-- command-line utilities to expose and emulate a subset of the Unix C
-- system interface, particularly routines for manipulating the filesystem
-- (e.g. glob(3), mkdir(2), readdir(3), stat(2)) and, naturally, command
-- invocation (8-bit clean, multi-argument forms of os.execute and
-- io.popen).
--
-- For the most part it only relies on portable behavior. Usually that means
-- the intersection of POSIX-specified behavior and the actual behaviors of
-- system shells (bash, ash, dash, ksh88, ksh93, mksh, pdksh) and utilities
-- (find, pax, sed, etc) as tested on various systems (AIX, FreeBSD, Linux,
-- NetBSD, OpenBSD, OS X, Solaris).
--
-- Despite using the shell it's designed to support 8-bit, NUL-terminated
-- strings (including embedded newlines) in the same manner as the Unix C
-- system interfaces. Other than being limited in its breadth of coverage,
-- the only other significant shortcomings are an inability to provide the
-- same atomicity guarantees as C system calls (stat(2) in particular) and
-- diminished error reporting capabilities (stderr must sometimes be
-- discarded).
--
-- Other than safety and portability, it strives to have mimimal
-- side-effects on the execution environment. As of the initial release it
-- does not make use of any temporary files, either in Lua or from the
-- shell.
--
-- This module is not meant to replace full-featured system modules, but to
-- provide access to a slightly broader range of system interfaces without
-- the burden of a C library dependency.
-- ==========================================================================

-- arc4random
--
-- Simple arc4random implementation. We define this first because we need to
-- generate at load-time a random nonce for our IPC mechanism.
--
-- =========================================================================

local function dehex(s)
	return string.gsub(s, "%x%x", function (n)
		return string.char(tonumber(n, 16))
	end)
end

local function arc4stream(key)
	local S, i, j = {}, 0, 0

	for i=0,255 do
		S[i] = i
	end

	local function swap()
		local S_i, S_j = S[i], S[j]

		S[i] = S_j
		S[j] = S_i
	end

	local function rekey(key)
		for k=0,255 do
			local kp = (k % #key) + 1
			local kc = string.byte(string.sub(key, kp, kp))
			j = (j + S[i] + kc) % 256
			swap()
			i = (i + 1) % 256
		end

		j = i
	end

	local function getbyte()
		i = (i + 1) % 256
		j = (j + S[i]) % 256
		swap()
		return S[(S[i] + S[j]) % 256]
	end

	local function stir()
		local function urandom()
			local fh = io.open("/dev/urandom", "rb")
			local seed

			if fh then
				seed = fh:read(256)
				fh:close()
			end

			return (seed and #seed > 16 and seed) or nil
		end

		local function clock()
			local ok, clk = pcall(string.format, "%A", os.clock())
			return ok and clk or string.format("%.16g", os.clock())
		end

		local seed = urandom()

		if seed then
			rekey(seed)

			return true
		else
			seed = table.concat{ os.date(), clock() }
			rekey(seed)

			return false
		end
	end

	if key then
		rekey(key)
	end

	return getbyte, stir
end

local arc4_count, arc4_getbyte, arc4_stir = 0, arc4stream()

local function arc4random()
	if arc4_count <= 0 then
		arc4_stir()

		for i=1,1024 do
			arc4_getbyte()
		end

		arc4_count = 1600000
	end

	return (arc4_getbyte() * 16777216)
	     + (arc4_getbyte() * 65536)
	     + (arc4_getbyte() * 256)
	     + (arc4_getbyte())
end

-- NOTE: 61 is prime
local function arc4random_nonce61(n)
	local t = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz012345678"

	local function getchars(n)
		if n and n > 0 then
			local r = (arc4random() % #t) + 1
			return string.byte(t, r, r), getchars(n - 1)
		end
	end

	n = n or 8

	return string.format(string.rep("%c", n), getchars(n))
end


-- Shell Code Library
--
-- Shell code chunks to be selectively concatenated together.
--
-- =========================================================================

local code = {}

-- code.epilog
--
-- Code to create a safe, consistent execution environment.
--
local stdout_prefix = arc4random_nonce61(8) -- magic string prefix for IPC

code.epilog = string.format([[
	set -e # strict errors
	set -u # don't expand unbound variables
	set -f # disable pathname expansion
	set -C # noclobber

	unset IFS # reset field splitting
	export LC_ALL=C # no locale headaches

	# print8 STRING
	#
	# Print octal-encoded STRING readable by printf(1) %%b format
	# specifier
	#
	print8() {
		printf "%%s" "${1}" | \
		od -An -to1 -v | \
		sed -e 's/\([0123456789][0123456789]*\)/\\0\1/g' | \
		tr -cd '\\0123456789'
	}

	# decode8 VARIABLE
	#
	# Decode octal-encoded string at VARIABLE, assigning the new string
	# to VARIABLE.
	#
	decode8() {
		IFS=
		eval "${1}=\"\$(printf \"%%bx\" "\${${1}}")\""
		eval "${1}=\${${1}%%x}"
		unset IFS
	}

	# sendmsg STRING ...
	#
	# Encode STRING arguments and print to stdout.
	#
	sendmsg() {
		while [ $# -gt 0 ]; do
			printf "<%%s,%%d> " "%s" "$#"
			print8 "${1}"
			printf "\n"
			shift 1
		done

		printf "<%%s,0>\n" "%s"
	}

]], stdout_prefix, stdout_prefix)


-- code.glob
--
-- glob shell function.
--
code.glob = [[
	# glob PATTERN
	#
	# Print octal-ended expansions of pathname PATTERN, delimited by
	# newline.
	#
	glob() {
		IFS=
		set +f
		set -- ${1}
		set -f
		unset IFS

		for F; do
			[ -e "${F}" ] || continue
			sendmsg "glob" "${F}"
		done
	}
]]


-- minify(STRING)
--
-- Strip comments and compact whitespace.
--
local _minified = {}
local function minify(code, ...)
	if not code then
		return
	elseif not _minified[code] then
		local lines = {}

		for ln in string.gmatch(code, "([^\n]+)") do
			ln = string.gsub(ln, "^%s*#.*", "")
			ln = string.gsub(ln, "%s+#.*", "")
			ln = string.gsub(ln, "^%s*", "")

			if #ln > 0 then
				lines[#lines + 1] = ln
			end
		end

		_minified[code] = table.concat(lines, "\n")
	end

	return _minified[code], minify(...)
end -- minify


-- encode8(STRING)
--
-- Octal-encode string suitable for parsing by printf(3) %b format specifier.
--
local function encode8(s)
	local format = string.format
	local byte = string.byte

	return string.gsub(s, ".", function (c)
		return format("\\0%.3o", byte(c))
	end)
end -- encode8


-- decode8(STRING)
--
-- Octal-decode string.
--
local function decode8(s)
	local tonumber = tonumber
	local char = string.char

	return (string.gsub(s, "\\(0%d+)", function (c)
		return char(tonumber(c, 8))
	end))
end -- decode8


-- Command Object
--
-- Object to build, execute, and read results from shell commands.
--
-- =========================================================================

local cmd = {}
cmd.__index = cmd

local defs = { nomux = false }

function cmd.new(opts)
	local self = setmetatable({ code = {}, stderr = {}, eof = false }, cmd)

	opts = opts or defs

	if not opts.nomux then
		self:addcode[[
			exec 2>&1
		]]
	end

	self:addlib"epilog"

	return self
end -- cmd.new

function cmd.execute(...)
	local cmd = cmd.new()

	cmd:setargs(...)
	cmd:addcode[[
		set +e
		(exec "$@")
		sendmsg "exit" "$?"
	]]

	local exit, status = cmd:result()

	cmd:close()

	if exit == "exit" then
		return (tonumber(status) == 0 or nil), exit, tonumber(status)
	else
		return nil, "exit", 127
	end
end -- cmd.execute

function cmd:addlib(name)
	if not code[name] then
		error(string.format("%s: no such library routine", tostring(name)), 2)
	end

	self.code[#self.code + 1] = minify(code[name])
end -- cmd:addlib

function cmd:addcode(fmt, ...)
	if select('#', ...) > 0 then
		self.code[#self.code + 1] = string.format(fmt, ...)
	else
		self.code[#self.code + 1] = fmt
	end
end --- cmd:addcode

function cmd:setargs(...)
	local args = { }

	for _, arg in ipairs{ ... } do
		arg = tostring(arg)

		if #arg > 0 then
			args[#args + 1] = encode8(arg)
		end
	end

	if #args > 0 then
		self:addcode([[
			A="%s"
			IFS=:
			set -- ${A}
			unset IFS

			I=0
			while [ ${I} -lt $# ]; do
				A="${1}"
				decode8 A
				set -- "$@" "${A}"
				shift 1

				I=$((${I} + 1))
			done
		]], table.concat(args, ":"))
	else
		self:addcode"set --"
	end
end -- cmd:setargs

function cmd:running()
	return self.fh ~= nil
end -- cmd:running()

function cmd:run()
	if not cmd:running() then
		self.fh = io.popen(table.concat(self.code, "\n"), "r")
	end

	return true
end -- cmd:run

function cmd:close()
	if self.fh then
		if io.type(self.fh) == "file" then
			self.fh:close()
		end
	end
end -- cmd:close

function cmd:errors()
	return #self.stderr > 0 and table.concat(self.stderr, "\n") or nil
end -- cmd:errors()

local stdout_pat = string.format("^<%s,(%%d+)>%%s*([\\01234567]*)", stdout_prefix)

function cmd:recvln()
	if self.eof then
		return
	elseif not self:running() then
		self:run()
	end

	local ln = self.fh:read()

	if not ln then
		self.eof = true
		self:close()

		return
	end

	local n, v = ln:match(stdout_pat)

	if n then
		return true, tonumber(n), decode8(v)
	else
		return false, 0, ln
	end
end -- cmd:recvln

local unpack = unpack or table.unpack

function cmd:recvmsg()
	local msg, eom = {}, false

	repeat
		local ok, n, ln = self:recvln()

		if ok then
			if tonumber(n) > 0 then
				msg[#msg + 1] = ln
			else
				eom = true
			end
		elseif ln then
			self.stderr[#self.stderr + 1] = ln
		end
	until not ln or eom

	if eom then
		return #msg, unpack(msg)
	end
end -- cmd:recvmsg

function cmd:results()
	return function ()
		return select(2, self:recvmsg())
	end
end -- cmd:results

function cmd:result(close)
	local function doclose(self, close, ...)
		if close then
			self:close()
		end

		return ...
	end

	return doclose(self, close, select(2, self:recvmsg()))
end -- cmd:result


-- Directory Object
--
-- Object to read directory entries.
--
-- TODO: Improve error handling.
--
-- =========================================================================

local dir = {}
dir.__index = dir

function dir.new(path)
	local self = setmetatable({}, dir)

	-- our readdir protocol cannot tolerate interleaving stdout and stderr
	self.cmd = cmd.new{ nomux = true }
	self.cmd:addcode[[
		exec 2>>/dev/null
	]]

	--
	-- We use the find utility rather than the shell's built-in pathname
	-- expansion to
	--
	-- 	(1) avoid any shell limits--number of pathname expansions
	-- 	might be limited to, e.g., ARG_MAX; and
	--
	-- 	(2) reduce the latency between reporting individual files,
	-- 	    otherwise we have to wait for the shell to read all
	-- 	    files in the directory.
	--
	-- Note that -maxdepth is a GNU extension not supported on Solaris
	-- or AIX. Instead we use -prune to prevent any recursion.
	--
	self.cmd:setargs(path)
	self.cmd:addcode([[
		find "${1}/." -name . -o \
			-type d -prune -exec printf "%s\0\n" {} \; -o \
			-exec printf "%s\0\n" {} \;
	]])

	self.cmd:run()

	self.eof = false
	self.lc = nil
	self.line = {}
	self.file = {}

	return self
end

function dir:parse(buf)
	for ln, nul, eol in buf:gmatch("([^\n%z]*)(%z?)(\n?)") do
		if ln and #ln > 0 then
			self.line[#self.line + 1] = ln
			self.lc = string.sub(ln, #ln, #ln)
		end

		if nul and #nul > 0 then
			self.lc = 0
		end

		if eol and #eol > 0 then
			if self.lc == 0 then
				local path = table.concat(self.line, "")
				local file = path:gsub(".*/", "")

				self.file[#self.file + 1] = file
				self.line = {}
				self.lc = nil
			else
				self.line[#self.line + 1] = eol
				self.lc = string.byte(eol, 1, 1)
			end
		end
	end
end -- dir:parse

function dir:step()
	-- reduce per-file latency by reading in smaller chunks
	local buf = not self.eof and self.cmd.fh:read(100)

	if buf then
		self:parse(buf)

		return true
	else
		self:close()

		return false
	end
end -- dir:step

function dir:read()
	while #self.file == 0 do
		if not self:step() then
			break
		end
	end

	if #self.file > 0 then
		local file = self.file[#self.file]

		self.file[#self.file] = nil

		return file
	end
end -- dir:read

function dir:files()
	return function ()
		return self:read()
	end
end -- dir:files()

function dir:close()
	if not self.eof then
		self.cmd.fh:close()
		self.eof = true
	end
end -- dir:close


-- Core Module Interfaces
--
-- Routines which emulate the Unix C system interface.
--
-- =========================================================================

local bit32 = bit32 or require"bit"
local shix = {}

shix.arc4random_nonce61 = arc4random_nonce61
shix.command = cmd

function shix.execute(...)
	return cmd.execute(...)
end -- shix.execute


function shix.getcwd()
	local cmd = cmd.new()

	cmd:addcode'sendmsg "$(pwd -P)"'

	local cwd = cmd:result(true)

	if cwd and #cwd > 0 then
		return cwd
	else
		return nil, cmd:errors()
	end
end -- shix.getcwd


function shix.getegid()
	local cmd = cmd.new()

	cmd:addcode'sendmsg "$(id -g)"'

	local egid = tonumber(cmd:result(true) or "")

	if egid then
		return egid
	else
		return nil, cmd:errors()
	end
end -- shix.getegid


function shix.geteuid()
	local cmd = cmd.new()

	cmd:addcode'sendmsg "$(id -u)"'

	local euid = tonumber(cmd:result(true) or "")

	if euid then
		return euid
	else
		return nil, cmd:errors()
	end
end -- shix.geteuid


function shix.getgid()
	local cmd = cmd.new()

	cmd:addcode'sendmsg "$(id -gr)"'

	local gid = tonumber(cmd:result(true) or "")

	if gid then
		return gid
	else
		return nil, cmd:errors()
	end
end -- shix.getgid


function shix.getuid()
	local cmd = cmd.new()

	cmd:addcode'sendmsg "$(id -ur)"'

	local uid = tonumber(cmd:result(true) or "")

	if uid then
		return uid
	else
		return nil, cmd:errors()
	end
end -- shix.getuid


function shix.glob(path)
	local files = {}
	local cmd = cmd.new()

	cmd:addlib"glob"
	cmd:setargs(path)
	cmd:addcode'glob "${1}"'

	for type, file in cmd:results() do
		if type == "glob" then
			files[#files + 1] = file
		end
	end

	return files
end -- shix.glob


local function cmode(mode)
	local cmask = bit32.bnot(assert(shix.umask()))
	return bit32.band(assert(mode, "no mode specified"), cmask)
end -- cmode

function shix.mkdir(path, mode)
	mode = string.format("0%.3o", cmode(assert(mode, "no mode specified")))

	return cmd.execute("mkdir", "-m", mode, "--", path)
end -- shix.mkdir


function shix.mkfifo(path, mode)
	mode = string.format("0%.3o", cmode(assert(mode, "no mode specified")))

	return cmd.execute("mkfifo", "-m", mode, "--", path)
end -- shix.mkfifo


function shix.opendir(path)
	return dir.new(path)
end -- shix.opendir

function shix.readdir(dh)
	return dh:read()
end -- shix.readdir

function shix.closedir(dh)
	return dh:close()
end -- shix.closedir

function shix.files(path)
	local dh = dir.new(path)

	return dh:files()
end -- shix.files


function shix.rename(old, new)
	return cmd.execute("mv", "--", old, new)
end -- shix.rename


function shix.sleep(n)
	return cmd.execute("sleep", tonumber(n))
end -- shix.sleep


local function ls_imode(s)
	local t, u, g, o = s:match("^(.)(...)(...)(...)")
	local m = 0
	local irusr, iwusr, ixusr = 0, 0, 0
	local irgrp, iwgrp, ixgrp = 0, 0, 0
	local iroth, iwoth, ixoth = 0, 0, 0
	local isuid, isgid, isvtx = 0, 0, 0

	if t then
		for c in u:gmatch"." do
			if c == "r" then
				irusr = tonumber("0400", 8)
			elseif c == "w" then
				iwusr = tonumber("0200", 8)
			elseif c == "x" then
				ixusr = tonumber("0100", 8)
			elseif c == "S" then
				ixusr = 0
				isuid = tonumber("4000", 8)
			elseif c == "s" then
				ixusr = tonumber("0100", 8)
				isuid = tonumber("4000", 8)
			end
		end

		for c in g:gmatch"." do
			if c == "r" then
				irgrp = tonumber("0040", 8)
			elseif c == "w" then
				iwgrp = tonumber("0020", 8)
			elseif c == "x" then
				ixgrp = tonumber("0010", 8)
			elseif c == "S" then
				ixgrp = 0
				isgid = tonumber("2000", 8)
			elseif c == "s" then
				ixgrp = tonumber("0010", 8)
				isgid = tonumber("2000", 8)
			end
		end

		for c in o:gmatch"." do
			if c == "r" then
				iroth = tonumber("0004", 8)
			elseif c == "w" then
				iwoth = tonumber("0002", 8)
			elseif c == "x" then
				ixoth = tonumber("0001", 8)
			elseif c == "T" then
				ixoth = 0
				isvtx = tonumber("1000", 8)
			elseif c == "s" then
				ixoth = tonumber("0001", 8)
				isvtx = tonumber("1000", 8)
			end
		end

		local perm = irusr + iwusr + ixusr
		           + irgrp + iwgrp + ixgrp
		           + iroth + iwoth + ixoth
		           + isuid + isgid + isvtx

		return t, perm
	end
end

local ls_fields = {
	"ino", "mode", "nlink", "user", "group", "size", "uid", "gid", "type"
}

local function need_ls(st)
	for i=1,#ls_fields do
		if not st[ls_fields[i]] then
			return true
		end
	end

	return false
end

local function ls_stat(path, st)
	if not need_ls(st) then
		return true
	end

	local cmd = cmd.new()

	cmd:setargs(path)
	cmd:addcode[[
		sendmsg "$(ls -ildH -- "${1}")"
		sendmsg "$(ls -ildHn -- "${1}")"
	]]

	local pat = "^%s*(%d+)%s+([^%s]+)%s+(%d+)%s+([^%s]+)%s+([^%s]+)%s+(%d+)%s*"
	local ino, mode, nlink, user, group, size
	local ug_ino, _, uid, gid
	local n = 0

	repeat
		n = n + 1
		ino, mode, nlink, user, group, size = (cmd:result() or ""):match(pat)
		ug_ino, _, _, uid, gid = (cmd:result() or ""):match(pat)
	until ino == ug_ino or n > 3

	cmd:close()

	if ino then
		if ino ~= ug_ino then
			return false, "inconsistent inode data"
		elseif st.ino and st.ino ~= tonumber(ino) then
			return false, "inode number changed"
		end

		st.ino = st.ino or tonumber(ino)

		local type, perm = ls_imode(mode or "")

		if type then
			st.mode = st.mode or perm
			st.type = st.type or type
		end

		st.nlink = st.nlink or tonumber(nlink)
		st.user = st.user or user
		st.group = st.group or group
		st.size = st.size or tonumber(size)

		st.uid = st.uid or tonumber(uid, 8)
		st.gid = st.gid or tonumber(gid, 8)

		return true
	else
		return false, cmd:errors()
	end
end -- ls_stat

local ustar_fields = { "mode", "uid", "gid", "size", "mtime", "user", "group", "type" }

local function need_ustar(st)
	for i=1,#ustar_fields do
		if not st[ustar_fields[i]] then
			return true
		end
	end

	local st_ifmt = st.mode / tonumber("07777", 8)

	if st_ifmt == 0 then
		return true
	end

	return false
end

local ustar_typemap = {
	[0] = "-", [2] = "l", [3] = "c", [4] = "b", [5] = "d", [6] = "p"
}

local function ustar_stat(path, st)
	if not need_ustar(st) then
		return true
	end

	-- NOTE: On NetBSD pax will sometimes issue an interactive prompt to
	-- change the volume. Disable use of pax everywhere for now until
	-- we can investigate further.
	do return false end

	local cmd = cmd.new{ nomux = true }
	cmd:addcode[[
		exec 2>>/dev/null
	]]

	cmd:setargs(path)
	cmd:addcode[[
		(pax -x ustar -wd -- "${1}" || tar cnf - -- "${1}") | dd bs=512 count=1
	]]

	cmd:run()
	local hdr = cmd.fh:read(512)
	cmd:close()

	if hdr and #hdr >= 500 then
		local function getoctal(hdr, p, n)
			return tonumber(hdr:sub(p + 1, p + n) or "", 8)
		end

		local function getname(hdr, p, n)
			local name = hdr:sub(p + 1, p + n)

			return name:match"[^%s%z]+"
		end

		local mode = getoctal(hdr, 100, 8)
		local uid = getoctal(hdr, 108, 8)
		local gid = getoctal(hdr, 116, 8)
		local size = getoctal(hdr, 124, 12)
		local mtime = getoctal(hdr, 136, 12)
		local tflag = getoctal(hdr, 156, 1)
		local user = getname(hdr, 265, 32)
		local group = getname(hdr, 297, 32)
		local devmajor = getoctal(hdr, 329, 8)
		local devminor = getoctal(hdr, 337, 8)
		local type

		if not tflag and getname(hdr, 156, 1) == "\0" then
			tflag = 0
		end

		type = ustar_typemap[tflag]
		size = type == "-" and size or nil

		-- skip if we didn't get any decent data
		if not uid or not gid or not type then
			return true
		end

		-- try to confirm we've read the same file as other methods,
		-- but pax doesn't reliably provide device or inode numbers :(
		if st.uid and uid ~= st.uid then
			return true
		elseif st.gid and gid ~= st.gid then
			return true
		elseif st.size and size and size ~= st.size then
			return true
		end

		-- copy the mode or any missing file format type bits
		if mode and not st.mode then
			st.mode = mode
		elseif mode and st.mode then
			local perm = mode % tonumber("07777", 8)
			local ifmt = mode / tonumber("07777", 8)
			local st_perm = st.mode % tonumber("07777", 8)
			local st_ifmt = st.mode / tonumber("07777", 8)

			if perm == st_perm and st_ifmt == 0 then
				st.mode = mode
			end
		end

		st.type = st.type or type
		st.uid = st.uid or uid
		st.gid = st.gid or gid
		st.size = st.size or size
		st.mtime = st.mtime or mtime
		st.user = st.user or user
		st.group = st.group or group

		return true
	end

	return nil, cmd:errors()
end -- ustar_stat

local function stat_stat(path, st)
	local cmd = cmd.new()

	cmd:setargs(path)
	cmd:addcode[[
		if [ -n "$(command -v stat)" ]; then
			sendmsg "bsd" "$(stat -Ls -- "${1}")"
			sendmsg "gnu" "$(stat -Lc "st_dev=%d st_ino=%i st_mode=%f st_nlink=%h st_uid=%u st_gid=%g st_size=%s st_atime=%X st_mtime=%Y st_ctime=%Z st_blksize=%B st_blocks=%b" -- "${1}")"
		fi
	]]

	local found = {}

	for type, info in cmd:results() do
		if info then
			for k,v in info:gmatch"st_(%w+)=(%d+)" do
				st[k] = st[k] or tonumber(v)
				found[k] = true
			end
		end
	end

	return found.dev and found.ino and found.mode and found.uid and found.gid
end -- stat_stat

function shix.stat(path, field, ...)
	local st = {}

	local stat_ok = stat_stat(path, st)
	local ls_ok, ls_why = ls_stat(path, st)
	local ustar_ok = ustar_stat(path, st)

	if not stat_ok and not ls_ok and not ustar_ok then
		-- ls is only command we can read stderr from
		return nil, ls_why
	end

	if field then
		local function pushfields(utsname, field, ...)
			if field then
				return st[field], pushfields(st, ...)
			end
		end

		return pushfields(st, field, ...)
	else
		return st
	end
end -- shix.stat


function shix.umask()
	local cmd = cmd.new()

	cmd:addcode[[
		sendmsg "$(umask)"
	]]

	local cmask = tonumber(cmd:result(true) or "", 8)

	if cmask then
		return cmask
	else
		return nil, cmd:errors()
	end
end -- shix.umask


function shix.uname(field, ...)
	local cmd = cmd.new()

	cmd:addcode[[
		set -- s sysname n nodename r release v version m machine

		while [ $# -ge 2 ]; do
			sendmsg "${2}" "$(uname "-${1}")"
			shift 2
		done
	]]

	local utsname = {}

	for k, v in cmd:results() do
		if v and #v > 0 then
			utsname[k] = v
		end
	end

	if field then
		local function pushfields(utsname, field, ...)
			if field then
				return utsname[field], pushfields(utsname, ...)
			end
		end

		return pushfields(utsname, field, ...)
	else
		return utsname
	end
end -- shix.uname


function shix.unlink(path)
	return cmd.execute("rm", "--", path)
end -- shix.unlink


return shix
