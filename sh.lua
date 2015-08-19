local code = {}

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
		local fh = io.open("/dev/urandom", "rb")

		if not fh then return false end

		local seed = fh:read(256)

		fh:close()

		if not seed then return false end

		rekey(seed)

		return true
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

local function random_c(n)
	-- NB: 61 is prime
	local t = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz012345678"

	if n and n > 0 then
		local r = (arc4random() % #t) + 1
		return string.sub(t, r, r), random_c(n - 1)
	end
end

local prefix = string.format(string.rep("%s", 8), random_c(8))

-- code.epilog
--
-- Code to create a safe, consistent execution environment.
--
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

]], prefix, prefix)


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
		return (status == 0 and true or nil), exit, tonumber(status)
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
		if #arg > 1 then
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

local stdout_pat = string.format("^<%s,(%%d+)>%%s*([\\01234567]*)", prefix)

function cmd:recvln(fh)
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

function cmd:result()
	return select(2, self:recvmsg())
end -- cmd:result


-- Directory Object
--
-- Object to read directory entries.
--
-- TODO: Improve error handling.
--
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
--
local sh = {}

function sh.execute(...)
	return os_execute(tocommand(setargs(...), [[
		exec "$@"
	]]))
end -- sh.execute


function sh.getcwd()
	return nil
end -- sh.getcwd


function sh.glob(path)
	local files = {}
	local cmd = cmd.new()

	cmd:addlib"glob"
	cmd:addcode([[
		P="%s"
		decode8 P
		glob "${P}"
	]], encode8(path))

	for type, file in cmd:results() do
		if type == "glob" then
			files[#files + 1] = file
		end
	end

	return files
end -- sh.glob


function sh.mkdir(path, mode)
	mode = string.format("0%.3o", assert(mode, "no mode specified") - assert(sh.umask()))

	return cmd.execute("mkdir", "-m", mode, "--", path)
end -- sh.mkdir


function sh.mkfifo(path, mode)
	mode = string.format("0%.3o", assert(mode, "no mode specified") - assert(sh.umask()))

	return cmd.execute("mkfifo", "-m", mode, "--", path)
end -- sh.mkfifo


function sh.opendir(path)
	return dir.new(path)
end -- sh.opendir

function sh.readdir(dh)
	return dh:read()
end -- sh.readdir

function sh.closedir(dh)
	return dh:close()
end -- sh.closedir

function sh.files(path)
	local dh = dir.new(path)

	return dh:files()
end -- sh.files


function sh.rename(old, new)
	return cmd.execute("mv", "--", old, new)
end -- sh.rename


local function stat_ls(path, st)
	local cmd = cmd.new()

	cmd:setargs(path)
	cmd:addcode[[
		sendmsg "$(ls -ildH -- "${1}")"
	]]

	cmd:run()
	local ln = cmd:result() or ""
	cmd:close()

	st = st or {}

	local serial, mode, nlink, user, group, size, p = ln:match("^(%d+)%s+([^%s]+)%s+(%d+)%s+([^%s]+)%s+([^%s]+)%s+(%d+)%s*()")

	if serial then
		st.ino = st.ino or tonumber(serial)
		st.mode = st.mode or mode
		st.nlink = st.nlink or nlink
		st.uid = st.uid or user
		st.gid = st.gid or group
		st.size = st.size or tonumber(size)

		return st
	else
		return nil, cmd:errors()
	end
end -- stat_ls

local function stat_ustar(path, st)
	local cmd = cmd.new{ nomux = true }
	cmd:addcode[[
		exec 2>>/dev/null
	]]

	cmd:setargs(path)
	cmd:addcode[[
		(pax -x ustar -wd "${1}" || tar cnf - "${1}") | dd bs=512 count=1
	]]

	cmd:run()
	local hdr = cmd.fh:read(512)
	cmd:close()

	st = st or {}

	return nil, cmd:errors()
end -- stat_ustar

local function stat_stat(path, st)
	local cmd = cmd.new()

	cmd:setargs(path)
	cmd:addcode[[
		if [ -n "$(command -v stat)" ]; then
			sendmsg "bsd" "$(stat -Ls "${1}")"
			sendmsg "gnu" "$(stat -Lc "st_dev=%d st_ino=%i st_mode=%f st_nlink=%h st_uid=%u st_gid=%g st_size=%s st_atime=%X st_mtime=%Y st_ctime=%Z st_blksize=%B st_blocks=%b" "${1}")"
		fi
	]]

	local ok = false

	st = st or {}

	for type, info in cmd:results() do
		if info then
			for k,v in info:gmatch"st_(%w+)=(%d+)" do
				st[k] = st[k] or tonumber(v)
				ok = true
			end
		end
	end

	return ok and st or nil
end -- stat_stat

function sh.stat(path, field, ...)
	local st = {}

	local stat_ok = stat_stat(path, st)
	local ls_ok, ls_why = stat_ls(path, st)

	if not stat_ok and not ls_ok then
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
end -- sh.stat


function sh.umask()
	local cmd = cmd.new()

	cmd:addcode[[
		sendmsg "$(umask)"
	]]

	local cmask = tonumber(cmd:result() or "", 8)

	cmd:close()

	if cmask then
		return cmask
	else
		return nil, cmd:errors()
	end
end -- sh.umask


function sh.uname(field, ...)
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
end -- sh.uname


function sh.unlink(path)
	return cmd.execute("rm", "--", path)
end -- sh.unlink


return sh
