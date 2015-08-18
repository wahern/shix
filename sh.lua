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

local function tocommand(...)
	return table.concat({ minify(code.epilog, ...) }, "\n")
end -- tocommand


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


-- cmd object
--
--
local cmd = {}
cmd.__index = cmd

local defs = { nostderr = false }

function cmd.new(opts)
	local self = setmetatable({ code = {}, stderr = {}, eof = false }, cmd)

	opts = opts or defs

	if not opts.nostderr then
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

	if #args > 1 then
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


local sh = {}

function sh.execute(...)
	return os_execute(tocommand(setargs(...), [[
		exec "$@"
	]]))
end -- sh.execute


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


-- sh.opendir
--
-- TODO: Use find(1) to work around any glob expansion limit.
--
function sh.opendir(path)
	local cmd = cmd.new()

	cmd:addlib"glob"
	cmd:addcode([[
		P="%s"
		decode8 P
		cd "${P}"
		glob "*"
	]], encode8(path))

	return cmd
end -- sh.opendir

function sh.readdir(cmd)
	for type, file in cmd:results() do
		if type == "glob" then
			return file
		end
	end
end -- sh.readdir

function sh.closedir(cmd)
	return cmd:close()
end -- sh.closedir


function sh.rename(old, new)
	return cmd.execute("mv", "--", old, new)
end -- sh.rename


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


function sh.unlink(path)
	return cmd.execute("rm", "--", path)
end -- sh.unlink


return sh
