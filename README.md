# Shell-based Unix API Lua Module

```shix``` is a Unix API Lua module which uses the POSIX shell instead of C
or FFI bindings, and thus has no dependencies and and can be used as-is. It's
limited to a relatively small subset of Unix filesystem and process
interfaces, but is still immensely useful in a pinch. I wrote it for a
project where importing my Lua Unix C-based module seemed like too much
trouble. Turned out that it wasn't too much trouble and I ended up using Lua
Unix, but I still think ```shix``` is a neat idea and worthwhile.

## Isn't the shell insufficient and insecure?

Correct shell programming is difficult. It certainly wasn't intended as a
general purpose language. But it's ubiquitous and a least common
denominator, second perhaps only to C.

One of my hobbies is portable C programming and portable Unix programming.
Over the years I've developed some well-tested shell constructs for use in
various scripts and which I've condensed to some basic primitives. To the
best of my knowledge they're safe and correct per POSIX. Though there
doesn't exist any shell that I'm aware of which is 100% POSIX compliant, I
try to test comprehensively on various system-shell implementations,
including bash, ksh88, ksh93, ash, pdksh, and derivatives thereof.
Nonetheless there may be some corner cases that break on some shells. Caveat
emptor.

While the primitives should be safe and POSIX compliant, some of features
require non-POSIX functionality. For example, the ```stat()``` routine
relies on the presence of a GNU-like or BSD-like ```stat(1)``` utility for
some file attributes, as ```ls(1)``` doesn't report all the members of
```struct stat```.

### Safe string handling

Communicating 8-bit strings is difficult, but not impractical, at least if
we exclude the NUL byte. See the shell routines ```print8``` and
```decode8``` defined in the code.epilog string, which use the ```od(1)```
utility for encoding and the ```printf(1)``` utility for decoding.

The string handling primitives are at the core of ```shix```, permitting
passing arbitrary strings between the shell and Lua using the only
facilities Lua provides, ```os.system``` and ```io.popen```. (Well,
excluding the use of temporary files, at least.)

### Safe directory reading

I've also developed a POSIX-compliant glob routine which permits handling
8-bit filenames. Notably it does not use ```find(1)```, but merely the
built-in shell globbing facility and a callback.

### Safe shell programming

All shell execution environments are initialized with

```sh
set -e # strict errors
set -u # don't expand unbound variables
set -f # disable pathname expansion
set -C # noclobber
```

## Fundamental Limitations

### Atomicity and Consistency

Unlike system calls, these bindings involve invoking subprocesses. Sometimes
an operation must be synthesized using multiple suboperations, whereas the
related C system call would be atomic--e.g. applying umask to file creation
modes. File system operations can't operate on descriptors directly. The
implications of these and similar limitations (see Failure Detection below)
can be subtle. This module probably shouldn't be used from privileged
processes without significant consideration.

(Indeed, some of the related C APIs shouldn't be used from privileged
processes at all, no matter the bindings module or language. My Lua Unix
module goes to great lengths to bind more modern, safer C API interfaces,
implemented in a race-free manner, and with the addition of auxiliary
interfaces which assist secure programming. This often means binding
portable-in-practice interfaces which may not be defined by POSIX, a
deficiency of modules like Lua POSIX.)

### Sensitive Data

Arguments are communicated to subshells using the argument vector, which is
usually readable by any user on the system. In the future a mechanism using
temporary files could be optionally used to communicate arguments.

### Process State

Because operations are performed in subprocesses, the application process
state cannot be modified. For example, it's not possible to change the
current working directory of the application.

### Performance

For simple tasks this module should be sufficient. For heavy process
management and filesystem manipulation performance will be quite slow.

### Failure Detection

The module cannot report C system error codes. Failure is usually binary.
Sometimes failure cannot be reported at all.

## Module API

### closedir(dh:object)

Close the directory handle.

### command.new([opts:table])

Return a command execution object, used to build, execute, and read results
from a subshell. This is currently an internal API and subject to change,
but can be useful for extending the module.

### command:addlib(name:string)

Add the specified library routine to the subshell invocation commandline.
E.g. "glob".

### command:addcode(fmt[, ...])

Add specified code the subshell invocation commandline. This code is not
escaped, so do not put arbitrary strings here. Use ```command:setargs```
instead.

### command:close()

FIXME

### command:recvmsg()

FIXME

### command:result()

FIXME

### command:results()

FIXME

### command:run()

FIXME

### command:setargs(arg:string[, ...])

Translate the specified string to shell code which sets the positional
argument list and add the generated code to the subshell invocation
commandline.

### files(path:string)

Opens a directory handle to path and returns an iterator over ```readdir```.

### getcwd()

Return the current working directory.

### getegid()

Return the effective group id.

### geteuid()

Return the effective user id.

### getgid()

Return the real group id.

### getuid()

Return the real user id.

### glob(pattern:string)

Return an array of path names generated by the glob pattern.

### mkdir(path:string, mode:integer)

Create directory at path.

### mkfifo(path:string, mode:integer)

Create named FIFO at path.

### opendir(path:string)

Return a directory handle. Unlike in C this is not a handle to an open
directory descriptor; it's a wrappar around a Lua file handle to the stdout
of a subshell sending the directory contents.

### readdir(dh:object)

Return the next directory entry name.

### rename(from-path:string, to-path:string)

Rename from-path to to-path. Uses ```mv(1)```.

### sleep(seconds:integer)

Suspend process for specific number of seconds.

### stat(path:string[, field:string[,...]])

Report attributes of file at path. If no additional arguments are specified,
returns a table with the same named fields as in the ```struct stat``` C
interface, but without the ```st_``` prefix. Otherwise additional arguments
are interpreted as field names specifying which values to report in the
return list.

Uses ```ls(1)``` and ```stat(1)``` (if available). There's code which uses
```pax(1)``` to query some attributes, but it's disabled because of a bug in
NetBSD's implementation which breaks non-interactive use.

### umask()

Return the process file creation mask as a number.

### uname([field:string[, ...]])

Report system name information. If no arguments are specified, returns a
table with the same named fields as in the ```struct utsname``` C interface.
Otherwise the arguments are interpreted as field names specifying which
values to report in the return list.

### unlink(path:string)

Unlink the specified path.  Uses ```rm(1)```.
