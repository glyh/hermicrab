### Background
We take inspiration from bash, ruby, perl, tcl and oils. 

### Principles
- Consistency
  - Only 2 sub languages exist in hmc, namely the command language and the expression language
- Extensibility
  - Macros
- Interop-friendly
  - Shell primitives like file descriptors has a generalized alternative in hmc.
- Batteries included: see std
### Notes
#### As a expression evaluator
```
= (1 + 2) # => 3
# if the first token in an command is `(`, it is interpreted as an expression
# but for literals you don't have to
= 1 # => 1
= 'I am raw string with byte \x03\xAF support'
= "I am a ${fancy} string where you ${interpolates} stuff"
= 1123123
= 0x7fff
= 0b101010101110
= 0o3176315673
```

#### Variables
- We have const. const enforced that even for ref type the referenced mem can't be changed as well

#### Comments
```
# single line
#| multiline #|comments that you may
  |# nest
|#
```

#### Strings
```
"""muliline string
  """that has zig syntax
 """and is easy to parse
```

#### Type system
- gradually-typed

#### Primitive Types
In hmc there's following primitive types
- unit type, indicating there's an absense of value.
- strings, bigints, floats
  - For byte string we should take Elixir's approach
- tuples, sets, lists, dicts
- range, and span 
  - range can be converted to list[int], and can't contain reverse index
  - span on the other hand, can have one reverse index representation. They can't be convet to list[int]
- lambdas: `\ a b c (x, y, z) { (x + 1) }`
  - proc definition `proc f a b c (x, y, z) { }`
  - may add `func` keyword later for pure procs
- atoms 
  - (clojure-like-semantic)
  - ~a(1) an atom storing value 1 inside
  - Doesn't have to be a sigil.
- pipeends
  - A generality of file descriptors, they aren't necesssarily backed by a file
  - channel-ends are typed by 2 pair (A, B), where A is the input type and B is the output type
- file paths
  - ~f/home/lyh, follows interpolation as string does
  - ~f"/for/a path that contains/ spaces $and $interpolation"
- environment variables/dynamic scoped variables, starts with a `~d`:
  - `~dABC`, `~dHOME`
- exitcode
  - exit code type, I think it make sense to separte it from plain integer to avoid confusion
  - the unit type is a subtype of exitcode type
  - has representation `~e0` (exit code 0, which is the same as `()`), `~e1` (exit code 1)
    - need some inspiration from elixir
- automatic variables, they are dynamic scoped specifically designed proxy that has specific shell-like behaviors.
  - `~?PATH`
    - When evaluated in command language, it evaluates to a `:` separated string `/usr/bin:/bin:/sbin`
    - When evaluated in expression language, it evaluates to `["/usr/bin", "/bin", "/sbin"]`
  - `~?PID`
    - Behaves the same as `$BASHPID` in bash
    - In expression language evaluates to a int (TODO: maybe pid type?)
  - `~?MASTER_PID`
    - Behaves the same as `$$` in bash
    - In expression language evaluates to a int (TODO: maybe pid type?)
  - `~?CWD` -> `$PWD`
    - In expression language evaluates to a file path
  - `~?INTERACTIVE` -> whether shell is interactive 
    - In expression language evaluates to bool

#### The Command Language

```
f g (a) # f with g as word arg and a as typed arg
f [g (a)] # f with the output of `g(a)` as word arg
x = 2
x += 1 # x += 1, evaluates to ()
# Idea: expand any `x fun= y` into `x = fun(x, y)` automagically
# An edge case for this is `<=, >=, ==, !=`, they are treated separatedly
# When second arg ends with a `=`, this command is parsed as an expression.
# This only work if the second arg is not substituted by variables
$x # => x, stringified
[cmd] # stdout of cmd
$(exp) # evaluated value for expression
y = "yay"
echo 1$x3$[y]wow # prints `123yaywow`
# What if I want to call a command containing `$, { or } in its name?`
Icancontain\$,\{,and\} # calls executable `Icancontain$,{,and}`
f = proc (x) { (x + 1) } # note here the `){` must be next to one another
# blocks
{ } # empty block, returns (), i.e. unit
# Or you can just write 
proc f (a) {
  (x + 1) # hmc is expression based, value of command is returned implicitly
}
```

#### Builtin commands
- cd
- test

#### piping

- Some builtin pipends
  - `~pclose`: `&-`
  - `~pdesc(i)`: i'th file descriptor
    - `~pstdin`: Just `~pdesc(0)` 
    - `~pstdout`: Just `~pdesc(1)` 
    - `~pstderr`: Just `~pdesc(2)` 
  - `~pfile"path"`: file at path, follows path sigil syntax
    - `~pnull`: Just `~pfile/dev/null`
    - `~prandom`: `~pfile/dev/random`

- redirecting can occur at any block 
  ```
  if (a == 1) {

  }>2 else {

  }<3
  ```
#### regexs and globs
  `~r/[a-z][0-9]?/`, `~g*.jpg`

#### Sigils
- `~` still works, but don't put anything right behind it without a space
- `~b` is the bareword types
  - not to be confused with word arg, as word args are strings

#### Macros
 - `quote`: quote
 - `unquote`: unquote

#### Some real code

```
#!/usr/bin/env hmc
# `||` and `&&` are still relevant, they are only allowed in command language
module stdlib/synch || return 0

#
# FIFO File Desriptors
#

proc w1 w2 w3 (t1, t2, t3; k1=w1, k2=kw3; blk) # last arg is a code block
# The (;;;) idiom is just like oils, except we mimic the call site syntax now
# yes in theory you can push the whole `{}` thing inside the arg list but that's not recommended
proc fifo-fd-new w1 w2 w3 (typed1, typed2, typed3; kwarg1, kwarg2) {
  # WARN: this section should be critical but for now it's not
  # A solution may be retry on fail.
  #====================
  setv fifo (mktemp -u)
  mkfifo $fifo
  #====================
  setv fd (open-fd rw (fifo)) # the flag `rw` is the file open mode flags
  # Refering to a typed value in the first position we need to wrap it around `()`
  # one important thing to know: everything is under the same namespace
  # values, binaries, variables, procs, everything.
  1 => inc # 2
  # the `=>` pipe is the ocamel operator `|>`
}

proc fifo-fd-destroy(; fd1) {
  setv fifo-file {readlink /proc/$PID/fd/$fd}
  # Magic variables and environment variables still use bash's variable style
  fd(fd1) | ~pclose # ~pclose stores an pipeend when being piped closed the other end, the same as `&-` in bash
  # space before `(` is not significant but for the sake of readbility it's like that
  ~pclose | fd(fd1)
  rm $fifo-file
}
```
