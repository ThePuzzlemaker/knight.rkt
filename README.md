# knight.rkt

An implementation of [sampersand](https://github.com/sampersand)'s [Knight](https://github.com/knight-lang/knight-lang) in Racket.

## Implementation-specific behaviour

- All UB (as far as I can tell) is caught, usually in the form of an exception
- Strings and ASCII (probably) allow unicode
- `=` allows dynamic variables via a string for its first variable
- `VALUE` is implemented
- `USE` is implemented
- `EVAL` is implemented
- `$` will be implemented Soon(tm)
