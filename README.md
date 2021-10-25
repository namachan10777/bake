# Bake

[![Test](https://github.com/namachan10777/bake/actions/workflows/Test.yml/badge.svg)](https://github.com/namachan10777/bake/actions/workflows/Test.yml)

A make alternative.

```yml
version: 0.0.1
rules:
  a.out:
    in: ${ object.out }
    out: a.out
    command: gcc {{ self.in }}
  object:
    source: ${ glob("*.c") }
    task:
      in: ${ source }
      out: ${ ext("c", ".o", source) }
      command: gcc -c {{source}}
  exe:
    in: a.out
    out: exe
    command: |
      strip a.out
      mv a.out exe
  default:
    in: a.out
```
