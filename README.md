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

## Document

## Task

* 入力を受け取って出力する単位。GNU Makeのルールと同じ
* 最初に`in`と`out`を評価し、その結果が`self.in`と`self.out`に代入されて`command`が評価される
* `out`は`string`もしくは`[string]`
* `in`は`string`もしくは`[string]`、または`{ [string]: string | [string] }`となる
  * `in.src`や`in.header`などの形で参照できる
* `command`は複数行に分けて書くと一行一コマンドと見做して実行される
* `.ONESHELL`相当の機能はない
* 特殊な入力として`:`から始まるタスクシンボルがある
  * `:test`などとすると`:test`ファイルではなく`test`タスクに依存する
  * GNU Makeの`.PHONY`相当の機能

```yml
in: main.c
out: main.o
command: gcc -c {{ self.in }}
```

### Rule

* 複数のTaskを生成するMapルールと単一のタスクのみ生成するSingleルールがある
* 以下のように`source`に`[string]`を指定すると
`task`内で`source`として参照出来る
* 複数のタスクを生成するので並列に実行が可能
* Singleの場合は`task:`の内容をそのまま`test`以下に書く

```yaml
rules:
  test:
    source: std.glob("**/*.c")
    task:
      in: {{ source }}
      out: {{ std.ext(source, "c", "o") }}
      command: gcc -c {{ self.in }}
```

### 構文

* 基本はYAML
* `rule.*.source`などの評価される部分は式として扱われる
  * `command`のみテンプレート文字列
    * テンプレート文字列は`{{ expression }}`の場合`expression`が展開される
  * 識別子は`.`区切りで先頭は大文字/小文字のアルファベットで、先頭以降はアルファベット, `-`, `_`, 数字
  * `identifier(argument)`で関数呼び出し
  * `"`でクオートすると文字列
  * 整数はそのまま
  * 浮動小数点数は`0.0`のように`.`前後の数字は省略できない
  * 識別子のうち`true`と`false`はそれぞれ真と偽になる
  * 配列は`[]`で囲い、`,`区切り

### 言語仕様

* 関数の引数が足りない場合はエラーではなく部分適用と見做される

### 標準ライブラリ

* `std.`から始まる

#### `glob(pattern: string): [string]`

* globによってファイルとディレクトリのリストを得る

#### `ext(ext_from: string, ext_to: string, file: string): string`

* `file`の拡張子を`ext_from`から`ext_to`に付け替える
* 拡張子がマッチしない場合はそのまま

#### `join(strings: [string], separator: string): string`

* `strings`を`separator`で結合する
