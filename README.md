# cl2rb

The superficial Common Lisp to Ruby transpiler (もげRPG同梱版)

動かすのに多少変更が必要だったので、変更したもげRPG を mogerpg/ に同梱
しました。CL 標準関数で入出力するので、コマンド文字を入力した後エンター
キーを押す必要があります。

★ mogerpg/ ディレクトリ以下のファイルは fusuya さんの作品です。
mogerpg/LICENSE に従って取り扱ってください。

./cl2rb が Lisp 処理系です。ruby 2.2 以降が必要になると思います。また、
sxp と ruby-beautify のジェムが必要です。`$ bundler` とやるとインストー
ルできるかと思います。

# 使い方

* `$ ./cl2rb` で REPL が起動します。
* `./cl2rb --load ほげ.lisp` で ほげ.lisp をロードした後、REPL が起動
  します。
* `./cl2rb --script ほげ.lisp` で ほげ.lisp をロードした後終了します。

ロードされた .lisp ファイルは .rb 拡張子にコンパイルされて .lisp と同
じディレクトリに保存されます。(現状デバッグ用で、.lisp ファイルの替わ
りに読み込まれたりはしません)

# バグ

* `LOAD` 関数で相対パスがカレントディレクトリ相対で解釈されるので、プ
  ログラムが LOAD する全ての .lisp ファイルはカレントディレクトリにあ
  る必要があります。

# もげRPG の動かし方

    $ cd mogerpg
    $ ../cl2rb --script load.lisp
