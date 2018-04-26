# Readerモナドについて勉強

## 準備

stack install mtl
が必要。

import Control.Monad.Reader
が必要。


## Readerモナドとは

環境（環境変数のようなグローバルでreadonlyな変数）を引数で引き回さなくても参照できるようにするもの。

runReader, ask がキーポイント。

- runReaderは、第1引数の関数に対し、第2引数を環境として与えて実行する
- runReaderの第1引数の関数は、Readerモナド値を返すこと
- runReaderの第1引数の関数の中では、askで環境を取得できる

Reader e a は IO a に似ていることを意識すると分かりやすい。

| Reader [Int] String            | IO String            |
|--------------------------------|----------------------|
| hoge :: Reader [Int] String    | getLine :: IO String |
| e <- ask                       | line <- getLine      |


- asks 関数は現在の環境を引数とする関数の返り値を得るユーティリティ関数
- local 関数は、環境に一時的に変更を加えて別のReaderモナドに渡す関数
