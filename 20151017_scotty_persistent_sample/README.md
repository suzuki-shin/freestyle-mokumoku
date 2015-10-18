# HaskellでWeb開発（初心者向け）
## ScottyとPersistentでJSONをやりとりするAPIを作る

2015/10/17の「[第9回【フリースタイル】もくもく会【学生歓迎！】](http://freestyle-mokumoku.connpass.com/event/20428/)」 でLTしたサンプルコードにちょっと解説足しました

プログラムの中身は説明のために作ったもので、機能自体に意味はありません。
[Persistent](http://www.stackage.org/package/persistent)（データ永続化ライブラリ）と[Scotty](https://hackage.haskell.org/package/scotty)（ウェブフレームワーク）で作っています。DBにはSQLiteを使っています。

LTで伝えたかったことは、HaskellでPersistentとScottyを使うと少ない記述量でJSONをやり取りするAPIが作れるよ、ということでした。

### インストール
```
git clone https://github.com/suzuki-shin/freestyle-mokumoku.git
cd freestyle-mokumoku/20151017_scotty_persistent_sample
stack build
```
stack自体のinstallは https://github.com/commercialhaskell/stack#how-to-install から。
stackの使い方は [igrepさんの記事](http://qiita.com/igrep/items/da1d8df6d40eb001a561) を読むと良いと思います。

### 実行
```
stack exec mokumoku-sample-exe
```
これで http://localhost:3000 でサーバが起動します。

### ソースコード
- app/Main.hs
 - Controller.runを呼んでいるだけ
- src/Controller.hs
 - ルーティング
- src/Model.hs
 - User型とChat型が定義されていて、それぞれの取得と追加の関数が定義されています。
- src/Model/Type.hs
 - Skill型が定義されています。
