# HaskellでWeb開発（初心者向け）
## ScottyとPersistentでJSONをやりとりするAPIを作る

2015/10/17の「[第9回【フリースタイル】もくもく会【学生歓迎！】](http://freestyle-mokumoku.connpass.com/event/20428/)」 でLTしたサンプルコードにちょっと解説足しました

### インストール
```
cd 20151017_scotty_persistent_sample
stack build
```
stack自体のinstallは https://github.com/commercialhaskell/stack#how-to-install から。
stackの使い方は [igrepさんの記事](http://qiita.com/igrep/items/da1d8df6d40eb001a561) を読むと良いと思います。

### 実行
```
stack exec mokumoku-sample-exe
```
これで http://localhost:3000 でサーバが起動します。
