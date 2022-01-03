

# rlist

<!-- badges: start -->
[![R-CMD-check](https://github.com/renkun-ken/rlist/workflows/R-CMD-check/badge.svg)](https://github.com/renkun-ken/rlist/actions)
<!-- badges: end -->
[![codecov.io](https://codecov.io/github/renkun-ken/rlist/coverage.svg?branch=master)](https://codecov.io/github/renkun-ken/rlist?branch=master)
[![CRAN Version](https://www.r-pkg.org/badges/version/rlist)](https://cran.r-project.org/package=rlist)

rlistはlistオブジェクトを操作するためのツール群です。このライブラリの目標は、listの中に格納されている表形式(data.frame)ではないデータを操作するための、幅広い関数をユーザに提供することで、listを操作しやすくすることです。

このパッケージは、listに対するマップ, フィルタリング、グルーピング、ソート、更新、検索、ファイル入出力などの数多くの関数を提供するものです。このパッケージにおける大半の関数は、listに対するデータ処理において、チェーン(連鎖)操作できるように、"パイプ・フレンドリー"な設計となっています。

**rlistの完璧なガイドとしては、[rlist Tutorial](https://renkun-ken.github.io/rlist-tutorial/)を強くお勧めします。**

## インストール

最新版をGithubからインストールする:

```r
devtools::install_github("renkun-ken/rlist")
```

[CRAN](https://cran.r-project.org/package=rlist)からインストールする:

```r
install.packages("rlist")
```

## モチベーション

Rにおいては、data.frameに代表されるような表形式で格納されている構造化されたデータを扱うための強力なツールがたくさんあります。しかし、ある種のデータは非表形式であり、例えば異なるDBなどのレコードが異なる属性(列)を持っているようなケースがあり、それぞれの属性(列)が異なる数値を格納しているようなケースも考えられます。

このような形式のデータをdata.frameに格納するのは難しいことではある一方、Rにおいては`list`オブジェクトは、このようなレコードの違い、あるいは多様性といったものを表現するために十分フレキシブルな構造を持っています。rlistは`list`オブジェクトにため込まれた非構造データを処理するためのツールボックスであり、listに対するパイプ・フレンドリーな多数の高階関数を提供するものです。

## はじめに

まず、私たちが、名前・年齢・趣味(複数)・使用するプログラミング言語、およびその使用年数を持つ"開発者"のリストを持っているとしましょう。


```r
library(rlist)
devs <- 
  list(
    p1=list(name="Ken",age=24,
      interest=c("reading","music","movies"),
      lang=list(r=2,csharp=4)),
    p2=list(name="James",age=25,
      interest=c("sports","music"),
      lang=list(r=3,java=2,cpp=5)),
    p3=list(name="Penny",age=24,
      interest=c("movies","reading"),
      lang=list(r=1,cpp=4,python=2)))
```

これはデータフレームの形とは合わないので、データのタイプとしては、関係データベースの用語でいうところの非リレーショナルなものです。しかし、JSONやYAMLのデータ形式としては、容易に格納される形ではあります。Rにおいては、listオブジェクトは、このような幅広い非リレーショナルなデータを表現するための十分な柔軟性があります。このパッケージは、この手のデータに対する問い合わせ(Queryの発行)や操作ための幅広い関数を提供します。

次で示す例においては、出力のデータ構造を示すために、`str()`を使います。

### フィルタリング

音楽が好きで3年以上のRの経験がある開発者をフィルタリングして抽出してみましょう。


```r
str( list.filter(devs, "music" %in% interest & lang$r >= 3) )
```

```
List of 1
 $ p2:List of 4
  ..$ name    : chr "James"
  ..$ age     : num 25
  ..$ interest: chr [1:2] "sports" "music"
  ..$ lang    :List of 3
  .. ..$ r   : num 3
  .. ..$ java: num 2
  .. ..$ cpp : num 5
```

### 選択

彼らの名前・年齢だけを選択してみましょう。


```r
str( list.select(devs, name, age) )
```

```
List of 3
 $ p1:List of 2
  ..$ name: chr "Ken"
  ..$ age : num 24
 $ p2:List of 2
  ..$ name: chr "James"
  ..$ age : num 25
 $ p3:List of 2
  ..$ name: chr "Penny"
  ..$ age : num 24
```

### マッピング

開発者のリストデータを、趣味の数のデータへとmap(変換)してみましょう。


```r
str( list.map(devs, length(interest)) )
```

```
List of 3
 $ p1: int 3
 $ p2: int 2
 $ p3: int 2
```

### その他の関数群

これらの基本的な関数に加えて、rlistはグルーピング・結合・検索・ソート・更新などの様々な関数もサポートしています。その他の機能については、[rlist Tutorial](https://renkun-ken.github.io/rlist-tutorial/)を読んでください。

## ラムダ式 

このパッケージにおいて、ほとんどすべての関数は、次に示すラムダ式の形式を使用することができます：
  
- 暗黙のラムダ式: `expression`
- 単変数のラムダ式: 
    * `x ~ expression`
    * `f(x) ~ expression`
- 複数変数のラムダ式:
    * `f(x,i) ~ expression`
    * `f(x,i,name) ~ expression`

ここで、`x` はリストの要素を表し、`i` はリストのインデックス、`name` はリストの要素の名前を表します. もし、`x, i, name`といったシンボルが明示的に与えられなかった場合、`.`、`.i` 、`.name` がそれぞれのデフォルトのオブジェクトとして使われます。
           
```r
nums <- list(a=c(1,2,3),b=c(2,3,4),c=c(3,4,5))
list.map(nums, c(min=min(.),max=max(.)))
list.filter(nums, x ~ mean(x)>=3)
list.map(nums, f(x,i) ~ sum(x,i))
```

## パイプを使う

### パイプ構文の操作

音楽が好きでRを使う開発者の名前を問い合わせ、その結果をdata.frameとして出力してみましょう。

```r
devs |> 
  list.filter("music" %in% interest & "r" %in% names(lang)) |>
  list.select(name,age) |>
  list.stack()
```

```
   name age
1   Ken  24
2 James  25
```

上記の例では、R4.1で導入されたパイプ構文 `|>`を使用して、コマンドを流暢なスタイルでチェーンします。

### List環境

`List()` 関数はほとんど全てのlist関数が定義された環境とデータであるlistを１つにまとめあげるものです。先ほどの例をList環境を用いて書いたものが以下になります。


```r
ldevs <- List(devs)
ldevs$filter("music" %in% interest & "r" %in% names(lang))$
  select(name,age)$
  stack()$
  data
```

```
   name age
1   Ken  24
2 James  25
```

## ライセンス

このパッケージは[MIT License](https://opensource.org/licenses/MIT)の下で公開されています。
