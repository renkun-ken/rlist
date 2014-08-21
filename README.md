

# rlist

[![Build Status](https://travis-ci.org/renkun-ken/rlist.png?branch=master)](https://travis-ci.org/renkun-ken/rlist)

rlist is a set of tools for working with list objects. Its goal is to make it easier to work with lists by providing a wide range of functions on non-tabular data stored in them.

This package supports list filtering, mapping, grouping, sorting, updating, searching, file input/output, and many other functions. It implements collection pipeline and strongly recommends functional programming style in list operations.

## What's new in 0.3?

- **API Break**: `list.search` now evaluates expression recursively in a list and supports lambda expression.
- Add `equal()` function for logical and fuzzy filtering and searching which supports exact equality, atomic equality, inclusion, pattern matching, string-distance tolerance.
- Add `List()` to provide an environment in which most list functions are defined for light-weight chaining that does not rely on external operators.

[Release notes](https://github.com/renkun-ken/rlist/releases)

## Installation

You can install the latest released version from [CRAN](http://cran.r-project.org/web/packages/rlist/) with

```r
install.packages("rlist")
```

or the latest development version from GitHub with

```r
devtools::install_github("rlist","renkun-ken")
```

## Getting started

The package provides a wide range of high-level functions to work with list objects.

Suppose we have a list of developers, each of whom has a name, age, a few interests, a list of programming languages they use and the number of years they have been using them.


```r
library(rlist)
devs <- 
  list(
    p1=list(name="Ken",age=24,
      interest=c("reading","music","movies"),
      lang=list(r=2,csharp=4,python=3)),
    p2=list(name="James",age=25,
      interest=c("sports","music"),
      lang=list(r=3,java=2,cpp=5)),
    p3=list(name="Penny",age=24,
      interest=c("movies","reading"),
      lang=list(r=1,cpp=4,python=2)))
```

This type of data is non-relational since it does not well fit the shape of a data table yet it can be easily stored in JSON or YAML format. In R, list object is powerful enough to represent a wide range of non-relational datasets like this. This package provides a wide range of functions to query this type of data.

## Examples

Filter those who like music and has been using R for more than 3 years.


```r
subset1 <- list.filter(devs, "music" %in% interest & lang$r >= 3)
str(subset1)
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

Select their names and ages.


```r
subset2 <- list.select(devs, name, age)
str(subset2)
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

Map each of them to the number of interests.


```r
result <- list.map(devs, length(interest))
str(result)
```

```
List of 3
 $ p1: int 3
 $ p2: int 2
 $ p3: int 2
```

## Lambda expression

In this package, almost all functions that work with expressions accept the following forms of lambda expressions:

- Implicit lambda expression: `g(x)`
- Univariate lambda expressions: 
    * `x ~ g(x)`
    * `f(x) ~ g(x)`
- Multivariate lambda expressions:
    * `f(x,i) ~ g(x,i)`
    * `f(x,i,name) ~ g(x,i,name)`

where `x` refers to the list member itself, `i` denotes the index, `name` denotes the name. If the symbols are not explicitly declared, `.`, `.i` and `.name` will by default be used to represent them, respectively.

```r
nums <- list(a=c(1,2,3),b=c(2,3,4),c=c(3,4,5))
list.map(nums, c(min=min(.),max=max(.)))
list.filter(nums, x ~ mean(x)>=3)
list.map(nums, f(x,i) ~ sum(x,i))
```

## Using pipeline

### Working with pipeR

Query the name of each developer who likes music and uses R, and put the results in a data frame.


```r
library(pipeR)
devs %>>% 
  list.filter("music" %in% interest & "r" %in% names(lang)) %>>%
  list.select(name,age) %>>%
  list.stack
```

```
   name age
1   Ken  24
2 James  25
```

The example above uses `pipeR`(http://renkun.me/pipeR/) package for pipeline operator `%>>%` that chains commands in a fluent style.

### List environment

`List()` function wraps a list within an environment where almost all list functions are defined. Here is the List-environment version of the previous example.


```r
m <- List(devs)
m$filter("music" %in% interest & "r" %in% names(lang))$
  select(name,age)$
  stack()$
  data
```

```
   name age
1   Ken  24
2 James  25
```


## Vignettes

The package also provides detailed vignettes for most functions. 

- [Introduction to rlist](http://cran.r-project.org/web/packages/rlist/vignettes/Introduction.html)
- [List Mapping](http://cran.r-project.org/web/packages/rlist/vignettes/Mapping.html)
- [List Filtering](http://cran.r-project.org/web/packages/rlist/vignettes/Filtering.html)
- [List Sorting](http://cran.r-project.org/web/packages/rlist/vignettes/Sorting.html)
- [List Grouping](http://cran.r-project.org/web/packages/rlist/vignettes/Grouping.html)
- [List Joining](http://cran.r-project.org/web/packages/rlist/vignettes/Joining.html)
- [List Updating](http://cran.r-project.org/web/packages/rlist/vignettes/Updating.html)
- [List Searching](http://cran.r-project.org/web/packages/rlist/vignettes/Searching.html)
- [List Input/Output](http://cran.r-project.org/web/packages/rlist/vignettes/IO.html)
- [Lambda expressions](http://cran.r-project.org/web/packages/rlist/vignettes/Lambda.html)
- [List environment](http://cran.r-project.org/web/packages/rlist/vignettes/List.html)
- [Examples](http://cran.r-project.org/web/packages/rlist/vignettes/Examples.html)

## Help overview

```r
help(package = rlist)
```

or view the documentation on [CRAN](http://cran.r-project.org/web/packages/rlist/rlist.pdf)

## License

This package is under [MIT License](http://opensource.org/licenses/MIT).
