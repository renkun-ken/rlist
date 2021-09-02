

# rlist

[![Linux Build Status](https://travis-ci.org/renkun-ken/rlist.png?branch=master)](https://travis-ci.org/renkun-ken/rlist) 
[![Windows Build status](https://ci.appveyor.com/api/projects/status/github/renkun-ken/rlist?svg=true)](https://ci.appveyor.com/project/renkun-ken/rlist)
[![codecov.io](http://codecov.io/github/renkun-ken/rlist/coverage.svg?branch=master)](http://codecov.io/github/renkun-ken/rlist?branch=master)
[![CRAN Version](http://www.r-pkg.org/badges/version/rlist)](http://cran.rstudio.com/web/packages/rlist)

rlist is a set of tools for working with list objects. Its goal is to make it easier to work with lists by providing a wide range of functions that operate on non-tabular data stored in them.

This package supports list mapping, filtering, grouping, sorting, updating, searching, file input/output, and many other functions. Most functions in the package are designed to be pipeline friendly so that data processing with lists can be chained.

**[rlist Tutorial](http://renkun.me/rlist-tutorial) is a highly recommended complete guide to rlist.**

This document is also translated into  [日本語](https://github.com/renkun-ken/rlist/blob/master/README.ja.md) (by [@teramonagi](https://github.com/teramonagi)).

## Installation

Install the latest version from GitHub:

```r
devtools::install_github("renkun-ken/rlist")
```

Install from [CRAN](http://cran.r-project.org/web/packages/rlist/):

```r
install.packages("rlist")
```

## Motivation

In R, there are numerous powerful tools to deal with structured data stored in tabular form such as data frame. However, a variety of data is non-tabular: different records may have different fields; for each field they may have different number of values. 

It is hard or no longer straightforward to store such data in data frame, but the `list` object in R is flexible enough to represent such records of diversity. rlist is a toolbox to deal with non-structured data stored in `list` objects, providing a collection of high-level functions which are pipeline friendly.

## Getting started

Suppose we have a list of developers, each of whom has a name, age, a few interests, a list of programming languages they use and the number of years they have been using them.


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

This type of data is non-relational since it does not well fit the shape of a data frame,  yet it can be easily stored in JSON or YAML format. In R, list objects are flexible enough to represent a wide range of non-relational datasets like this. This package provides a wide range of functions to query and manipulate this type of data.

The following examples use `str()` to show the structure of the output.

### Filtering

Filter those who like music and has been using R for more than 3 years.


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

### Selecting

Select their names and ages.


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

### Mapping

Map each of them to the number of interests.


```r
str( list.map(devs, length(interest)) )
```

```
List of 3
 $ p1: int 3
 $ p2: int 2
 $ p3: int 2
```

### More functions

In addition to these basic functions, rlist also supports various types of grouping, joining, searching, sorting, updating, etc. For the introduction to more functionality, please go through the [rlist Tutorial](http://renkun.me/rlist-tutorial).

## Lambda expression

In this package, almost all functions that work with expressions accept the following forms of lambda expressions:

- Implicit lambda expression: `expression`
- Univariate lambda expressions: 
    * `x ~ expression`
    * `f(x) ~ expression`
- Multivariate lambda expressions:
    * `f(x,i) ~ expression`
    * `f(x,i,name) ~ expression`

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

## Help overview

```r
help(package = rlist)
```

or view the documentation on [CRAN](http://cran.r-project.org/web/packages/rlist/rlist.pdf)

## License

This package is under [MIT License](http://opensource.org/licenses/MIT).
