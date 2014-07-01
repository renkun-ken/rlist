

# rlist

[![Build Status](https://travis-ci.org/renkun-ken/rlist.png?branch=master)](https://travis-ci.org/renkun-ken/rlist)

rlist is a toolset for working with list objects. It has two main goals:

- Make it easier to work with list objects used to store data in more flexible structures than data frames.
- Perform a wide range of functions on non-relational data using list constructs.

[Release notes](https://github.com/renkun-ken/rlist/releases)

*Notice: Currently, this package may not be stable enough for production purpose. Its functions and implmentations may change overtime and cannot guarantee the backward compatibility at the moment. Please be cautious when you use it in production.*

## Installation

You can install the lastest released version from CRAN with

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

In the following example, we query the name of each developer who likes music and uses R, and put the results in a data frame.


```r
library(rlist)
library(pipeR)
devs %>>% 
  list.filter("music" %in% interest & "r" %in% names(lang)) %>>%
  list.select(name,age) %>>%
  list.rbind %>>%
  data.frame
```

```
    name age
p1   Ken  24
p2 James  25
```

The example above uses [`pipeR` package](http://renkun.me/pipeR/) for pipeline operator `%>>%` that chains commands in a fluent style.

The table below lists the functions currently supported.

- `list.map`: Map each list member by an expression
- `list.filter`: Filter the list by an logical expression
- `list.select`: Select by expressions for each list item
- `list.sort`: Sort the list by an expression
- `list.group`: Group the list members by an expression
- `list.class`: Classify the list members by a vector expression
- `list.join`: Join two lists by an expression
- `list.update`: Update a list with partial specification
- `list.parse`: Parse `yaml`, `json` format text, or `data.frame` and `matrix` to a list with identical structure.
- `list.load`, `list.save`: Load or save a list stored in `yaml`, `json`, `xml` or `RData` file.
- ...

## Lambda expression

In this package, all functions that work with expressions support the following forms of lambda expressions:

- `x ~ g(x)`
- `x -> g(x)`
- `f(x) -> g(x)`
- `f(x,i) -> g(x,i)`
- `f(x,i,name) -> g(x,i,name)`

where `x` refers to the list member itself, `i` denotes the index, `name` denotes the name. If the symbols are not explicitly declared, `.`, `.i` and `.name` will by default be used to represent them, respectively.

```r
nums <- list(a=c(1,2,3),b=c(2,3,4),c=c(3,4,5))
nums %>>%
  list.map(data.frame(min=min(.),max=max(.))) %>>%
  list.rbind
nums %>>% list.mapv(x ~ sum(x))
nums %>>% list.filter(x -> mean(x)>=3)
nums %>>% list.mapv(f(x,i) -> sum(x,i))
```

## Vignettes

The package also provides detailed vignettes for most functions. Read them in R with

```r
vignette("introduction", package = "rlist")
```

## Help overview

```r
help(package = rlist)
```

## License

This package is under [MIT License](http://opensource.org/licenses/MIT).
