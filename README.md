# rlist

[![Build Status](https://travis-ci.org/renkun-ken/rlist.png?branch=master)](https://travis-ci.org/renkun-ken/rlist)

rlist is a set of tools for working with list objects. It has two main goals:

- Make it easier to work with list objects used to store data in more flexible structures than data frames.

- Quickly perform a wide range of functions and manipulations on non-relational data structured as list objects.

*Currently, this package is still on a very early stage. Its functions and implmentations may change overtime and cannot guarantee the backward compatibility. Please be cautious when you use it in production.*

## Installation

You can install the latest development version from GitHub with

```
devtools::install_github("rlist","renkun-ken")
```

## Getting started

A simple list will be generated to demonstrate the functions.

Suppose we have a list of developers, each of which has a name, age, some interests and a list of programming langauge they use and the number of years they have been using them.

```
developers <- 
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

### Subsetting

If we want to know the names of those whose age is below 25, we can write

```
library(rlist)
subset(developers,age < 25, name)
```
The output is
```
$p1
[1] "Ken"

$p3
[1] "Penny"
```

If we want to know the names and ages of those who like music and uses C++, we can write

```
subset(developers,"music" %in% interest & "cpp" %in% names(lang),list(name=name,age=age))
```
The output is 
```
$p2
$p2$name
[1] "James"

$p2$age
[1] 25
```

If we want to know, instead, how long they have been using R for those who use Python, we can write

```
subset(developers, "python" %in% names(lang), lang$r)
```
The output is
```
$p1
[1] 2

$p3
[1] 1
```

The principle is simple: evaluate the select expression for each list item that satifies the subset expression.

## Help overview

```
help(package = rlist)

## License

This package is under [MIT License](http://opensource.org/licenses/MIT).
