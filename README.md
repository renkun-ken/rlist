

# rlist

[![Build Status](https://travis-ci.org/renkun-ken/rlist.png?branch=master)](https://travis-ci.org/renkun-ken/rlist)

rlist is a set of tools for working with list objects. It has two main goals:

- Make it easier to work with list objects used to store data in more flexible structures than data frames.
- Perform a wide range of functions on non-relational data using list constructs.

[Release notes](https://github.com/renkun-ken/rlist/releases)

*Notice: Currently, this package may not be stable enough for production purpose. Its functions and implmentations may change overtime and cannot guarantee the backward compatibility at the moment. Please be cautious when you use it in production.*

## Installation

Install from CRAN with

```r
install.packages("rlist")
```

or install the latest development version from GitHub with

```r
devtools::install_github("rlist","renkun-ken")
```

## Functions

The package provides a wide range of functions to work with list objects.

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

This type of data can be easily stored in JSON or YAML format.

### Filtering

Filter members whose age is no less than 25 by calling `list.filter`.


```r
str(list.filter(devs,age >= 25))
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

### Mapping

Get the name of each person by calling `list.map` that maps each member by an expression.


```r
list.map(devs, name)
```

```
$p1
[1] "Ken"

$p2
[1] "James"

$p3
[1] "Penny"
```

Get the programming language each person has been using for the longest time by calling `list.map`.


```r
list.map(devs, sort(unlist(lang),decreasing = T)[1])
```

```
$p1
csharp 
     4 

$p2
cpp 
  5 

$p3
cpp 
  4 
```

### Selecting

Select the name and age of each member by calling `list.select`.


```r
str(list.select(devs,name,age))
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

Select the name and evaluate the range of the number of years using programming languages.


```r
str(list.select(devs,name,range=range(unlist(lang))))
```

```
List of 3
 $ p1:List of 2
  ..$ name : chr "Ken"
  ..$ range: num [1:2] 2 4
 $ p2:List of 2
  ..$ name : chr "James"
  ..$ range: num [1:2] 2 5
 $ p3:List of 2
  ..$ name : chr "Penny"
  ..$ range: num [1:2] 1 4
```

### Grouping

Build a list that contains sublists each represents an age group by calling `list.group`.


```r
str(list.group(devs,age))
```

```
List of 2
 $ 24:List of 2
  ..$ p1:List of 4
  .. ..$ name    : chr "Ken"
  .. ..$ age     : num 24
  .. ..$ interest: chr [1:3] "reading" "music" "movies"
  .. ..$ lang    :List of 3
  .. .. ..$ r     : num 2
  .. .. ..$ csharp: num 4
  .. .. ..$ python: num 3
  ..$ p3:List of 4
  .. ..$ name    : chr "Penny"
  .. ..$ age     : num 24
  .. ..$ interest: chr [1:2] "movies" "reading"
  .. ..$ lang    :List of 3
  .. .. ..$ r     : num 1
  .. .. ..$ cpp   : num 4
  .. .. ..$ python: num 2
 $ 25:List of 1
  ..$ p2:List of 4
  .. ..$ name    : chr "James"
  .. ..$ age     : num 25
  .. ..$ interest: chr [1:2] "sports" "music"
  .. ..$ lang    :List of 3
  .. .. ..$ r   : num 3
  .. .. ..$ java: num 2
  .. .. ..$ cpp : num 5
```

### Sorting

Sort the developers by the number of interests in descending order, then by the number of years they have been using R in descending order by calling `list.sort`.


```r
sorted <- list.sort(devs,desc(length(interest)),desc(lang$r))
list.map(sorted,name)
```

```
$p1
[1] "Ken"

$p2
[1] "James"

$p3
[1] "Penny"
```

### Updating

Use `list.update` to update the list by removing `age` and `lang` columns and introducing the number of languages each member uses as `nlang`.


```r
str(list.update(devs,age=NULL,lang=NULL,nlang=length(lang)))
```

```
List of 3
 $ p1:List of 3
  ..$ name    : chr "Ken"
  ..$ interest: chr [1:3] "reading" "music" "movies"
  ..$ nlang   : int 3
 $ p2:List of 3
  ..$ name    : chr "James"
  ..$ interest: chr [1:2] "sports" "music"
  ..$ nlang   : int 3
 $ p3:List of 3
  ..$ name    : chr "Penny"
  ..$ interest: chr [1:2] "movies" "reading"
  ..$ nlang   : int 3
```

### More functions

Much more functions are provided than the examples show. Please read the documentation of the package.

### Working with pipeline

Pipeline operators may hugely improve the readability of the code especially when a chain of commands are executed. [pipeR package](http://renkun.me/pipeR) is recommended to co-work with this package.

If we want to know the developers whose age is no more than 24 and create a data frame where they are sorted by the number of years using R in descending order and each row tells us the name, years of using R, and the longest time using a language they know.


```r
library(pipeR)
devs %>>%
  list.filter(age <= 24) %>>%
  list.sort(desc(lang$r)) %>>%
  list.map(data.frame(name=name,r=lang$r,
    longest=max(unlist(lang)))) %>>%
  list.rbind
```

```
    name r longest
p1   Ken 2       4
p3 Penny 1       4
```

### Lambda expression

In this package, all functions that work with expressions support the following forms of lambda expressions, for example,

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
```

```
  min max
a   1   3
b   2   4
c   3   5
```

```r
nums %>>% list.mapv(x ~ sum(x))
```

```
 a  b  c 
 6  9 12 
```

```r
nums %>>% list.filter(x -> mean(x)>=3)
```

```
$b
[1] 2 3 4

$c
[1] 3 4 5
```

```r
nums %>>% list.mapv(f(x,i) -> sum(x,i))
```

```
 a  b  c 
 7 11 15 
```

## Help overview

```r
help(package = rlist)
```

## License

This package is under [MIT License](http://opensource.org/licenses/MIT).
