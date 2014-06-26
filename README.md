

# rlist

[![Build Status](https://travis-ci.org/renkun-ken/rlist.png?branch=master)](https://travis-ci.org/renkun-ken/rlist)

rlist is a set of tools for working with list objects. It has two main goals:

- Make it easier to work with list objects used to store data in more flexible structures than data frames.
- Perform a wide range of functions on non-relational data using list constructs.

*Currently, this package is still on a early stage. Its functions and implmentations may change overtime and cannot guarantee the backward compatibility. Please be cautious when you use it in production.*

## Installation

You can install the latest development version from GitHub with

```r
devtools::install_github("rlist","renkun-ken")
```

## Functions

A wide range of functions are provided to work with list objects.

Suppose we have a list of developers, each of which has a name, age, some interests and a list of programming langauge they use and the number of years they have been using them.


```r
library(rlist)
```

```

Attaching package: 'rlist'

The following objects are masked _by_ '.GlobalEnv':

    list.insert, list.iter, list.remove
```

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

### Filtering

If we want to know the names of those whose age is no less than 25, we can call `list.filter` to filter the list.


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

If we want to know the name of each person, we can call `list.map` to map each member by an expression.


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

If we want to know the programming language each person has been using for the longest time


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

### Grouping

If we want to build a list that contains sublists each represents an age group, we can call `list.group`.


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

If we want to sort the developers by the number of interests in descending order, then by the number of years they have been using R in descending order, we can call `list.sort`.


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

Pipeline operators may hugely improve the readibility of the code especially when a chain of commands are executed. [pipeR package](http://renkun.me/pipeR) is recommended to cowork with this package.

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

## Help overview

```r
help(package = rlist)
```

## License

This package is under [MIT License](http://opensource.org/licenses/MIT).
