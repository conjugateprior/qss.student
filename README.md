# qss.student

A package to let students grab a problem set from the QSS book.
Problem sets (without answers) are bundled as Rmarkdown files in the package.

## Usage

To unpack the problem set 'gay marriage' the student types
```
get_pset("gay-marriage")
```
This extracts a folder containing 
```
gay-marriage.Rmd
data/gay.csv
```
The student then sets their working directory to the new problem set
```
setwd("gay-marriage")
```
and opens the Rmd file.  The first code chunk in the Rmd file can then be 
(in base R)
```
gay <- read.csv("data/gay.csv")
```
A useful convention is that the student answer each question by adding a new
heading, e.g.
```
## Answer 2
```
and adding text and code chunks as an answer until the next question heading 
is reached.

## Troubleshooting

If the student mistypes the problem set name the package will attempt to suggest
which package they might have meant and offer to load that.

The new (2018) default for `get_pset` is *not* to set the working directory 
when a problem set is unpacked as this was confusing students with little 
understanding of file path.

A complete list of problem set names can be gotten by typing
```
list_psets()
```

Will Lowe, January 2018.
