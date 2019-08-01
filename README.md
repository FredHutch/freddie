# freddie

<!-- badges: start -->
<!-- badges: end -->

The goal of freddie is to collect helper functions and tools that Fred Hutch R community folk create. 

## Installation

You can install the released version of freddie from github with:

``` r
devtools::install_github("FredHutch/freddie")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(freddie)
# Create a toy data frame that is 22 rows, and 4 columns
df <- data.frame(this = c(NA, seq(1, 5, 1), seq(1, 5, 1)), that = rep(0, 11),
                 thisotherThing = rep(NA), ohAndThis = rep(c("trash", "0"), 11))
                 
# Drop columns that are all NA OR those that contain only values in the array `trash`
cleanerdf <- dropWhen(df, unique = FALSE, trash = c("0", "trash"), requireAll = FALSE)
# cleanerdf should have 22 rows and only the `this` column

# Drop columns that are all NA or all equal to numeric, integer or character zeros
noZeros <- dropWhen(df, trash = c("0"), requireAll = TRUE)
# noZeros should have 22 rows and the columns `this` and `ohAndThis`

# Drop columns that are all NA or all equal to numeric, integer or character zeros and return only unique rows
noZerosUnique <- dropWhen(df, trash = c("0"), requireAll = TRUE, unique = TRUE)
# noZeros should have 12 rows and the columns `this` and `ohAndThis`


```

