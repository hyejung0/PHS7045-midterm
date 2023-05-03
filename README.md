
# boots.lmer

<!-- badges: start -->

[![R-CMD-check](https://github.com/hyejung0/PHS7045-midterm/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/hyejung0/PHS7045-midterm/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of `boots.lmer` is to first, generate bootstrap samples for longitudinal data, and second, to fit linear mixed effects model on the bootstrap samples to make inference about the estimated effects of covariates.

## Installation

You can install the development version of boots.lmer from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("UofUEpiBio/PHS7045-midterm")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(boots.lmer)
#> Loading required package: data.table
## basic example code

data(example.dat) #read in the data
output<-boots_samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
lmer.out<-boots_lmer(y="Y", X=c("X1","X2","X3"), dat=example.dat, boots.samples.list = output) #fit LMER models

##Print the summary
summary(lmer.out)

##Boxplot of the estimated coefficients
plot(lmer.out)
```

## slurmR example

The slurmR example Rmd file can be found in the inst/slurmR folder, titled "slurmR_example.html"
