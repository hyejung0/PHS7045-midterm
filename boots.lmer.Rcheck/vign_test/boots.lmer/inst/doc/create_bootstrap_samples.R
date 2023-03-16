## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(boots.lmer)

## -----------------------------------------------------------------------------
set.seed(1020)
example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
                        "X1"=rpois(n=length(example.subject), lambda = 3),
                        "X2"=rnorm(n=length(example.subject)),
                        "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
                        "subjects"=example.subject)
knitr::kable(example.dat, caption = "Table 1. Example data set.")

## -----------------------------------------------------------------------------
output<-boots.samples(dat = example.dat, sub.id = "subjects",B=4)

output
# knitr::kable(output,caption = "Table 2. Four boostrap samples of example.dat data sets. The sampling unit is subject, hence the number of rows of each data set is expected to be different from one another.",row.names = FALSE)

