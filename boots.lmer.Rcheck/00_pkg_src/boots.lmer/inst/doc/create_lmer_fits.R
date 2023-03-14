## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)


## ----setup--------------------------------------------------------------------
library(boots.lmer)

## ---- eval=FALSE--------------------------------------------------------------
#  set.seed(1020)
#  example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
#  example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
#                          "X1"=rpois(n=length(example.subject), lambda = 3),
#                          "X2"=rnorm(n=length(example.subject)),
#                          "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
#                          "subjects"=example.subject) #generate example data
#  
#  
#  output<-boots.samples(dat=example.dat,sub.id ="subjects",B=4) #a list of 4 bootstrap samples
#  

