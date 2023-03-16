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
                        "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5)+rnorm(length(example.subject), mean=6),
                        "subjects"=example.subject) #generate example data


output<-boots.samples(dat=example.dat,sub.id ="subjects",B=4) #a list of 4 bootstrap samples 


## -----------------------------------------------------------------------------
lmer.out<-boots.lmer(y="Y", X=c("X1","X2","X3"), boots.samples.list = output)

lmer.out

## ----cache=TRUE---------------------------------------------------------------
set.seed(1204)
output<-boots.samples(dat=example.dat,sub.id ="subjects",B=5000)


## ----cache=TRUE---------------------------------------------------------------
lmer.out<-boots.lmer(y="Y", X=c("X1","X2","X3"), boots.samples.list = output)

## -----------------------------------------------------------------------------
coef.out<-
lapply(lmer.out,function(fit){
  if(class(fit)=="lm"){ #If linear model was fit, use below function
    coef(fit)
  }else{ #IF 
    lme4::fixef(fit)
  }
})

## -----------------------------------------------------------------------------

#extract coefficient estimate for X3
this.X3<-
lapply(coef.out,function(x){
  x["X3"]
})
est.X3<-unlist(this.X3)

#Here the number of NA's
sum(is.na(est.X3))

#index of all whose estimated effect of X3 is NA
which(is.na(est.X3)) 

## -----------------------------------------------------------------------------
lmer.out[[27]]

## -----------------------------------------------------------------------------
hist(est.X3)

