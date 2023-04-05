# This file is part of the standard setup for testthat.
# It is recommended that you do not modify it.
#
# Where should you do additional test configuration?
# Learn more about the roles of various files in:
# * https://r-pkgs.org/tests.html
# * https://testthat.r-lib.org/reference/test_package.html#special-files

library(testthat)
library(boots.lmer)

# test_check("boots.lmer")

example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
                        "X1"=rpois(n=length(example.subject), lambda = 3),
                        "X2"=rnorm(n=length(example.subject)),
                        "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
                        "subjects"=example.subject)
output<-boots.samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
lmer.out<-boots.lmer(y="Y", X=c("X1","X2","X3"), dat=example.dat, boots.samples.list = output)


set.seed(1204)
boots.sapmle1<-boots.samples(dat=example.dat,sub.id ="subjects",B=5000)
system.time(bootsamplesdata<-boots.lmer(y="Y", X=c("X1","X2","X3"), boots.samples.list = boots.sapmle1))
# Warning message:
#   In parallel::mclapply(boots.samples.list, function(boots.dat) { :
#       all scheduled cores encountered errors in user code
# usethis::use_data(bootsamplesdata)
