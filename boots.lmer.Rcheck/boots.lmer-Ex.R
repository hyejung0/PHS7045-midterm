pkgname <- "boots.lmer"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "boots.lmer-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('boots.lmer')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("boots.lmer")
### * boots.lmer

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: boots.lmer
### Title: Bootstrap Sampled Linear Mixed Effects Models (LMERs)
### Aliases: boots.lmer

### ** Examples

example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
                        "X1"=rpois(n=length(example.subject), lambda = 3),
                        "X2"=rnorm(n=length(example.subject)),
                        "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
                        "subjects"=example.subject)
output<-boots.samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
lmer.out<-boots.lmer(y="Y", X=c("X1","X2","X3"), boots.samples.list = output)



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("boots.lmer", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("boots.samples")
### * boots.samples

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: boots.samples
### Title: Bootstrap Samples
### Aliases: boots.samples

### ** Examples

example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
                        "X1"=rpois(n=length(example.subject), lambda = 3),
                        "X2"=rnorm(n=length(example.subject)),
                        "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
                        "subjects"=example.subject)
output<-boots.samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
output[[1]] #This is the first one!



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("boots.samples", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
