nsims  <-2
n      <- 500 #500 that goes in the boots_samples funcntion
ncores <- 4L


#########
load(file.path(system.file("big-data",package="boots.lmer"), "example.dat.rda"))
library(boot.lmer)

simfun <- function(i){
  output<-boots_samples(dat=example.dat,sub.id = "subjects",B=n) #create n bootstrap samples
  lmer.out<-boots_lmer(y="Y",
                       X=c("X1","X2","X3"),
                       dat=example.dat,
                       boots.samples.list = output,
                       num_workers = ncores) #this is the correct way to get number of cores available running the scritp using slurm..
}
########
# Setting up slurmR
library(slurmR) # This also loads the parallel package
# Making the cluster, and exporting the variables
cl <- makeSlurmCluster(ncores, account="notchpeak-shared-short", partition="notchpeak-shared-short")
# Approximation
# clusterExport(cl, c("ncores","n", "boots_samples","boots_lmer","simfun","example.dat"))
clusterExport(cl, c("ncores","n", "simfun","example.dat"))

ans <- parSapply(cl, 1:nsims, simfun)
# Closing connection
stopCluster(cl)
saveRDS(ans, "example_slurmR.rds")
