nsims  <- 100
n      <- 100 #500 that goes in the boots_samples funcntion
ncores <- 4L
njobs<-2L #number of nodes


#########

library(boots.lmer)

# data(example.dat)

simfun <- function(i){
  data(example.dat)
  output<-boots_samples(dat=example.dat,sub.id = "subjects",B=n) #create n bootstrap samples
  lmer.out<-boots_lmer(y="Y",
                       X=c("X1","X2","X3"),
                       dat=example.dat,
                       boots.samples.list = output,
                       num_workers = ncores) #this is the correct way to get number of cores available running the scritp using slurm..
  lmer.out$est.betas #Just return estimated betas
}
########
# Setting up slurmR
library(slurmR) # This also loads the parallel package


ans <- Slurm_lapply(
  1:nsims, simfun,
  # n.       = n,
  njobs    = njobs,
  mc.cores = 1L,
  plan     = "collect",
  tmp_path = "/scratch/general/nfs1/u1317537", # This is where all temp files will be exported
  sbatch_opt=list(account="notchpeak-shared-short",
                  partition="notchpeak-shared-short"),
  export = c("ncores","n")
)


saveRDS(ans, "example_slurmR2.rds")


# temp --------------------------------------------------------------------


# # Making the cluster, and exporting the variables
# cl <- makeSlurmCluster(ncores, account="greene", partition="lonepeak")
# # Approximation
# # clusterExport(cl, c("ncores","n", "boots_samples","boots_lmer","simfun","example.dat"))
# clusterExport(cl, c("ncores","n", "simfun","example.dat"))
#
# ans <- parSapply(cl, 1:nsims, simfun)
# # Closing connection
# stopCluster(cl)
