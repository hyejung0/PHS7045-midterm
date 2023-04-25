

#If I want to create 10,000 bootstrap analyses.
id <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "0"))
message("We are working in array #", id)

is_array <- ifelse(
  Sys.getenv("SLURM_ARRAY_TASK_ID", unset = "") == "",
  FALSE,
  TRUE
)


# Generating seeds
set.seed(123)

# Need to make sure all arrays have different seeds
if (is_array) {
  seeds <- as.integer(Sys.getenv("SLURM_ARRAY_TASK_COUNT", 1))
  seeds <- sample.int(.Machine$integer.max, seeds)
  set.seed(seeds[id])
}


# set.seed(1249)
subjects<-c("Sarah","John","Beth","Anna","Chris","Blake")
example.subject<-sample(subjects, size = 50,replace = TRUE)
table(example.subject)
example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
                        "X1"=rpois(n=length(example.subject), lambda = 3),
                        "X2"=rnorm(n=length(example.subject)),
                        "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
                        "subjects"=example.subject)
source("boots_samples.R")
source("boots_lmer.R")

output<-boots_samples(dat=example.dat,sub.id = "subjects",B=500) #create 4 bootstrap samples
lmer.out<-boots_lmer(y="Y",
                     X=c("X1","X2","X3"),
                     dat=example.dat,
                     boots.samples.list = output,
                     num_workers = strtoi(Sys.getenv("SLURM_TASKS_PER_NODE")) #this is the correct way to get number of cores available running the scritp using slurm..
                     )
# summary(lmer.out)
# plot(lmer.out)

#save
saveRDS(lmer.out, sprintf("lmer.out%05i.rds", id))


