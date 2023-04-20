
library(boots.lmer)


example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
                        "X1"=rpois(n=length(example.subject), lambda = 3),
                        "X2"=rnorm(n=length(example.subject)),
                        "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
                        "subjects"=example.subject)
output<-boots.samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
lmer.out<-boots.lmer(y="Y", X=c("X1","X2","X3"), dat=example.dat, boots.samples.list = output)


set.seed(1204)
boots.sapmle1<-boots.samples(dat=example.dat,sub.id ="subjects",B=1000)
system.time(bootsamplesdata<-boots.lmer(y="Y", X=c("X1","X2","X3"), boots.samples.list = boots.sapmle1))
bootsamplesdata<-boots.lmer(y="Y", X=c("X1","X2","X3"), boots.samples.list = boots.sapmle1)
class(bootsamplesdata)
attributes(bootsamplesdata)
length(bootsamplesdata$est.betas)
length(bootsamplesdata$est.betas[[1]])
head(bootsamplesdata$est.betas[[1]])
sum(is.na(bootsamplesdata$est.betas[[3]]))
output.dat<-lapply(bootsamplesdata$est.betas,function(betas){
  Mean<-mean(betas,na.rm=TRUE)
  my.sd<-sd(betas, na.rm = TRUE)
  c(Mean,my.sd,Mean+c(-1,1)*qnorm(0.975)*my.sd)
})
output.dat<-do.call(rbind,output.dat)
colnames(output.dat)<-c("Mean","SD","2.5%","97.5%")

length(bootsamplesdata$all.fits)
bootsamplesdata$Error.Index
bootsamplesdata$all.fits[bootsamplesdata$Error.Index]
