#' @title Bootstrap Samples
#' @return A list of data.table of length B. Each data table contains `index` column and `no.repeat.id` column.
#' The `index` column shows the row number of the original dataset that were selected as bootstrap samples.
#'  Therefore, in order to get the bootstrap data, the user has to use the index column as row number in the original dataset.
#'  `no.repeat.id` column shows the 'sub.id' concatenated by trailing '__#' to represent subjects repeated multiple times.
#' @param dat data frame or data table of our interest
#' @param sub.id String vector, the column name of the subject ID's.
#' @param B Natural number. Number of bootstrap samples to create.
#' @examples
#' set.seed(1249)
#' subjects<-c("Sarah","John","Beth","Anna","Chris","Blake")
#' example.subject<-sample(subjects, size = 50,replace = TRUE)
#' example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
#'                         "X1"=rpois(n=length(example.subject), lambda = 3),
#'                         "X2"=rnorm(n=length(example.subject)),
#'                         "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
#'                         "subjects"=example.subject)
#' output<-boots_samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
#' head(output[[1]]) #This is the first outcome.
#' bootstrap_dat1<-example.dat[output[[1]]$index,] #this is the bootstrap sampled dataset from the first bootstrap run.
#' bootstrap_dat1$no.repeat.id<-output[[1]]$no.repeat.id #this code adds the correctly named subjects to the bootstrap data so that when the subject is used for LMER fit, it properly accounts for the subjects.
#' @export
boots_samples<-function(dat, sub.id,B){

  sub.id<-dat[,sub.id]
  #collect unique subject ID's
  unique.id<-unique(sub.id)

  #sample unique subject ID, B number of times
  this.index<-sapply(1:B,function(e){sample(unique.id, size = length(unique.id),replace = TRUE)})

  #we need to take a note of people who are sampled more than once to make sure that they are sampled from the data as if they are different individuals.
  no.repeat.id<-
    apply(this.index,2, function(x){
      temp<-table(x)
      for(i in seq_along(temp)){
        if(temp[i]>1){ #if the subject id appears more than once
          num.appearance<-temp[i] #number of times the subject appears in the sample
          x[which(x==names(temp)[i])]<-paste0(names(temp)[i],"__",1:num.appearance) #generate new subject ID by concatenating "_#" at the end of the id. Replace the old name with this
        }
      }
      x #return the list of sampled subject ID , now all sample should be unique subject ID.
    })


  output<-
    apply(no.repeat.id,2,function(x){
      temp<-
        lapply(x,function(x_i){ #for each subject
          index<-which(sub.id==stringr::word(x_i,1,sep = "\\__")) #index of original data where the subject id appears
          cbind(index, "no.repeat.id"=rep(x_i,length(index)))}) #get all observation for this particular subject.

      #####Return just index and new naming#####
      dat_return<-Reduce("rbind",temp)
      dat_return<-data.table::as.data.table(dat_return)
      dat_return[,index:=as.numeric(index)]
      dat_return


    }
    )

  output
}

