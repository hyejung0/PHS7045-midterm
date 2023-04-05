#' @import lme4 rlang
NULL

#' Bootstrap Samples
#' @return A list of length B, of matrices with index and subjects
#' @param dat data frame or data table of our interest
#' @param sub.id String vector, the column name of the subject ID's.
#' @param B Natural number. Number of bootstrap samples to create.
#' @examples
#' example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
#' example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
#'                         "X1"=rpois(n=length(example.subject), lambda = 3),
#'                         "X2"=rnorm(n=length(example.subject)),
#'                         "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
#'                         "subjects"=example.subject)
#' output<-boots.samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
#' output[[1]] #This is the first sample. A data table with two columns, an index column and the new subject names
#' @export
boots.samples<-function(dat, sub.id,B){

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
      ##########################################

      ###### return data frame ######
      # temp.dat<-do.call(rbind, lapply(temp, data.frame))
      # complete.dat<-dat[temp.dat$index,]
      # complete.dat[,"no.repeat.sub.id"]<-temp.dat[,"no.repeat.id"]
      # return(complete.dat)
      ##########################################

      # #bind the index and no.repeat.id. as rows
      # temp.dat<-do.call(rbind, lapply(temp, data.table::data.table))
      # complete.dat<-data.table::as.data.table(dat[temp.dat$index,])
      # # complete.dat[,no.repeat.sub.id:=(temp.dat$no.repeat.id)] # add the new subject ID column, with no repetition
      # # complete.dat$no.repeat.sub.id<-temp.dat$no.repeat.id
      # complete.dat[,"no.repeat.sub.id"]<-temp.dat$no.repeat.id
      # return(complete.dat)
    }
    )

  output
}

