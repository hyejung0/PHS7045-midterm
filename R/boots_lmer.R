#' @importFrom stats as.formula formula sd qnorm

#' @title Bootstrap Sampled Linear Mixed Effects Models (LMERs)
#' @description The function returns a fitted LMER model using bootstrap sample data, and returns inference about the fits.
#' @return A list of length 4.
#'
#' `Error.Index` is a vector of logicals. The corresponding `boots.samples.list` input where `Error.Index ==TRUE` had an error fitting the lme4::lmer function.
#'
#' `est.betas` is a list of length same as the `X` input. Each list shows estimated coefficient of the corresponding covariates using bootstrap samples.
#'
#'  `Estimates` is a data frame of the bootstrap output. It contains "Mean", "SD", "2.5%", and "97.5%" columns.
#'  The "Mean" column is calculated by using \code{\link[base:mean]{mean}} function on the `est.betas`.
#'  The "SD" column is calculated by using \code{\link[stats:sd]{sd}} function on the `est.betas`.
#'  The "2.5%" and "97.5%" confidence intervals are calculated using `Mean + c(-1, 1)*qnorm(0.975)*SD`.
#'
#' @param y character string. Name of column in each data.table of boots.samples.list, to be used as the outcome in LMER.
#' @param X a vector of character string.  Names of the of covariates to fit in LMER, as they appear on each dataset of boots.samples.list.
#' @param dat a data frame or data table, which contains y, X. Also should contain the subject column which was used to generate boots.samples.list.
#' @param boots.samples.list \code{\link[boots.lmer:boots_samples]{boots_samples}} output.
#' @param use.formula a vector of class formula. This is to be used in lmer fit. If not provided, then a basic additive fixed effects model with random effect of (1|no.repeat.sub.id) will be fit.
#' @param num_workers integer. A number of cores to use for parallel computing.
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
#' lmer.out<-boots_lmer(y="Y", X=c("X1","X2","X3"), dat=example.dat, boots.samples.list = output)
#'
#' ##Print the summary
#' summary(lmer.out)
#'
#' ##Boxplot of the estimated coefficients
#' plot(lmer.out)
#' @import data.table
#' @export
boots_lmer<-function(y,X,dat,boots.samples.list,use.formula=NULL, num_workers=2L){




  lmer.fit<-function(y,X,dat,one.boot.sample,use.formula=NULL){#a function that fits one LMER


    if(length(unique(one.boot.sample$no.repeat.id))==nrow(one.boot.sample)){ #If no subjects are repeating, no need to fit LMER, use LM.
      return(simpleError(message = "Not a repeated sample. Did not fit LMER"))
    }

    #generate bootstrap data
    dat<-dat[one.boot.sample$index,]
    dat$no.repeat.id<-one.boot.sample$no.repeat.id

    #If longitudinal data, then fit LMER model.
    if(is.null(use.formula)){#if we are not provided with specific formula, then create our own
      X.names<-lapply(X, as.name)
      if(length(X)>1){ #generate X1+X2+...
        X.formula<-as.formula(paste0("~ ", paste(X.names, collapse=" + ")))
      }else{#If we only have one covariate, make it a formula as well
        X.formula<-as.formula(paste("~",X.names))
      }
      use.formula<-formula(paste0("`",rlang::quo_name(y),"`",rlang::quo_name(X.formula),"+(1|no.repeat.id)"))
      lme4::lmer(use.formula,data=dat, REML=FALSE)
    }else{ #If we are provided with formula, then fit the LMER using the formula
      lme4::lmer(use.formula,data=dat, REML=FALSE)
    }

  }

  #If unix, use forking
  if(.Platform$OS.type=="unix"){
    all.fits<-parallel::mclapply(boots.samples.list,function(boots.dat){
      #Convert to data table to save time
      res <- tryCatch(lmer.fit(y=y,X=X,dat=dat,one.boot.sample=boots.dat), error = function(e) e)
      return(res)
      }
      ,mc.cores = num_workers)
  }else{ #If windows, use PSOCK.

    cl <- parallel::makePSOCKcluster(num_workers) #number of clusters
    on.exit(parallel::stopCluster(cl)) #Close parapllel once done running
    parallel::clusterExport(cl, "lmer.fit")

    all.fits<-parallel::parLapply(cl, boots.samples.list,function(boots.dat){
      #Convert to data table to save time
      res <- tryCatch(lmer.fit(y=y,X=X,dat=boots.dat), error = function(e) e)

      if (inherits(res, "error")) {
        # ""
        return(res)
      } else {
        return(res)
      }

      })
  }


  #count the number of errors and print it off (if there were errors)
  if(.Platform$OS.type=="unix"){ #If linux
    MyError<-parallel::mclapply(all.fits,function(fit){
      if(inherits(fit,"simpleError")){
        TRUE
      }else{
        FALSE
      }
    }
    ,mc.cores = num_workers)

  }else{ #If windows
    cl <- parallel::makePSOCKcluster(num_workers) #number of clusters
    on.exit(parallel::stopCluster(cl)) #Close parallel once done running
    parallel::clusterExport(cl, "all.fits")

    MyError<-parallel::parLapply(cl, all.fits,function(fit){
      if(inherits(fit,"simpleError")){
        TRUE
      }else{
        FALSE
      }

    })
  }

  MyError<-do.call("c",MyError)
  Error.Index<-NA
  if(sum(MyError)>0){ #If there were errors, print that there were errors
    Error.Index<-which(MyError) #Save the index of bootstrap samples that resulted in error. Return it
  }

  #save number of error and print out as a message in summary
  error.message<-paste0("There were ",sum(MyError)," error(s) from LMER fitting, and they were omitted from the analyses.")



  #If all bootstrap samples were errors, then exit the function
  if(sum(MyError)==length(all.fits)){
    stop("All bootstrap samples had errors while fitting LMER function. Therefore, no inference could be made.")
  }

  #If there were at least one LMER fit that was successful, then extract the beta coefficients and make inference about it.
  all.fits<-all.fits[!MyError]

  #Extract all coefficients
  coef.out<-lapply(all.fits,function(fit){lme4::fixef(fit)})

  #extract coefficient estimate for covariates in X
  est.betas<-lapply(X,function(cov){ #For each covariate X_i,
    this.cov<-lapply(coef.out, function(beta){ #Extracted the estimated betas
      beta[cov]
    })
    this.cov<-unlist(this.cov)
  })
  names(est.betas)<-X

  #Generate table to print. Mean, bootstrap SD, 95% CI
  output.dat<-lapply(est.betas,function(betas){
    Mean<-mean(betas,na.rm=TRUE)
    my.sd<-sd(betas, na.rm = TRUE)
    c(Mean,my.sd,Mean+c(-1,1)*qnorm(0.975)*my.sd)
  })
  output.dat<-do.call(rbind,output.dat)
  colnames(output.dat)<-c("Mean","SD","2.5%","97.5%")
  rownames(output.dat)<-X


  #return a list with class boots_output
  structure(list("Error.Index"=Error.Index, #index of bootstrap samples that resulted in error. NA if there were no errors.
                 #"all.fits"=all.fits, #Printing the fits make the file too big.
                 "est.betas"=est.betas,
                 "Estimates"=output.dat,
                 "error.message"=error.message)
            , class = "boots_output")



}
