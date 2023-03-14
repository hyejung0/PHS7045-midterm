

#' Bootstrap Sampled Linear Mixed Effects Models (LMERs)
#' @return A list of length B, of linear mixed effects model fits.
#' @param boots.samples.list boots.samples output.
#' @param y character string. Name of column in each data.table of boots.samples.list, to be used as the outcome in LMER.
#' @param X a vector of character string.  Names of the of covariates to fit in LMER, as they appear on each dataset of boots.samples.list.
#' @param use.formula a vector of class formula. This is to be used in lmer fit. If not provided, then a basic additive fixed effects model with random effect of (1|no.repeat.sub.id) will be fit.
#' @examples
#' example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
#' example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
#'                        "X"=rpois(n=length(example.subject), lambda = 3),
#'                        "subjects"=example.subject)
#' output<-boots.samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
#' lmer.out<-boots.lmer(y="Y", X=c("X1","X2","X3"), boots.samples.list = output)
#' @export
boots.lmer<-function(y,X,boots.samples.list,use.formula=NULL){
library(data.table)

  lmer.fit<-function(y,X,dat,use.formula=NULL){#a function that fits one LMER


    if(!is.null(use.formula)){#If we have a specific formula we want to use for lmer, then we use it.
      dat[,lme4::lmer(use.formula,data=dat, REML=FALSE)]
    }else{ #if not, then we will fit a basic function with random effect with common intercept, etc.
      #Make sure we use backtick quotes in case the names have spaces and special characters
      X.names<-lapply(X, as.name)
      #If we have more than one covariate, concatenate them with +
      if(length(X)>1){
        X.names<-paste(X.names, collapse=" + ")
        X.formula<-as.formula(paste0("~ ", paste(X.names, collapse=" + ")))
      }
      model_formula1<-formula(paste0("`",rlang::quo_name(y),"`", "~", rlang::quo_name(X.names),"+(1|no.repeat.sub.id)"))
      dat[,lme4::lmer(model_formula1,data=dat, REML=FALSE)]
    }

  }


  #CRAN limits the number of cores available to packages to 2, for performance reasons. There was a thread in the mailing list, I believe, but I can't find it right now.
  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

  if (nzchar(chk) && chk == "TRUE") {
    # use 2 cores in CRAN/Travis/AppVeyor
    num_workers <- 2L
  } else {
    # use all cores in devtools::test()
    num_workers <- parallel::detectCores()
  }


  parallel::mclapply(boots.samples.list,function(boots.dat){
    lmer.fit(y=y,X=X,dat=boots.dat)}
    ,mc.cores = num_workers)

}
