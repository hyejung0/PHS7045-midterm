

#' Bootstrap Sampled Linear Mixed Effects Models (LMERs)
#' @return A list of length B, of linear mixed effects model fits.
#' @param y character string. Name of column in each data.table of boots.samples.list, to be used as the outcome in LMER.
#' @param X a vector of character string.  Names of the of covariates to fit in LMER, as they appear on each dataset of boots.samples.list.
#' @param dat a data frame or data table, which contains y, X. Also should contain the subject column which was used to generate boots.samples.list.
#' @param boots.samples.list boots.samples output.
#' @param use.formula a vector of class formula. This is to be used in lmer fit. If not provided, then a basic additive fixed effects model with random effect of (1|no.repeat.sub.id) will be fit.
#' @param num_workers integer. A number of cores to use for parallel computing.
#' @examples
#' example.subject<-c("Sarah","John","Beth","Anna","Sarah","Sarah","Chris","Blake","John","Anna")
#' example.dat<-data.frame("Y"=rnorm(n=length(example.subject)),
#'                         "X1"=rpois(n=length(example.subject), lambda = 3),
#'                         "X2"=rnorm(n=length(example.subject)),
#'                         "X3"=rbeta(n=length(example.subject), shape1 = 3, shape2 = 0.5),
#'                         "subjects"=example.subject)
#' output<-boots.samples(dat=example.dat,sub.id = "subjects",B=4) #create 4 bootstrap samples
#' lmer.out<-boots.lmer(y="Y", X=c("X1","X2","X3"), dat=example.dat, boots.samples.list = output)
#' @export
boots.lmer<-function(y,X,dat,boots.samples.list,use.formula=NULL, num_workers=NULL){



  lmer.fit<-function(y,X,dat,use.formula=NULL){#a function that fits one LMER


    if(is.null(use.formula)){#if we are not provided with specific formula, then create our own
      X.names<-lapply(X, as.name)
      if(length(X)>1){
        X.names<-paste(X.names, collapse=" + ")
        X.formula<-as.formula(paste0("~ ", paste(X.names, collapse=" + ")))
      }

      if(length(unique(dat$no.repeat.sub.id))==nrow(dat)){ #If no subjects are repeating, no need to fit LMER, use LM.
        use.formula<-formula(paste0("`",rlang::quo_name(y),"`",rlang::quo_name(X.formula)))
        lm(use.formula,data=dat)
      }else{
        use.formula<-formula(paste0("`",rlang::quo_name(y),"`",rlang::quo_name(X.formula),"+(1|no.repeat.sub.id)"))
        lme4::lmer(use.formula,data=dat, REML=FALSE)
      }
    }else{ #If we are provided with formula....
      lme4::lmer(use.formula,data=dat, REML=FALSE)
    }

  }

  if(is.null(num_workers)){
    #CRAN limits the number of cores available to packages to 2, for performance reasons. There was a thread in the mailing list, I believe, but I can't find it right now.
    chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")

    if (nzchar(chk) && chk == "TRUE") {
      # use 2 cores in CRAN/Travis/AppVeyor
      num_workers <- 2L
    } else {
      # use all cores in devtools::test()
      num_workers <- parallel::detectCores()
    }

  }



  parallel::mclapply(boots.samples.list,function(boots.dat){
    #Convert to data table to save time
    lmer.fit(y=y,X=X,dat=boots.dat)}
    ,mc.cores = num_workers)

}
