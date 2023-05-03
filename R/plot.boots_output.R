#' @rdname boots_lmer
#' @title Plotting output of bootstrap Sampled Linear Mixed Effects Models
#' @description This is a generic function returns to an object class \code{boots_output} from \code{\link[boots.lmer:boots_lmer]{boots_lmer}}. It returns a boxplot of the estimated effects of covarites from bootstrap samples.
#' @param x An object of class \code{boots_output}.
#' @param y Ignored.
#' @param ... Further arguments passed to
#' \code{\link[graphics:plot.window]{plot.window}}.
#' @export
plot.boots_output<- function(x,y,...) {

  #
  #calculate range of estimated betas for x-axis.
  bootstrap_estimates<-do.call("c",x$est.betas)
  plot.dat<-
    data.table::data.table(
      bootstrap_estimates,
      "betas"=rep(names(x$est.betas),each=length(x$est.betas[[1]]))
      )

  graphics::plot.new()
  graphics::boxplot(bootstrap_estimates~betas,
                    data=plot.dat,
                    horizontal = T,
                    main="Bootstrap density of the \n estimated effects of the covariate(s)",
                    xlab="Bootstrap estimates",
                    ylab="Eestimated effects")

}
