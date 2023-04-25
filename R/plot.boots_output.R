#' @rdname boots_lmer
#' Plotting the boots_lmer output
#' @export
#' @param x An object of class \code{boots_output}.
#' @param y Ignored.
#' @param ... Further arguments passed to
#' \code{\link[graphics:plot.window]{plot.window}}.
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

  # graphics::plot.window(xlim = range(unlist(c(0,x))), ylim = c(-.5,1))
  # graphics::axis(1)
  # with(x, graphics::segments(0, 1, ab, col = "blue", lwd=3))
  # with(x, graphics::segments(0, 0, a, col = "green", lwd=3))
  # with(x, graphics::segments(a, .5, a + b, col = "red", lwd=3))
  # graphics::legend("bottom", col = c("blue", "green", "red"),
  #                  legend = c("a+b", "a", "b"), bty = "n",
  #                  ncol = 3, lty = 1, lwd=3)
}
