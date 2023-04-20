#' Plotting the boots.lmer output
#' @export
#' @param x An object of class \code{boots.lmer.pkg_bootsout}.
#' @param y Ignored.
#' @param ... Further arguments passed to
#' \code{\link[graphics:plot.window]{plot.window}}.
plot.boots.lmer.pkg_bootsout<- function(x) {


  graphics::plot.new()
  graphics::plot.window(xlim = range(unlist(c(0,x))), ylim = c(-.5,1))
  graphics::axis(1)
  with(x, graphics::segments(0, 1, ab, col = "blue", lwd=3))
  with(x, graphics::segments(0, 0, a, col = "green", lwd=3))
  with(x, graphics::segments(a, .5, a + b, col = "red", lwd=3))
  graphics::legend("bottom", col = c("blue", "green", "red"),
                   legend = c("a+b", "a", "b"), bty = "n",
                   ncol = 3, lty = 1, lwd=3)
}
