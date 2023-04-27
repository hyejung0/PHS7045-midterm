#' @rdname boots_lmer
#' Summary print of the boots.lmer output
#' @export
#' @param x An object of class \code{boots_output}.
#' @param ... Further arguments passed to

summary.boots_output<- function(object,...) {


  #Print the error message
  cat(paste(object$error.message,"\n \n"))

  #print the table
  cat("Bootstreap estimaes summary: \n")
  print(object$Estimates)


}
