#' @title Summary output of bootstrap Sampled Linear Mixed Effects Models
#' @rdname boots_lmer
#' @description This is a generic function returns to an object class \code{boots_output} from \code{\link[boots.lmer:boots_lmer]{boots_lmer}}.
#' @param x An object of class \code{boots_output}.
#' @param ... Further arguments passed to
#' @return Inference about the bootstrap sampled LMER fits.
#' @export
summary.boots_output<- function(object,...) {


  #Print the error message
  cat(paste(object$error.message,"\n \n"))

  #print the table
  cat("Bootstreap estimaes summary: \n")
  print(object$Estimates)


}
