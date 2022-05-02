
#' Generic Diagnostics Function
#'
#' @param x the object to extract diagnostics.
#' @param ... further arguments.
#'
#' @export
diagnostics<-function(x, ...){
  UseMethod("diagnostics")
}


#' @rdname diagnostics
#' @export
diagnostics.JD3<-function(x, ...){
  cat("No diagnostic\n")
}

