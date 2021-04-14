
#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
diagnostics<-function(x, ...){
  UseMethod("diagnostics")
}


#' Title
#'
#' @param x
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
diagnostics.JD3<-function(x, ...){
  cat("No diagnostic\n")
}

