#' Title
#'
#' @param val
#' @param pval
#' @param dist
#'
#' @return
#' @export
#'
#' @examples
statisticaltest<-function(val, pval, dist=NULL){
  if (pval<0){
    pval=0
  }else if (pval>1){
    pval=1
  }
  return (structure(list(value=val, pvalue=pval), distribution=dist, class=c("JD3TEST", "JD3")))
}

#' Title
#'
#' @param test
#' @param details
#'
#' @return
#' @export
#'
#' @examples
print.JD3TEST<-function(test, details=F){
  cat('Value: ', test$value, '\n')
  cat('P-Value: ', sprintf('%.4f', test$pvalue), '\n')
  if (details){
    dist=attr(test, "distribution")
    if (! is.null(dist)){
      cat('[', dist, ']\n')
    }
  }
}
