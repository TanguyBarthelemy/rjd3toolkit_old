#' @include utils.R
NULL

p2r_differencing<-function(p){
  if (is.null(p)){
    return (NULL)
  } else{
    del<-sapply(p$differences, function(z){(return (c(z$lag,z$order)))})
    del<-`rownames<-`(del, c("lag", "order"))
    return (list(ddata=p$stationary_series,
                 mean=p$mean_correction,
            differences=del))
  }
}

#' Title
#'
#' @param data
#' @param period
#'
#' @return
#' @export
#'
#' @examples
do.stationary<-function(data, period){
  jst<-.jcall("demetra/modelling/r/Differencing", "Ldemetra/modelling/StationaryTransformation;", "doStationary",
         as.numeric(data), as.integer(period))
  q<-.jcall("demetra/modelling/r/Differencing", "[B", "toBuffer", jst)
  p<-RProtoBuf::read(regarima.StationaryTransformation, q)
  return (p2r_differencing(p))
}

#' Title
#'
#' @param data
#' @param period
#' @param mad
#' @param centile
#' @param k
#'
#' @return
#' @export
#'
#' @examples
differencing.fast<-function(data, period, mad=T, centile=90, k=1.2){
  jst<-.jcall("demetra/modelling/r/Differencing", "Ldemetra/modelling/StationaryTransformation;", "fastDifferencing",
              as.numeric(data), as.integer(period), as.logical(mad), centile, k)
  q<-.jcall("demetra/modelling/r/Differencing", "[B", "toBuffer", jst)
  p<-RProtoBuf::read(regarima.StationaryTransformation, q)
  return (p2r_differencing(p))

}

#' Title
#'
#' @param data
#' @param lags
#' @param mean
#'
#' @return
#' @export
#'
#' @examples
differences<-function(data, lags=1, mean=T){
  return (.jcall("demetra/modelling/r/Differencing", "[D", "differences",
                 as.numeric(data), .jarray(as.integer(lags)), mean))
}

