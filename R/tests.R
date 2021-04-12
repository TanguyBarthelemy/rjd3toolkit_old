#' @include utils.R
NULL

#' Title
#'
#' @param data
#' @param k
#' @param lag
#' @param nhp
#' @param sign
#' @param mean
#'
#' @return
#' @export
#'
#' @examples
ljungbox<-function(data, k, lag=1, nhp=0, sign=0, mean=T){
  jtest<-.jcall("demetra/stats/r/Tests", "Ldemetra/stats/StatisticalTest;", "ljungBox",
                as.numeric(data), as.integer(k), as.integer(lag), as.integer(nhp), as.integer(sign), as.logical(mean))
  return (jd2r_test(jtest))
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
bowmanshenton<-function(data){
  jtest<-.jcall("demetra/stats/r/Tests", "Ldemetra/stats/StatisticalTest;", "bowmanShenton",as.numeric(data))
  return (jd2r_test(jtest))
}

#' Title
#'
#' @param data
#'
#' @return
#' @export
#'
#' @examples
doornikhansen<-function(data){
  jtest<-.jcall("demetra/stats/r/Tests", "Ldemetra/stats/StatisticalTest;", "doornikHansen",as.numeric(data))
  return (jd2r_test(jtest))
}

#' Title
#'
#' @param data
#' @param k
#' @param sample
#'
#' @return
#' @export
#'
#' @examples
jarquebera<-function(data, k=0, sample=T){
  jtest<-.jcall("demetra/stats/r/Tests", "Ldemetra/stats/StatisticalTest;", "jarqueBera",
                as.numeric(data), as.integer(k), as.logical(sample))
  return (jd2r_test(jtest))
}

#' Title
#'
#' @param data
#' @param nar
#' @param n
#'
#' @return
#' @export
#'
#' @examples
autocorrelations.inverse<-function(data, nar=30, n=15){
  return (.jcall("demetra/stats/r/Tests", "[D", "inverseAutocorrelations",
                as.numeric(data), as.integer(nar), as.integer(n)))
}
