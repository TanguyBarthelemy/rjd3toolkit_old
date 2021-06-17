#' @include jd2r.R
NULL

#' Aggregation of time series
#'
#' @param s
#' @param nfreq
#' @param conversion
#' @param complete
#'
#' @return
#' @export
#'
#' @examples
aggregate<-function(s, nfreq=1, conversion="Sum", complete=TRUE){
  if (is.null(s)){
    return (NULL)
  }
  jd_s<-ts_r2jd(s)
  jd_agg<-.jcall("demetra/timeseries/r/TsUtility", "Ldemetra/timeseries/TsData;", "aggregate", jd_s, as.integer(nfreq), conversion, complete)
  if (is.null(jd_agg)){
    return (NULL);
  }
  else{
    return (ts_jd2r(jd_agg))
  }
}

