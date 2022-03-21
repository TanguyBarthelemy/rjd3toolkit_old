#' @include jd2r.R
NULL

#' Aggregation of time series
#'
#' Makes a frequency change of this series.
#'
#' @param s the input time series.
#' @param nfreq the new frequency. Must be la divisor of the frequency of \code{s}.
#' @param conversion Aggregation mode: sum (\code{"Sum"}),
#' average (\code{"Average"}), first observation (\code{"First"}), last observation
#' (\code{"Last"}), minimum (\code{"Min"}), maximum (\code{"Max"}).
#' @param complete  boolean indicatif if the observation for a given period in the
#' new series is set not computed missing if some data in the original series are missing.
#'
#' @return A new time series of frequency \code{nfreq}.
#' @export
#'
#' @examples
#' s = ABS$X0.2.09.10.M
#' # Annual sum
#' aggregate(s, nfreq = 1, conversion = "Sum")
#' # Quarterly mean
#' aggregate(s, nfreq = 4, conversion = "Average")
aggregate<-function(s, nfreq=1,
                    conversion=c("Sum", "Average", "First", "Last", "Min", "Max"),
                    complete=TRUE){
  conversion <- match.arg(conversion)
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

