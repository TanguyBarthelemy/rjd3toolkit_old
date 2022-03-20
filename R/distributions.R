#' The Student Distribution
#'
#' Density, (cumulative) distribution function and random generation for Student distribution.
#'
#' @param df degrees of freedom.
#' @param n number of observations.
#' @param x vector of quantiles.
#'
#'
#' @examples
#' # T with 2 degrees of freedom.
#' z<-densityT(2, .01*seq(-100, 100, 1))
#' # T with 2 degrees of freedom. 100 randoms
#' z<-randomsT(2, 100)
#' @name normaldistribution
#' @rdname normaldistribution
#' @export
randomsT<-function(df, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsT", df, as.integer(n))
}

#' @rdname normaldistribution
#' @export
densityT<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityT", df, .jarray(as.numeric(x)))
}

#' @rdname normaldistribution
#' @export
cdfT<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfT", df, .jarray(as.numeric(x)))
}

#' The Chi-Squared Distribution
#'
#' Density, (cumulative) distribution function and random generation for chi-squared distribution.
#'
#' @inheritParams normaldistribution
#'
#' @name chi2distribution
#' @rdname chi2distribution
#' @export
randomsChi2<-function(df, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsChi2", df, as.integer(n))
}

#' @rdname chi2distribution
#' @export
densityChi2<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityChi2", df, .jarray(as.numeric(x)))
}

#' @rdname chi2distribution
#' @export
cdfChi2<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfChi2", df, .jarray(as.numeric(x)))
}

#' The Gamma Distribution
#'
#' Density, (cumulative) distribution function and random generation for Gamma distribution.
#'
#' @inheritParams normaldistribution
#' @param shape,scale shape and scale parameters.
#'
#' @name gammadistribution
#' @rdname gammadistribution
#' @export
randomsGamma<-function(shape, scale, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsGamma", shape, scale, as.integer(n))
}

#' @rdname gammadistribution
#' @export
densityGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname gammadistribution
#' @export
cdfGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfGamma", shape, scale, .jarray(as.numeric(x)))
}

#' The Inverse-Gamma Distribution
#'
#' Density, (cumulative) distribution function and random generation for inverse-gamma distribution.
#'
#' @inheritParams gammadistribution
#'
#' @name invgammadistribution
#' @rdname invgammadistribution
#' @export
randomsInverseGamma<-function(shape, scale, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsInverseGamma", shape, scale, as.integer(n))
}

#' @rdname invgammadistribution
#' @export
densityInverseGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityInverseGamma", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname invgammadistribution
#' @export
cdfInverseGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfInverseGamma", shape, scale, .jarray(as.numeric(x)))
}

#' The Inverse-Gaussian Distribution
#'
#' Density, (cumulative) distribution function and random generation for inverse-gaussian distribution.
#'
#' @inheritParams gammadistribution
#'
#' @name invgaussiandistribution
#' @rdname invgaussiandistribution
#' @export
randomsInverseGaussian<-function(shape, scale, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsInverseGaussian", shape, scale, as.integer(n))
}

#' @rdname invgaussiandistribution
#' @export
densityInverseGaussian<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityInverseGaussian", shape, scale, .jarray(as.numeric(x)))
}

#' @rdname invgaussiandistribution
#' @export
cdfInverseGaussian<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfInverseGaussian", shape, scale, .jarray(as.numeric(x)))
}
