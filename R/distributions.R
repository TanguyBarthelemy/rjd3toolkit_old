#' Title
#'
#' @param df
#' @param n
#'
#' @return
#' @export
#'
#' @examples
randomsT<-function(df, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsT", df, as.integer(n))
}

#' Title
#'
#' @param df
#' @param x
#'
#' @return
#' @export
#'
#' @examples
densityT<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityT", df, .jarray(x))
}

#' Title
#'
#' @param df
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cdfT<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfT", df, .jarray(x))
}

#' Title
#'
#' @param df
#' @param n
#'
#' @return
#' @export
#'
#' @examples
randomsChi2<-function(df, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsChi2", df, as.integer(n))
}

#' Title
#'
#' @param df
#' @param x
#'
#' @return
#' @export
#'
#' @examples
densityChi2<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityChi2", df, x)
}

#' Title
#'
#' @param df
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cdfChi2<-function(df, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfChi2", df, x)
}

#' Title
#'
#' @param shape
#' @param scale
#' @param n
#'
#' @return
#' @export
#'
#' @examples
randomsGamma<-function(shape, scale, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsGamma", shape, scale, as.integer(n))
}

#' Title
#'
#' @param shape
#' @param scale
#' @param x
#'
#' @return
#' @export
#'
#' @examples
densityGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityGamma", shape, scale, x)
}

#' Title
#'
#' @param shape
#' @param scale
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cdfGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfGamma", shape, scale, x)
}

#' Title
#'
#' @param shape
#' @param scale
#' @param n
#'
#' @return
#' @export
#'
#' @examples
randomsInverseGamma<-function(shape, scale, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsInverseGamma", shape, scale, as.integer(n))
}

#' Title
#'
#' @param shape
#' @param scale
#' @param x
#'
#' @return
#' @export
#'
#' @examples
densityInverseGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityInverseGamma", shape, scale, x)
}

#' Title
#'
#' @param shape
#' @param scale
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cdfInverseGamma<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfInverseGamma", shape, scale, x)
}

#' Title
#'
#' @param shape
#' @param scale
#' @param n
#'
#' @return
#' @export
#'
#' @examples
randomsInverseGaussian<-function(shape, scale, n){
  .jcall("demetra/stats/r/Distributions", "[D", "randomsInverseGaussian", shape, scale, as.integer(n))
}

#' Title
#'
#' @param shape
#' @param scale
#' @param x
#'
#' @return
#' @export
#'
#' @examples
densityInverseGaussian<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "densityInverseGaussian", shape, scale, x)
}

#' Title
#'
#' @param shape
#' @param scale
#' @param x
#'
#' @return
#' @export
#'
#' @examples
cdfInverseGaussian<-function(shape, scale, x){
  .jcall("demetra/stats/r/Distributions", "[D", "cdfInverseGaussian", shape, scale, x)
}


