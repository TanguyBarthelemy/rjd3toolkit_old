#' @include utils.R
#' @import RProtoBuf
#' @importFrom stats frequency ts
NULL

#' @export
#' @rdname jd3_utilities
enum_sextract<-function(type, p){
  return (type$value(number=p)$name())
}
#' @export
#' @rdname jd3_utilities
enum_sof<-function(type, code){
  return (type$value(name=code)$number())
}

#' @export
#' @rdname jd3_utilities
enum_extract<-function(type, p){
  name<-type$value(number=p)$name()
  return (substring(name, regexpr("_", name)+1))
}

#' @export
#' @rdname jd3_utilities
enum_of<-function(type, code, prefix){
  i<-type$value(name=paste(prefix, code, sep='_'))$number()
}

#' @export
#' @rdname jd3_utilities
r2p_parameter<-function(r){
  p<-jd3.Parameter$new()
  if (is.null(r)) return (p)

  p$value<-r$value
  p$type<-enum_of(jd3.ParameterType, r$type, "PARAMETER")
  return (p)
}
#' @export
#' @rdname jd3_utilities
p2r_parameter<-function(p){
  if (! p$has("type")) return (NULL)
  return (list(value = p$value, type=enum_extract(jd3.ParameterType, p$type)))
}
#' @export
#' @rdname jd3_utilities
r2p_parameters<-function(r){

  n<-length(r)
  if (n == 0) return (NULL)
  p<-apply(r, 2, function(z){r2p_parameter(z)})
  return (p)
}
#' @export
#' @rdname jd3_utilities
p2r_parameters<-function(p){
  n<-length(p)
  if (n == 0) return (NULL)
  r<-sapply(p, function(z){list(value=z$value, type=enum_extract(jd3.ParameterType, z$type))})
  return (r)
}
#' @export
#' @rdname jd3_utilities
p2r_parameters_rslt<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){enum_extract(jd3.ParameterType, z$type)})
  return (data.frame(value=value, type=type))
}
#' @export
#' @rdname jd3_utilities
p2r_parameters_rsltx<-function(p){
  if (is.null(p))
    return (NULL)
  if (length(p) == 0)
    return (NULL)
  value<-sapply(p, function(z){z$value})
  type<-sapply(p, function(z){enum_extract(jd3.ParameterType, z$type)})
  description<-sapply(p, function(z){z$description})

  rslt<-data.frame(value=value, type=type)
  row.names(rslt)<-description

  return (rslt)
}
#' @export
#' @rdname jd3_utilities
p2r_test<-function(p){
  return (rjd3toolkit::statisticaltest(p$value, p$pvalue, p$description))
}
#' @export
#' @rdname jd3_utilities
p2r_matrix<-function(p){
  m<-matrix(data=p$values, nrow = p$nrows, ncol = p$ncols)
  `attr<-`(m, "name", p$name)
  return (m)
}
#' @export
#' @rdname jd3_utilities
p2r_ts<-function(p){
  if (length(p$values) == 0)
    return (NULL)
  s<-ts(data=p$values, frequency = p$annual_frequency, start = c(p$start_year, p$start_period))
  `attr<-`(s, "name", p$name)
  return (s)
}
#' @export
#' @rdname jd3_utilities
p2r_parameters_estimation<-function(p){
  if (is.null(p))
    return (NULL)
  return (list(val=p$value, score=p$score, cov=p2r_matrix(p$covariance), description=p$description))
}

#' @export
#' @rdname jd3_utilities
p2r_likelihood<-function(p){
  return (likelihood(p$nobs, p$neffectiveobs, p$nparams,
                         p$log_likelihood, p$adjusted_log_likelihood,
                         p$aic, p$aicc, p$bic, p$bicc, p$ssq))
}

#' @export
#' @rdname jd3_utilities
p2r_date<-function(p){
  if (p$has('year')){
    return (ymd(p$year, p$month, p$day))
  }else{
    return (NULL)
  }
}
#' @export
#' @rdname jd3_utilities
r2p_date<-function(s){
  if (is.null(s)) return(jd3.Date$new())
  else return (parseDate(s))
}


# Span
#' @export
#' @rdname jd3_utilities
p2r_span<-function(span){
  type<-enum_extract(jd3.SelectionType, span$type)
  dt0<-p2r_date(span$d0)
  dt1<-p2r_date(span$d1)

  return (structure(list(type=type, d0=dt0, d1=dt1, n0=span$n0, n1=span$n1), class= "JD3_SPAN"))
}
#' @export
#' @rdname jd3_utilities
r2p_span<-function(rspan){
  pspan<-jd3.TimeSelector$new()
  pspan$type<-enum_of(jd3.SelectionType, rspan$type, "SPAN")
  pspan$n0<-rspan$n0
  pspan$n1<-rspan$n1
  pspan$d0<-r2p_date(rspan$d0)
  pspan$d1<-r2p_date(rspan$d1)
  return (pspan)
}
#' @export
#' @rdname jd3_utilities
p2r_test<-function(p){
  if (is.null(p))
    return (NULL)
  else{
    return (p$as.list())
  }
}




