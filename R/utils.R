#' @importFrom rJava .jpackage .jcall .jnull .jarray .jevalArray .jcast .jcastToArray .jinstanceof is.jnull .jnew .jclass
#' @importFrom methods is
NULL

#' @export
#' @rdname jd3_utilities
ymd<-function(y, m, d=1){
  return (as.Date(sprintf("%04i-%02i-%02i", y, m, d)))
}
#' @export
#' @rdname jd3_utilities
yearOf<-function(s){
  return ( as.integer(substr(s, 1, 4)))
}
#' @export
#' @rdname jd3_utilities
monthOf<-function(s){
  return ( as.integer(substr(s, 6, 7)))
}
#' @export
#' @rdname jd3_utilities
dayOf<-function(s){
  return ( as.integer(substr(s, 9, 10)))
}
#' @export
#' @rdname jd3_utilities
dateOf<-function(year, month, day){
  d<-jd3.Date$new()
  d$year<-year
  d$month<-month
  d$day<-day
  return (d)
}

#' @export
#' @rdname jd3_utilities
parseDate<-function(s){
  d<-jd3.Date$new()
  d$year<-yearOf(s)
  d$month<-monthOf(s)
  d$day<-dayOf(s)
  return (d)
}

#' Title
#'
#' @export
reload_dictionaries<-function(){
  .jcall("demetra/information/InformationExtractors", "V", "reloadExtractors")
}





