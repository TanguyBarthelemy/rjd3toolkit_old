ymd<-function(y, m, d=1){
  return (as.Date(sprintf("%04i-%02i-%02i", y, m, d)))
}

yearOf<-function(s){
  return ( as.integer(substr(s, 1, 4)))
}

monthOf<-function(s){
  return ( as.integer(substr(s, 6, 7)))
}

dayOf<-function(s){
  return ( as.integer(substr(s, 9, 10)))
}

dateOf<-function(year, month, day){
  d<-jd3.Date$new()
  d$year<-year
  d$month<-month
  d$day<-day
  return (d)
}

parseDate<-function(s){
  d<-jd3.Date$new()
  d$year<-yearOf(s)
  d$month<-monthOf(s)
  d$day<-dayOf(s)
  return (d)
}

ts_r2jd<-function(s){
  if (is.null(s)){
    return (NULL)
  }
  freq<-frequency(s)
  start<-start(s)
  .jcall("demetra/timeseries/r/TsUtility", "Ldemetra/timeseries/TsData;", "of",
         as.integer(freq), as.integer(start[1]), as.integer(start[2]), as.double(s))
}

tsdomain_r2jd<-function(period, startYear, startPeriod, length){
  .jcall("demetra/timeseries/r/TsUtility", "Ldemetra/timeseries/TsDomain;", "of",
         as.integer(period), as.integer(startYear), as.integer(startPeriod), as.integer(length))
}


ts_jd2r<-function(s){
  if (is.null(s)){
    return (NULL)
  }
  pstart<-.jcall("demetra/timeseries/r/TsUtility", "[I", "startPeriod", s)
  jx<-.jcall(s, "Ldemetra/data/DoubleSeq;", "getValues")
  x<-.jcall(jx, "[D", "toArray")
  ts(x,start=pstart[2:3], frequency=pstart[1])
}

matrix_jd2r<-function(s){
  if (is.jnull(s)){
    return (NULL)
  }
  nr<-.jcall(s, "I", "getRowsCount")
  nc<-.jcall(s, "I", "getColumnsCount")
  d<-.jcall(s, "[D", "toArray")
  return (array(d, dim=c(nr, nc)))
}

matrix_r2jd<-function(s){
  if (is.null(s))
    return (.jnull("demetra/maths/matrices/Matrix"))
  if (!is.matrix(s)){
    s<-matrix(s, nrow=length(s), ncol=1)
  }
  sdim<-dim(s)
  return (.jcall("demetra/maths/matrices/Matrix","Ldemetra/maths/matrices/Matrix;", "ofInternal", as.double(s), as.integer(sdim[1]), as.integer(sdim[2])))
}

enum_extract<-function(type, p){
  name<-type$value(number=p)$name()
  return (substring(name, regexpr("_", name)+1))
}

enum_of<-function(type, code, prefix){
  i<-type$value(name=paste(prefix, code, sep='_'))$number()
}



