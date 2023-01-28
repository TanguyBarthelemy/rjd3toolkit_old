createVariable<-function(id, name = NULL, lag = 0, coef = NULL, regeffect=c("Undefined", "Trend", "Seasonal", "Irregular", "Series", "SeasonallyAdjusted")){
  regeffect=match.arg(regeffect)
  if (is.null(name)) {
    name<-id
  }
  res = list(id=id, name=name, lag=lag, coef = fixedParameter(coef), regeffect=regeffect)
  return (res)
}

createRamp<-function(start, end, name = NULL, coef=NULL){
  res = list(name=name, start=start, end=end, coef = fixedParameter(coef))
  return (res)
}
createOutlier<-function(code, pos, name = NULL, coef=NULL){
  res = list(name=name, pos=pos, code=code, coef = fixedParameter(coef))
  return (res)
}

fixedParameters<-function(coef){
  ncoef<-length(coef)
  if (ncoef == 0)return (NULL)
  l<-lapply(coef, function(v){list(value=v, type='FIXED')})
  return (l)
}

fixedParameter<-function(coef){
  if (is.null(coef)) return (NULL)
  if (coef == 0) return (NULL)
  return (list(value=coef, type='FIXED'))
}

