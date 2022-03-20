#' Generic Likelihood Function for JDemetra+
#'
#' Generic functions to format likelihood JDemetra+
#'
#' @param nobs,neffectiveobs,nparams,ll,adjustedll,aic,aicc,bic,bicc,ssq parameters.
#'
#' @return A \code{"JD3_LIKELIHOOD"} object.
#' @export
likelihood<-function(nobs, neffectiveobs=NA, nparams=0, ll, adjustedll=NA, aic, aicc, bic, bicc, ssq){

  if (is.na(neffectiveobs)) neffectiveobs=nobs
  if (is.na(adjustedll)) adjustedll=ll

  return (structure(list(nobs=nobs, neffectiveobs=neffectiveobs, nparams=nparams,
                         ll=ll, adjustedll=adjustedll,
                         aic=aic, aicc=aicc, bic=bic, bicc=bicc, ssq=ssq),
                    class = "JD3_LIKELIHOOD"))
}
