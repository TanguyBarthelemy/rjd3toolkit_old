#' @include jd3rslts.R
NULL

OBJ<-'JD3_Object'
RSLT<-'JD3_ProcResults'

jd3Object<-function(jobjRef, subclasses=NULL, result=F){
  if (result)
    classes<-c(OBJ, RSLT, subclasses)
  else
    classes<-c(OBJ, subclasses)
  return (structure(list(internal=jobjRef), class=classes))
}

#' Title
#'
#' @param object
#'
#' @return
#' @export
#'
#' @examples
dictionary<-function(object){
  if (! is(object, RSLT))
    stop("No dictionary for this type of object")
  if (is.jnull(object$internal)){
    stop("No java object")
  }else{
    if (.jinstanceof(object$internal, "demetra/information/Explorable")){
      proc_dictionary2(object$internal)
    }else{
      proc_dictionary(.jclass(object$internal))
    }
  }
}

#' Title
#'
#' @param object
#' @param id
#'
#' @return
#' @export
#'
#' @examples
result<-function(object, id){
  if (! is(object, RSLT))
    stop("No result for this type of object")
  if (is.jnull(object$internal)){
    stop("No java object")
  }else{
    proc_data(object$internal, id)}
}

