#' @include utils.R
NULL

DATE_MIN<-NULL

DATE_MAX<-NULL

.onLoad <- function(libname, pkgname) {

  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  # what's your java  version?  Need > 17.0.0
  jversion <- .jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "17.0.0") {
    stop(paste("Your java version is ", jversion,
               ".  Need 17.0.0 or higher.", sep=""))
  }

  proto.dir <- system.file("proto", package = pkgname)
  readProtoFiles2(protoPath = proto.dir)

  DATE_MIN<<-dateOf(1,1,1)
  DATE_MAX<<-dateOf(9999, 12, 31)

}


