#' @export
.JD3_ENV<-new.env(emptyenv())


.onLoad <- function(libname, pkgname) {
  # For debugging: to see if Jars are effectively loaded
  # options(java.parameters = "-verbose:class")

  # TODO : devtools will look only in RJDemetra3\java for JAR files so copied them there too
  result <- .jpackage(pkgname, lib.loc=libname)
  if (!result) stop("Loading java packages failed")

  # what's your java  version?  Need > 1.5.0.
  jversion <- .jcall('java.lang.System','S','getProperty','java.version')
  if (jversion < "1.8.0") {
    stop(paste("Your java version is ", jversion,
               ".  Need 1.8.0 or higher.", sep=""))
  }

  proto.dir <- system.file("proto", package = pkgname)
  readProtoFiles2(protoPath = proto.dir)

  .JD3_ENV$enum_extract<-enum_extract
  .JD3_ENV$enum_of<-enum_of
  .JD3_ENV$jd2r_test<-jd2r_test
  .JD3_ENV$matrix_jd2r<-matrix_jd2r
  .JD3_ENV$matrix_r2jd<-matrix_r2jd
  .JD3_ENV$ts_jd2r<-ts_jd2r
  .JD3_ENV$ts_r2jd<-ts_r2jd
  .JD3_ENV$tsdomain_r2jd<-tsdomain_r2jd
}

