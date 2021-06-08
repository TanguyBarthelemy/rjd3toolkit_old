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

  .JD3_ENV$jd2r_test<-jd2r_test
  .JD3_ENV$matrix_jd2r<-matrix_jd2r
  .JD3_ENV$matrix_r2jd<-matrix_r2jd
  .JD3_ENV$ts_jd2r<-ts_jd2r
  .JD3_ENV$ts_r2jd<-ts_r2jd
  .JD3_ENV$tsdomain_r2jd<-tsdomain_r2jd
  .JD3_ENV$proc_numeric<-proc_numeric
  .JD3_ENV$proc_int<-proc_int
  .JD3_ENV$proc_bool<-proc_bool
  .JD3_ENV$proc_vector<-proc_vector
  .JD3_ENV$proc_ts<-proc_ts
  .JD3_ENV$proc_str<-proc_str
  .JD3_ENV$proc_desc<-proc_desc
  .JD3_ENV$proc_test<-proc_test
  .JD3_ENV$proc_parameter<-proc_parameter
  .JD3_ENV$proc_parameters<-proc_parameters
  .JD3_ENV$proc_matrix<-proc_matrix
  .JD3_ENV$proc_data<-proc_data
  .JD3_ENV$proc_dictionary<-proc_dictionary
  .JD3_ENV$proc_likelihood<-proc_likelihood

  # perhaps to put in another environment

  .JD3_ENV$enum_extract<-enum_extract
  .JD3_ENV$enum_of<-enum_of
  .JD3_ENV$p2r_matrix<-p2r_matrix
  .JD3_ENV$p2r_ts<-p2r_ts
  .JD3_ENV$p2r_test<-p2r_test
  .JD3_ENV$p2r_parameter<-p2r_parameter
  .JD3_ENV$p2r_parameters<-p2r_parameters
  .JD3_ENV$r2p_parameter<-r2p_parameter
  .JD3_ENV$r2p_parameters<-r2p_parameters
  .JD3_ENV$p2r_parameters_rslt<-p2r_parameters_rslt
  .JD3_ENV$p2r_parameters_rsltx<-p2r_parameters_rsltx
}

