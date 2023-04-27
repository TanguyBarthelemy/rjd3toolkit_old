#' Set Benchmarking Specification
#'
#' @param x the specification.
#' @param enabled Boolean to enable the user to perform benchmarking.
#' @param target specifies the target variable for the benchmarking procedure, which can be the raw series (\code{"Original"}); or the series adjusted for calendar effects (\code{"CalendarAdjusted"}).
#' @param rho the value of the AR(1) parameter (set between 0 and 1) in the function used for benchmarking.
#' @param lambda a parameter in the function used for benchmarking that relates to the weights in the regression equation; it is typically equal to 0, 1/2 or 1.
#' @param forecast Boolean indicating if the forecasts of the seasonally adjusted series and of the target variable (\code{target}) are used in the benchmarking computation so the benchmarking constrain is applied also to the forecasting period.
#' @param bias TODO
#' @export
set_benchmarking <- function(x, enabled = NA,
                             target = c(NA, "CalendarAdjusted", "Original"),
                             rho = NA,
                             lambda = NA,
                             forecast = NA,
                             bias = c(NA, "None")) {
    UseMethod("set_benchmarking", x)
}
#' @export
set_benchmarking.default <- function(x, enabled = NA,
                                     target = c(NA, "CalendarAdjusted", "Original"),
                                     rho = NA,
                                     lambda = NA,
                                     forecast = NA,
                                     bias = c(NA, "None")) {
    target <- match.arg(toupper(target[1]),
                        c(NA, "CALENDARADJUSTED", "ORIGINAL"))
    bias <- match.arg(toupper(bias)[1],
                      c(NA, "NONE"))
    benchmarking <- x$benchmarking
    if (!is.na(enabled) && is.logical(enabled)) {
        benchmarking$enabled <- enabled
    }
    if (!is.na(target)) {
        benchmarking$target <- sprintf("TARGET_%s", target)
    }
    if (!is.na(lambda)) {
        benchmarking$lambda <- lambda
    }
    if (!is.na(rho)) {
        benchmarking$rho <- rho
    }
    if (!is.na(bias)) {
        benchmarking$bias <- sprintf("BIAS_%s", bias)
    }
    if (!is.na(forecast) && is.logical(forecast)) {
        benchmarking$forecast <- forecast
    }
    x$benchmarking <- benchmarking
    
    x
}
