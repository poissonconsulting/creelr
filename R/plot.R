#' Plot Creel Data
#'
#' @inheritParams trad_one_access
#' @param parameter A string of the parameter to plot ('Catch' or 'Effort').
#' @return A \code{\link{ggplot}} object.
#' @export
#' @examples
#' library(ggplot2)
#' data(toa_example)
#' plot_creel_data(toa_example) + ylab("Daily Catch")
plot_creel_data <- function(data, weekend = c("Saturday", "Sunday"),
                            holidays = NULL, parameter = "Catch") {
  
  assert_that(is.data.frame(data))
  assert_that(is.character(weekend) && noNA(weekend))
  assert_that(is.null(holidays) || is.date(holidays))
  
  check_rows(data)
  check_columns(data, c("Date", "Period", "RodHours", "Catch"))
  check_class_columns(data, list(Date = "Date", Period = c("character", "factor"), 
                                 RodHours = c("numeric", "integer"), 
                                 Catch = c("numeric", "integer")))

  data %<>% process_trad_one_access(weekend = weekend, holidays = holidays) 
  
  data %<>% dplyr::filter_(~Parameter == parameter)
  
  ggplot2::ggplot(data = data, ggplot2::aes_string(x = "Date", y = "Value")) +
    ggplot2::geom_point(ggplot2::aes_string(shape = "DayType", color = "DayType")) +
    ggplot2::expand_limits(y = 0)
}

#' Plot Creel Estimates
#'
#' @param data A data.frame with the columns Parameter, Estimate, Lower and Upper
#' and the variable for the x-axis.
#' @param parameter A string of the parameter to plot ('Catch' or 'Effort').
#' @param x A string of the variable to plot on the x-axis.
#' @return A \code{\link{ggplot}} object.
#' @export
#' @examples
#' library(ggplot2)
#' data(toa_example)
#' plot_creel_estimates(trad_one_access(toa_example)) + ylab("Monthly Catch")
plot_creel_estimates <- function(data, parameter = "Catch", x = "Month") {
  assert_that(is.data.frame(data))
  assert_that(is.string(parameter))
  assert_that(is.string(x))
  
  check_rows(data)
  check_columns(data, c(x, "Parameter", "Estimate", "Lower", "Upper"))
  check_class_columns(data, list(Parameter = c("factor", "character"),
                                 Estimate = "numeric",
                                 Lower = "numeric",
                                 Upper = "numeric"))

  data %<>% dplyr::filter_(~Parameter == parameter)
  ggplot2::ggplot(data = data, ggplot2::aes_string(x = x, y = "Estimate")) +
    ggplot2::geom_pointrange(ggplot2::aes_string(ymin = "Lower", ymax = "Upper")) +
    ggplot2::expand_limits(y = 0)
}
