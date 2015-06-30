#' Plot Creel Estimates
#'
#' @param data A data.frame with the columns Month, Parameter, Estimate and SE.
#' @param parameter A string of the parameter to filter by.
#' @param x A string of the variable to plot on the x-axis.
#' @return A \code{\link{ggplot}} object.
#' @export
#' @examples
#' library(ggplot2)
#' data(toa_example)
#' plot_creel_estimates(trad_one_access(toa_example)) + ylab("Catch")
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
    ggplot2::expand_limits(y = 0) + ggplot2::ylab(parameter)
}
