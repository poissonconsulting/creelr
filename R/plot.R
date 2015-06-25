#' Plot Creel Estimates
#'
#' @param data A data.frame with the columns Month, Parameter, Estimate and SE.
#' @param parameter A string of the parameter to filter by.
#' @return A \code{\link{ggplot}} object.
#' @export
#' @examples
#' library(ggplot2)
#' data(toa_example)
#' plot_creel_estimates(trad_one_access(toa_example)) + ylab("Catch")
plot_creel_estimates <- function(data, parameter = "Catch") {
  assert_that(is.data.frame(data))
  assert_that(is.string(parameter))
  
  check_rows(data)
  check_columns(data, c("Month", "Parameter", "Estimate", "SE"))
  check_class_columns(data, list(Month = "numeric", 
                                 Parameter = c("factor", "character"),
                                 Estimate = "numeric",
                                 SE = "numeric"))
  
  data$Lower <- data$Estimate - data$SE * 1.96
  data$Upper <- data$Estimate + data$SE * 1.96
  
  data %<>% dplyr::filter_(~Parameter == parameter)
  ggplot2::ggplot(data = data, ggplot2::aes_string(x = "Month", y = "Estimate")) +
    ggplot2::geom_pointrange(ggplot2::aes_string(ymin = "Lower", ymax = "Upper")) +
    ggplot2::expand_limits(y = 0)
}
