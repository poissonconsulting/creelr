creel_sum <- function(data, alpha){
  dplyr::summarise_(data, .dots = setNames(list(~sum(Estimate), ~sqrt(sum(SD ^ 2))), 
                                       c("Estimate", "SD")))  %>%
    dplyr::mutate_(.dots = setNames(list(~Estimate - SD * qnorm(1 - alpha),
                                         ~Estimate + SD * qnorm(1 - alpha)), 
                                   c("Lower", "Upper")))
}

#' Sum Creel Estimates
#'
#' @param data A data.frame with the results from trad_one_access
#' @param by A flag indicating the time period by which aggregation is made
#' @param alpha Significance level for confidence intervals
#'
#' @return A data.frame with the aggregated results
#' @export
#'
#' @examples
#' data(toa_dummy)
#' toa_results <- trad_one_access(toa_dummy)
#' sum_creel_estimates(toa_results)
sum_creel_estimates <- function (data, by = "Year", alpha = 0.05) {
  if (alpha > 1 || alpha < 0) stop("alpha must be a probability")
  assert_that(is.data.frame(data))
  assert_that(is.string(by))
  check_rows(data)
  check_columns(data, c("Year", "Month", "Parameter", "Estimate", "SD"))
  check_class_columns(data, list(Year = "numeric",
                                 Month = "numeric", 
                                 Parameter = c("factor", "character"),
                                 Estimate = "numeric",
                                 SD = "numeric"))
  
  data %<>% plyr::ddply(c(by, "Parameter"), .fun = creel_sum, alpha)
  data <- data[rev(order(data$Parameter)),]
  data
} 
