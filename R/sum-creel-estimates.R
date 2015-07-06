creel_sum <- function(data, alpha){
  
  data %<>% dplyr::mutate_(.dots = setNames(list(~SD * 2), c("var")))
  data %<>% dplyr::select_(~Year, ~Month, ~Parameter, ~Estimate, ~var)
  sum_est <- sum(data$Estimate)
  var_est <- sum(data$var)
  sd_est <- sqrt(var_est)
  lower <- sum_est - sd_est * qnorm(1 - alpha)
  upper <- sum_est + sd_est * qnorm(1 - alpha)  
  result <- data.frame(sum_est, sd_est, lower, upper, row.names = NULL)
  names(result) <- c("Sum_Estimate", "SD", "Lower", "Upper")  
  result
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
  
  plyr::ddply(data, c(by, "Parameter"), .fun = creel_sum, alpha)
} 