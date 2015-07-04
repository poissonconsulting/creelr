#' Get Day Type
#'
#' @param x A Date vector to get the day types for.
#' @inheritParams trad_one_access
#' @return A factor of the day types ('Week' or 'Weekend').
#' @export
#' @examples
#' day_type(seq(as.Date("2000-12-01"), as.Date("2000-12-31"), by = "day"))
#' day_type(seq(as.Date("2000-12-01"), as.Date("2000-12-31"), by = "day"), 
#'   holidays = as.Date("2000-12-25"))
day_type <- function(x, weekend = c("Saturday", "Sunday"), holidays = NULL) {
  assert_that(is.date(x))
  assert_that(is.character(weekend) && noNA(weekend))
  assert_that(is.null(holidays) || is.date(holidays))
  
  dtype <- lubridate::wday(x, label = TRUE, abbr = FALSE)
  allweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  levels(dtype) <- list(Week = setdiff(allweek, weekend),
                    Weekend = weekend)
  
  dtype[x %in% holidays] <- "Weekend"
  dtype
}

nday_type_month <- function(month, year, weekend, holidays) {
  assert_that(is.count(month))
  assert_that(is.count(year))
  
  first <- as.Date(paste(year, month, 01, sep = "-"))
  last <- as.Date(first + months(1, abbreviate = FALSE) - lubridate::days(1))
  dates <- seq(first, last, by = "day")
  x2 <- day_type(dates, weekend = weekend, holidays = holidays)
  c(Week = sum(x2 == "Week"), Weekend = sum(x2 == "Weekend"))
}

trad_one_access_month <- function(data, weekend, holidays, alpha, weighted) {
  
  wk <- data$DayType == "Week"
  wknd <- data$DayType == "Weekend"
  w_wk <- wk * data$Probability
  w_wknd <- wknd * data$Probability
  samplen <- c(Week = sum(wk), Weekend = sum(wknd))
  wsamplen <- c(Week = sum(w_wk), Weekend = sum(w_wknd))
  totaln <- nday_type_month(data$Month[1], data$Year[1], weekend, holidays)
  wsample_n <- matrix(rep(wsamplen, 2), nrow = 2, byrow = T)
  sample_n <- matrix(rep(samplen, 2), nrow = 2, byrow = T)
  total_n <- matrix(rep(totaln, 2), nrow = 2, byrow = T)
  
  variab <- as.factor(c(rep(1, nrow(data)), rep(2, nrow(data))))
  dt <- as.factor(rep(as.numeric(data$DayType),2))
  daily <- c(data$daily_eff, data$daily_cat)
  data2 <- data.frame(variab, dt, daily)
  
  mean_est <- tapply(data2$daily, list(variab, dt), mean)
  var_est <- tapply(data2$daily, list(variab, dt), function(x) var(x)/length(x))
  var_total <- total_n ^ 2 * var_est
  total_est <- total_n * mean_est
  overall_est <- apply(total_est, 1, sum)
  overall_var <- apply(var_total, 1, sum)
  overall_sd <- sqrt(overall_var)
  lower <- overall_est - overall_sd * qnorm(1 - alpha)
  upper <- overall_est + overall_sd * qnorm(1 - alpha) 
  if (weighted == TRUE) {
    result <- data.frame(c("Effort", "Catch"),overall_est, overall_sd, lower, upper,  
                         total_n[ ,1], total_n[ ,2], wsample_n[ ,1], wsample_n[ ,2], 
                         row.names = NULL)
  } else {
    result <- data.frame(c("Effort", "Catch"),overall_est, overall_sd, lower, upper,  
                         total_n[ ,1], total_n[ ,2], sample_n[ ,1], sample_n[ ,2], 
                         row.names = NULL)
  }
  
  names(result) <- c("Parameter", "Estimate", "SD", "Lower", "Upper", "WK", 
                     "WKND", "Coverage_WK", "Coverage_WKND")
  
  result
}


#' Traditional One Access Estimates
#'
#' @param data A data.frame containing Date, Period, RodHours and Catch.
#' @param am A number indicating the weighting for the AM period.
#' @param weekend A string indicating the days to be considered weekend.
#' @param holidays A Date containing holidays that can be treated as weekends.
#' @param alpha A number indicating the significance level desired for the confidence intervals.
#' @param weighted A flag that indicates whether the sampling coverage should weight by Period.
#'
#' @return A data.frame with the total effort and catch estimates, confidence intervals, in-week and weekend days and sampling coverage
#' @export
#'
#' @examples
#' data(toa_dummy)
#' trad_one_access(toa_dummy)
trad_one_access <- function(data, am = 0.5, 
                             weekend = c("Saturday", "Sunday"),
                             holidays = NULL, 
                             alpha = 0.05, weighted = FALSE) {
  assert_that(is.data.frame(data))
  assert_that(is.number(am) && noNA(am))
  assert_that(is.character(weekend) && noNA(weekend))
  assert_that(is.null(holidays) || is.date(holidays))
  assert_that(is.number(alpha) && noNA(alpha))
  assert_that(is.flag(weighted) && noNA(weighted))
 
  if (am > 1 || am < 0) stop("am must be a probability")
  if (alpha > 1 || alpha < 0) stop("alpha must be a probability")
  
  check_columns(data, c("Date", "Period", "RodHours", "Catch"))
  check_class_columns(data, list(Date = "Date", Period = c("character", "factor"), 
                                 RodHours = c("numeric", "integer"), 
                                 Catch = c("numeric", "integer")))
  
  if (!all(data$Period %in% c("AM", "PM")))
    stop("the values in the data Period column must be 'AM' or 'PM'")

  data %<>% dplyr::group_by_(.dots = list(~Date, ~Period)) %<>% 
    dplyr::summarise_(.dots = setNames(list(~sum(Catch), ~sum(RodHours)), c("Catch", "RodHours"))) %>%
    dplyr::ungroup()
  
  if (anyDuplicated(data$Date)) stop("Only one time period allowed per day")
  
  data$DayType <- day_type(data$Date, weekend, holidays)
  
  data$Period %<>% factor(levels = c("AM", "PM"))
  data$Year <- lubridate::year(data$Date)
  data$Month <- lubridate::month(data$Date)
  
  data$Probability <- c(am, 1 - am)[as.integer(data$Period)]
  data %<>% dplyr::mutate_("daily_eff" = "RodHours / Probability", 
                           "daily_cat" = "Catch / Probability")
  
  data %<>% dplyr::select_(~Year, ~Month, ~DayType, ~Period, ~Probability, 
                           ~daily_eff, ~daily_cat)
  
  plyr::ddply(data, c("Year", "Month"), .fun = trad_one_access_month, weekend = weekend, 
              holidays = holidays, alpha = alpha, weighted = weighted)
}
