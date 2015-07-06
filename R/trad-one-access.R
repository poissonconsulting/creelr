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
  
  dtype <- factor(lubridate::wday(x, label = TRUE, abbr = FALSE))
  dtype <- dtype %in% weekend
  dtype %<>% factor()
  levels(dtype) <- list("Week" = FALSE, "Weekend" = TRUE)
  dtype[x %in% holidays] <- "Weekend"
  dtype
}

nday_type_month <- function(month, year, weekend, holidays) {
  assert_that(is.count(month))
  assert_that(is.count(year))
  
  first <- as.Date(paste(year, month, 01, sep = "-"))
  last <- as.Date(first + months(1, abbreviate = FALSE) - lubridate::days(1))
  dates <- seq(first, last, by = "day")
  day_type <- data.frame(DayType = day_type(dates, weekend = weekend, holidays = holidays))
  dplyr::group_by_(day_type, ~DayType) %>% 
    dplyr::summarise_(.dots = setNames(list(~n()),
                                       c("TotalDays"))) %>%
    dplyr::ungroup()
}

trad_one_access_month <- function(data, weekend, holidays, alpha, weighted) {
  
  data %<>% dplyr::mutate_(.dots = setNames(list(~Value / Probability), c("Value")))
  
  estimate <- dplyr::group_by_(data, .dots = list(~DayType)) %>% 
    dplyr::summarise_(.dots = setNames(list(~mean(Value), ~n(), ~var(Value), ~mean(Probability)), 
                                       c("Mean", "Days", "Variance", "Probability")))
  
  estimate %<>% dplyr::inner_join(nday_type_month(data$Month[1], data$Year[1], weekend, holidays), 
                                  by = "DayType")
  
  estimate %<>% dplyr::mutate_(.dots = setNames(list(
    ~Mean * TotalDays, ~sqrt(Variance / Days * TotalDays ^ 2), ~Days * Probability), 
    c("Estimate", "SD", "WeightedDays")))
  
  total <- dplyr::ungroup(estimate) %>% 
    dplyr::summarise_(.dots = setNames(list(~sum(Estimate), ~sqrt(sum(SD ^ 2))), c("Estimate", "SD"))) %>%
    dplyr::mutate_(.dots = setNames(list(~Estimate - SD * qnorm(1 - alpha), ~Estimate + SD * qnorm(1 - alpha)), c("Lower", "Upper")))
  
  total$WK <- estimate$TotalDays[estimate$DayType == "Week"]
  total$WKND <- estimate$TotalDays[estimate$DayType == "Weekend"]
  
  if (weighted)
    estimate$Days <- estimate$WeightedDays
  
  total$Coverage_WK <- estimate$Days[estimate$DayType == "Week"]
  total$Coverage_WKND <- estimate$Days[estimate$DayType == "Weekend"] 
  
  dplyr::select_(total, .dots = list(~Estimate, ~SD, ~Lower, ~Upper, ~WK, 
                                     ~WKND, ~Coverage_WK, ~Coverage_WKND))
}

process_trad_one_access <- function(data, weekend, holidays) {
  data %<>% dplyr::group_by_(.dots = list(~Date, ~Period)) %<>% 
    dplyr::summarise_(.dots = setNames(list(~sum(Catch), ~sum(RodHours)), c("Catch", "Effort"))) %>%
    dplyr::ungroup()
  
  if (anyDuplicated(data$Date)) stop("Only one time period allowed per day")
  
  data$DayType <- day_type(data$Date, weekend, holidays)
  
  data$Period %<>% factor(levels = c("AM", "PM"))
  data$Year <- lubridate::year(data$Date)
  data$Month <- lubridate::month(data$Date)
  
  tidyr::gather_(data, "Parameter", "Value", c("Effort", "Catch"))
}

#' Traditional One Access
#' 
#' Calculates estimates for Traditional Access Design for One Access Site.
#'
#' @param data A data.frame containing Date, Period, RodHours and Catch columns.
#' @param am A number indicating the weighting for the AM period (by default 0.5).
#' @param weekend A string indicating the days to be considered weekend.
#' @param holidays A Date vector of weekday holidays that can be treated as weekends.
#' @param alpha A number indicating the significance level desired for the confidence intervals.
#' @param weighted A flag that indicates whether the sampling coverage should weight by Period.
#' @return A data.frame with the total effort and catch estimates, confidence intervals, in-week and weekend days and sampling coverage
#' @export
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
  
  data %<>% process_trad_one_access(weekend = weekend, holidays = holidays) 
  
  data$Probability <- c(am, 1 - am)[as.integer(data$Period)]
  
  plyr::ddply(data, c("Year", "Month", "Parameter"), .fun = trad_one_access_month, 
              weekend = weekend, 
              holidays = holidays, alpha = alpha, weighted = weighted)
  
}
