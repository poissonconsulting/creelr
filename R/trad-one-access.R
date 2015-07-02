day_type <- function (x, weekend = c("Saturday", "Sunday")) {
  assert_that(is.date(x))
  x %<>% lubridate::wday(label = TRUE, abbr = FALSE)
  allweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  levels(x) <- list(Week = setdiff(allweek, weekend),
                    Weekend = weekend)
  x
}

sub_holiday <- function(data, holidays = NULL) {
  if (is.null(holidays) == FALSE) {
    assert_that(is.date(holidays))
    k <- length(holidays)
    for (i in 1:k) {
      data$DayType[data$Date == holidays[i]] <- "Weekend"
    }
  }
  data$DayType
}


nday_type_month <- function (month, year = 2000, 
                             weekend = c("Saturday", "Sunday"),
                             holidays = NULL) {
  assert_that(is.count(month))
  assert_that(is.count(year))
  
  first <- as.Date(paste(year, month, 01, sep = "-"))
  last <- as.Date(first + months(1) - lubridate::days(1))
  x <- seq(first, last, by = "day")
  x2 <- day_type(x, weekend)
  y <- data.frame(x, x2)
  names(y) <- c("Date", "DayType")
  x2 <- sub_holiday(y, holidays)
  c(Week = sum(x2 == "Week"), Weekend = sum(x2 == "Weekend"))
}

check_period <- function(data) {
  red <- aggregate(data$Catch, by = list(data$Date, data$Period), sum )
  blue <- unique(data$Date)
  length(red$Group.1) == length(blue)
}

trad_one_access_month <- function (data, weekend = c("Saturday", "Sunday"),
                                   holidays = NULL, alpha = 0.05, weighted = FALSE) {
  sample_days <- unique(dplyr::select_(data, ~Date, ~DayType, ~Month, ~Period, ~Probability))
  wk <- sample_days$DayType[sample_days$Month == lubridate::month(sample_days$Date[1])] == "Week"
  wknd <- sample_days$DayType[sample_days$Month == lubridate::month(sample_days$Date[1])] == "Weekend"
  w_wk <- wk*sample_days$Probability[sample_days$Month == lubridate::month(sample_days$Date[1])]
  w_wknd <- wknd*sample_days$Probability[sample_days$Month == lubridate::month(sample_days$Date[1])]
  samplen <- c(Week = sum(wk), Weekend = sum(wknd))
  wsamplen <- c(Week = sum(w_wk), Weekend = sum(w_wknd))
  totaln <- nday_type_month(lubridate::month(data$Date[1]), 
                            lubridate::year(data$Date[1]),
                            weekend, holidays)
  wsample_n <- matrix(rep(wsamplen, 2), nrow = 2, byrow = T)
  sample_n <- matrix(rep(samplen, 2), nrow = 2, byrow = T)
  total_n <- matrix(rep(totaln, 2), nrow = 2, byrow = T)
  
  daily_data <- aggregate(data$daily_cat, by = list(data$Date, data$Year, 
                                                    data$Month, data$DayType, data$Probability), sum)
  agg_eff <- aggregate(data$daily_eff, by = list(data$Date, data$Year, 
                                                 data$Month, data$DayType, data$Probability), sum)
  daily_data$daily_eff <- agg_eff$x
  names(daily_data) <- c("Date", "Year", "Month", "DayType", "Probability", "daily_cat", "daily_eff")
  
  variab <- as.factor(c(rep(1, nrow(daily_data)), rep(2, nrow(daily_data))))
  dt <- as.factor(rep(as.numeric(daily_data$DayType),2))
  daily <- c(daily_data$daily_eff, daily_data$daily_cat)
  data2 <- data.frame(variab, dt, daily)
  
  mean_est <- tapply(data2$daily, list(variab, dt), mean)
  var_est <- tapply(data2$daily, list(variab, dt), function(x) var(x)/length(x))
  var_total <- total_n^2 * var_est
  sd_total <- sqrt(var_total)
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
#' @param data A data.frame containing Date, DayType, Period, RodHours and Catch
#' @param am A flag indicating the selection probility for the AM period
#' @param holidays A Date vector containing holidays that can be treated as weekends
#' @param weekend A string vector indicating the days to be considered weekend
#' @param alpha The significance level desired for the confidence intervals
#' @param weighted A logical value that indicated whether the sampling coverage should weight by Period or not
#'
#' @return A data.frame with the total effort and catch estimates, confidence intervals, in-week and weekend days and sampling coverage
#' @export
#'
#' @examples
#' data(toa_dummy)
#' trad_one_access(toa_dummy)
trad_one_access <- function (data, am = 0.5, holidays = NULL, 
                             weekend = c("Saturday", "Sunday"),
                             alpha = 0.05, weighted = FALSE) {
  assert_that(is.numeric(am))
  assert_that(is.data.frame(data))
  assert_that(is.numeric(alpha))
  
  assert_that(is.logical(weighted))
  if (am > 1 || am < 0) stop("am must be a probability")
  if (check_period(data) == F) stop("Only one time period allowed per day")
  if (alpha > 1 || alpha < 0) stop("alpha must be a probability")
  
  data$Date %<>% as.Date()
  data$DayType <- day_type(data$Date, weekend)
  data$Period %<>% factor(levels = c("AM", "PM"))
  data$Year <- lubridate::year(data$Date)
  data$Month <- lubridate::month(data$Date)
  
  data$DayType <- sub_holiday(data, holidays)
  
  data$Probability <- c(am, 1 - am)[as.integer(data$Period)]
  data %<>% dplyr::mutate_("daily_eff" = "RodHours / Probability", 
                           "daily_cat" = "Catch / Probability")
  
  plyr::ddply(data, c("Year", "Month"), .fun = trad_one_access_month, weekend = weekend, 
              holidays = holidays, alpha = alpha, weighted = weighted)
}

