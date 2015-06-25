day_type <- function (x, weekend = c("Saturday", "Sunday")) {
  assert_that(is.date(x))
  x %<>% lubridate::wday(label = TRUE, abbr = FALSE)
  allweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  levels(x) <- list(Week = setdiff(allweek, weekend),
                    Weekend = weekend)
  x
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
  if (is.date(holidays) == TRUE) {
    k <- length(holidays)
    for (i in 1:k) {
      x2[x == holidays[i]] <- "Weekend"
    }
  }
  c(Week = sum(x2 == "Week"), Weekend = sum(x2 == "Weekend"))
}

trad_one_access_month <- function (data, weekend = c("Saturday", "Sunday"),
                                   holidays = NULL, alpha = 0.05 ) {
  samplen <- c(Week = sum(data$DayType[data$Month == lubridate::month(data$Date[1])] == "Week"), 
                Weekend = sum(data$DayType[data$Month == lubridate::month(data$Date[1])] == "Weekend"))
  totaln <- nday_type_month(lubridate::month(data$Date[1]), 
                            lubridate::year(data$Date[1]),
                            weekend, holidays)
  sample_n <- matrix(rep(samplen, 2), nrow = 2, byrow = T)
  total_n <- matrix(rep(totaln, 2), nrow = 2, byrow = T)
  
  variab <- as.factor(c(rep(1, nrow(data)), rep(2, nrow(data))))
  dt <- as.factor(rep(as.numeric(data$DayType),2))
  daily <- c(data$daily_eff, data$daily_cat)
  data2 <- data.frame(variab, dt, daily)
  
  mean_est <- tapply(data2$daily, list(variab, dt), mean)
  var_est <- tapply(data2$daily, list(variab, dt), function(x) var(x)/length(x))
  var_total <- total_n^2 * var_est
  se_total <- sqrt(var_total)
  total_est <- total_n * mean_est
  overall_est <- apply(total_est, 1, sum)
  overall_var <- apply(var_total, 1, sum)
  overall_se <- sqrt(overall_var)
  lower <- overall_est - overall_se * qt(1 - alpha, samplen - 1)
  upper <- overall_est + overall_se * qt(1 - alpha, samplen - 1)
  result <- data.frame(c("Effort", "Catch"),overall_est, overall_se, 
                       total_n[ ,1], total_n[ ,2], sample_n[ ,1], sample_n[ ,2], 
                       row.names = NULL)
  names(result) <- c("Parameter", "Estimate", "SE", "Working Days", "Holidays", 
                     "Coverage Working Days", "Coverage Holidays")
  
  result
}

#' Title
#'
#' @param data A data.frame xx
#' @param am A flag indicating xx
#'
#' @return A data.frame with xx
#' @export
#'
#' @examples
#' data(toa_dummy)
#' trad_one_access(toa_dummy)
trad_one_access <- function (data, am = 0.5, holidays = NULL, 
                             weekend = c("Saturday", "Sunday"),
                             alpha = 0.05) {
  data$Date %<>% as.Date()
  data$DayType <- day_type(data$Date, weekend)
  data$Period %<>% factor(levels = c("AM", "PM"))
  data$Year <- lubridate::year(data$Date)
  data$Month <- lubridate::month(data$Date)
  
  if (is.date(holidays) == TRUE) {
    k <- length(holidays)
    for (i in 1:k) {
      data$DayType[data$Date == holidays[i]] <- "Weekend"
    }
  }
  
  data$Probability <- c(am, 1 - am)[as.integer(data$Period)]
  data %<>% dplyr::mutate_("daily_eff" = "RodHours / Probability", 
                           "daily_cat" = "Catch / Probability")
  
  plyr::ddply(data, c("Year", "Month"), trad_one_access_month)
}


sum_creel_estimates <- function (data, by = "Year") {
  assert_that(is.data.frame(data))
  assert_that(is.string(by))
  check_rows(data)
  check_columns(data, c("Year", "Month", "Parameter", "Estimate", "SE"))
  check_class_columns(data, list(Month = "numeric", 
                                 Parameter = c("factor", "character"),
                                 Estimate = "numeric",
                                 SE = "numeric"))
  #xx
} 
