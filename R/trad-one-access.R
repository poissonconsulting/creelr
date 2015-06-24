day_type <- function (x, working = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
  assert_that(is.date(x))
  x %<>% lubridate::wday(label = TRUE, abbr = FALSE)
  allweek <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  levels(x) <- list(Week = working,
                    Weekend = setdiff(allweek, working))
  x
}

nday_type_month <- function (month, year = 2000) {
  assert_that(is.count(month))
  assert_that(is.count(year))
  
  first <- as.Date(paste(year, month, 01, sep = "-"))
  last <- as.Date(first + months(1) - lubridate::days(1))
  x <- seq(first, last, by = "day")
  x %<>% day_type()
  c(Week = sum(x == "Week"), Weekend = sum(x == "Weekend"))
}

trad_one_access_month <- function (data) {
  
  totaln <- nday_type_month(lubridate::month(data$Date[1]), 
                            lubridate::year(data$Date[1]))
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
  
  result <- data.frame(c("Effort", "Catch"),overall_est, overall_se, row.names = NULL)
  names(result) <- c("Parameter", "Estimate", "SE")
  
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
                             working = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) {
  data$Date %<>% as.Date()
  data$DayType <- day_type(data$Date, working)
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


