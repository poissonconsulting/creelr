library(plyr)
library(dplyr)
library(tidyr)
library(magrittr)
library(ggplot2)

data(toa_example)

toa_example %<>% gather(key = "Species", value = "Catch", 
                        Catch, KOCatch, CTCatch, RBCatch, BTCatch, LTCatch)

toa_example$Species %<>% sub("(.+)(Catch)", "\\1", .)

toa_example %<>% filter(Species != "Catch")

summary(toa_example)

plot_creel_data(toa_example, by = "Species", holidays = as.Date("2014-05-29"), 
                weekend = c("Friday", "Saturday", "Sunday")) + 
  ylab("Daily Catch") + facet_wrap(~Species)

toa_example %<>% ddply("Species", trad_one_access, weekend = c("Friday", "Saturday", "Sunday"),
                       holidays = as.Date("2014-05-29"))

plot_creel_estimates(toa_example) + facet_wrap(~Species) + ylab("Monthly Catch")

sum_creel_estimates(toa_example, by = "Month")
sum_creel_estimates(toa_example, by = "Species")
