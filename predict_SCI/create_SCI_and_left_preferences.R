# Цель: создать SCI_and_sum_preferences.csv
# Inputs:  raw_data/EU_preferences_renamed.xlsx
#          raw_data/gadm1_nuts2_gadm1_nuts2_Aug2020.tsv
# Outputs: predict_SCI/SCI_and_left_preferences.csv
# Дата: 2021-09-05




library(haven)
library(tidyverse)
library(readxl)
library(readr)
library(data.table)


# данные о предпочтениях
EU_preferences <- read_excel("raw_data/EU_preferences_renamed.xlsx")


# сгруппируем по NUTS2 регионам
gr_mean <- function(x) mean(x, na.rm = TRUE)

EU_preferences_grouped <- EU_preferences %>% 
  group_by(NUTS) %>% 
  mutate_all(gr_mean) %>% 
  ungroup() %>% 
  distinct()

EU_preferences_grouped <- EU_preferences_grouped %>% replace(is.na(.), 0)
NUTS_IDs <- EU_preferences_grouped$NUTS


# для всех уникальных пар регионов выберем предпочтения левого региона
preferences_and_SCI <- CJ(user_loc = NUTS_IDs, fr_loc = NUTS_IDs) %>% 
  filter(user_loc != fr_loc)

seen <- list()
leave <- c()
for (i in 1:dim(preferences_and_SCI)[1]) {
  curr = c(preferences_and_SCI$user_loc[i], preferences_and_SCI$fr_loc[i])
  if (!(list(curr) %in% seen)) {
    len <- length(leave)
    leave[len + 1] <- i
    len <- length(seen)
    seen[[len + 1]] <- c(preferences_and_SCI$fr_loc[i], preferences_and_SCI$user_loc[i])
  }
  print(i)
}
preferences_and_SCI <- preferences_and_SCI[leave,]

for (name in colnames(EU_preferences_grouped)[2:dim(EU_preferences_grouped)[2]]) {
  preferences_and_SCI[, paste(name)] <- 0
}


# предпочтения левого региона из каждой пары
for (i in 1:dim(preferences_and_SCI)[1]) {
  u_loc <- preferences_and_SCI$user_loc[i]
  f_loc <- preferences_and_SCI$fr_loc[i]
  
  preferences_and_SCI[i, 3:dim(preferences_and_SCI)[2]] <- 
    (EU_preferences_grouped %>% filter(NUTS == paste(u_loc)))[1, 2:dim(EU_preferences_grouped)[2]]
  print(i)
}


# данные по SCI
raw_SCI <- read.table("raw_data/gadm1_nuts2_gadm1_nuts2_Aug2020.tsv", sep = '\t', header = TRUE)


# итоговый набор данных
SCI_and_preferences <- preferences_and_SCI %>%
  inner_join(raw_SCI, by = c("user_loc" = "user_loc", "fr_loc" = "fr_loc"))


SCI_and_left_preferences <- SCI_and_preferences


# сохраним результат
write.csv(SCI_and_left_preferences, "predict_SCI/SCI_and_left_preferences.csv")

