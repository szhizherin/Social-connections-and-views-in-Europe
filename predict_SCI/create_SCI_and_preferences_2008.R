# Цель: создать наборы данных SCI_and_preferences_2008
# Inputs:  raw_data/ZA7503_v2-0-0.dta
#          raw_data/gadm1_nuts2_gadm1_nuts2_Aug2020.tsv
# Outputs: predict_SCI/SCI_and_preferences_2008.csv
#          predict_SCI/SCI_and_sum_preferences_2008.csv
#          predict_SCI/SCI_and_left_preferences_2008.csv
# Дата: 2021-09-05




library(haven)
library(tidyverse)
library(readxl)
library(readr)
library(data.table)


# данные о предпочтениях
evs_data <- read_dta("raw_data/ZA7503_v2-0-0.dta")

EU_preferences <- evs_data %>% 
  filter(S002EVS == 4) %>% 
  rename(NUTS = "X048b_n2")


for (name in colnames(EU_preferences)) {
  tt <- EU_preferences[paste(name)]
  tt[tt == -1] <- NA
  tt[tt == -2] <- NA
  tt[tt == -3] <- NA
  tt[tt == -4] <- NA
  tt[tt == -5] <- NA
  EU_preferences[paste(name)] <- tt
}


EU_preferences <- EU_preferences[colSums(EU_preferences %>% is.na()) < 6628] # 10%
EU_preferences <- EU_preferences[, c(34:55,
                                     87:120,
                                     122:154,
                                     157:201,
                                     204,
                                     211:244,
                                     250:257,
                                     282)]

# сгруппируем по NUTS2 регионам
gr_mean <- function(x) mean(x, na.rm = TRUE)

EU_preferences_grouped <- EU_preferences %>% 
  group_by(NUTS) %>% 
  mutate_all(gr_mean) %>% 
  ungroup() %>% 
  distinct()

EU_preferences_grouped <- EU_preferences_grouped %>% 
  filter(!is.na(NUTS)) %>% 
  replace(is.na(.), 0)

NUTS_IDs <- EU_preferences_grouped$NUTS


# создадим разности предпочтений для пар регионов
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

for (name in colnames(EU_preferences_grouped)[1:dim(EU_preferences_grouped)[2]-1]) {
  preferences_and_SCI[, paste(name)] <- 0
}


for (i in 1:dim(preferences_and_SCI)[1]) {
  u_loc <- preferences_and_SCI$user_loc[i]
  f_loc <- preferences_and_SCI$fr_loc[i]
  
  preferences_and_SCI[i, 3:dim(preferences_and_SCI)[2]] <- 
    abs(
      (EU_preferences_grouped %>% filter(NUTS == paste(u_loc)))[1, 1:dim(EU_preferences_grouped)[2]-1] -
      (EU_preferences_grouped %>% filter(NUTS == paste(f_loc)))[1, 1:dim(EU_preferences_grouped)[2]-1]
    )
  print(i)
}


# данные по SCI
raw_SCI <- read.table("raw_data/gadm1_nuts2_gadm1_nuts2_Aug2020.tsv", sep = '\t', header = TRUE)


# итоговый набор данных
SCI_and_preferences <- preferences_and_SCI %>%
  inner_join(raw_SCI, by = c("user_loc" = "user_loc", "fr_loc" = "fr_loc"))


SCI_and_preferences <- SCI_and_preferences %>% 
  distinct(across(colnames(SCI_and_preferences)[3:dim(SCI_and_preferences)[2]]), .keep_all = TRUE)


# сохраним результат
write.csv(SCI_and_preferences, "predict_SCI/SCI_and_preferences_2008.csv")


##################################################################################################


# создадим суммы предпочтений для пар регионов
preferences_and_SCI <- CJ(user_loc = NUTS_IDs, fr_loc = NUTS_IDs) %>% 
  filter(user_loc != fr_loc)

preferences_and_SCI <- preferences_and_SCI[leave,]

for (name in colnames(EU_preferences_grouped)[1:dim(EU_preferences_grouped)[2]-1]) {
  preferences_and_SCI[, paste(name)] <- 0
}


for (i in 1:dim(preferences_and_SCI)[1]) {
  u_loc <- preferences_and_SCI$user_loc[i]
  f_loc <- preferences_and_SCI$fr_loc[i]
  
  preferences_and_SCI[i, 3:dim(preferences_and_SCI)[2]] <- 
    (EU_preferences_grouped %>% filter(NUTS == paste(u_loc)))[1, 1:dim(EU_preferences_grouped)[2]-1] +
    (EU_preferences_grouped %>% filter(NUTS == paste(f_loc)))[1, 1:dim(EU_preferences_grouped)[2]-1]
  print(i)
}


# итоговый набор данных
SCI_and_preferences <- preferences_and_SCI %>%
  inner_join(raw_SCI, by = c("user_loc" = "user_loc", "fr_loc" = "fr_loc"))


SCI_and_preferences <- SCI_and_preferences %>% 
  distinct(across(colnames(SCI_and_preferences)[3:dim(SCI_and_preferences)[2]]), .keep_all = TRUE)


# сохраним результат
write.csv(SCI_and_preferences, "predict_SCI/SCI_and_sum_preferences_2008.csv")


##################################################################################################


# выберем значение предпочтения левого региона для каждой пары
preferences_and_SCI <- CJ(user_loc = NUTS_IDs, fr_loc = NUTS_IDs) %>% 
  filter(user_loc != fr_loc)

preferences_and_SCI <- preferences_and_SCI[leave,]

for (name in colnames(EU_preferences_grouped)[1:dim(EU_preferences_grouped)[2]-1]) {
  preferences_and_SCI[, paste(name)] <- 0
}


for (i in 1:dim(preferences_and_SCI)[1]) {
  u_loc <- preferences_and_SCI$user_loc[i]
  f_loc <- preferences_and_SCI$fr_loc[i]
  
  preferences_and_SCI[i, 3:dim(preferences_and_SCI)[2]] <- 
    (EU_preferences_grouped %>% filter(NUTS == paste(u_loc)))[1, 1:dim(EU_preferences_grouped)[2]-1]
  print(i)
}


# итоговый набор данных
SCI_and_preferences <- preferences_and_SCI %>%
  inner_join(raw_SCI, by = c("user_loc" = "user_loc", "fr_loc" = "fr_loc"))


# сохраним результат
write.csv(SCI_and_preferences, "predict_SCI/SCI_and_left_preferences_2008.csv")

