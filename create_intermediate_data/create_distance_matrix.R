# Цель: создать пространственные матрицы на основе расстояний между центроидами регионов
# Inputs:  borrowed_raw_data/geo_distance_dat.csv
#          final_data/trust_in_EU.xlsx
#          final_data/early_leavers_from_edu.xlsx
#          final_data/world_values_survey.xlsx
#          final_data/anti_EU_votes.xlsx
#          final_data/primary_secondary_participation.xlsx
#          intermediate_data/evs_EU_data_2008.xlsx
# Outputs: intermediate_data/distance_matrix.xlsx
#          intermediate_data/distance_matrix_el.xlsx
#          intermediate_data/distance_matrix_wvs.xlsx
#          intermediate_data/distance_matrix_anti.xlsx
#          intermediate_data/distance_matrix_ps.xlsx
#          intermediate_data/distance_matrix_evs_2008.xlsx
# Дата: 2021-09-03




library(plyr)
library(haven)
library(tidyverse)
library(xlsx)
library(readr)
library(readxl)


# функция для выбора NUTS2 тех регионов, которые NUTS1 в eurobarometer_regress_dat
is.NUTS1_subregion <- function(region, NUTS1) {
  subset_vec <- c()
  for (i in 1:length(region)) {
    flag <- FALSE
    for (j in 1:length(NUTS1)) {
      if (startsWith(region[i], NUTS1[j])) { # NUTS1 и region - векторы
        flag <- TRUE
      }
      subset_vec[i] <- flag
    }
  }
  return(subset_vec)
}


eurobarometer_regress_dat <- read_excel("final_data/trust_in_EU.xlsx") %>% 
  select(-1)


# данные для пространственной матрицы на основе расстояний
raw_distance <- read_csv("borrowed_raw_data/geo_distance_dat.csv")


# все используемые коды NUTS2
NUTS2_IDs <- eurobarometer_regress_dat %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- eurobarometer_regress_dat %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться несколько минут)
for_distance_matrix <- raw_distance %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_distance_matrix_grouped <- for_distance_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_distance_matrix_grouped)[1]) {
  if (is.element(str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$user_loc[i] <- str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$fr_loc[i] <- str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по distance
for_distance_matrix_grouped <- ddply(for_distance_matrix_grouped, .(user_loc, fr_loc), summarise, distance = mean(distance))


#наконец, матрица
distance_matrix <- tapply(for_distance_matrix_grouped$distance, for_distance_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(distance_matrix)[1]) {
  distance_matrix[i, i] <- 0
}
View(distance_matrix)


# сохраним результат
write.xlsx(distance_matrix, file = "intermediate_data/distance_matrix.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
early_leavers_from_edu <- read_excel("final_data/early_leavers_from_edu.xlsx") %>% 
  select(-c(1))


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs <- early_leavers_from_edu %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs <- early_leavers_from_edu %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться несколько минут)
for_distance_matrix <- raw_distance %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_distance_matrix_grouped <- for_distance_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_distance_matrix_grouped)[1]) {
  if (is.element(str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$user_loc[i] <- str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$fr_loc[i] <- str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по distance
for_distance_matrix_grouped <- ddply(for_distance_matrix_grouped, .(user_loc, fr_loc), summarise, distance = mean(distance))


#наконец, матрица
distance_matrix_el <- tapply(for_distance_matrix_grouped$distance, for_distance_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(distance_matrix_el)[1]) {
  distance_matrix_el[i, i] <- 0
}
View(distance_matrix_el)


# сохраним результат
write.xlsx(distance_matrix_el, file = "intermediate_data/distance_matrix_el.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
world_values_survey <- read_excel("final_data/world_values_survey.xlsx") %>% 
  select(-c(1))


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs <- world_values_survey %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs <- world_values_survey %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться несколько минут)
for_distance_matrix <- raw_distance %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_distance_matrix_grouped <- for_distance_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_distance_matrix_grouped)[1]) {
  if (is.element(str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$user_loc[i] <- str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$fr_loc[i] <- str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по distance
for_distance_matrix_grouped <- ddply(for_distance_matrix_grouped, .(user_loc, fr_loc), summarise, distance = mean(distance))


#наконец, матрица
distance_matrix_wvs <- tapply(for_distance_matrix_grouped$distance, for_distance_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(distance_matrix_wvs)[1]) {
  distance_matrix_wvs[i, i] <- 0
}
View(distance_matrix_wvs)


# сохраним результат
write.xlsx(distance_matrix_wvs, file = "intermediate_data/distance_matrix_wvs.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
anti_EU_votes <- read_excel("final_data/anti_EU_votes.xlsx") %>% 
  select(-c(1))


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs <- anti_EU_votes %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs <- anti_EU_votes %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться несколько минут)
for_distance_matrix <- raw_distance %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_distance_matrix_grouped <- for_distance_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_distance_matrix_grouped)[1]) {
  if (is.element(str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$user_loc[i] <- str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$fr_loc[i] <- str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по distance
for_distance_matrix_grouped <- ddply(for_distance_matrix_grouped, .(user_loc, fr_loc), summarise, distance = mean(distance))


#наконец, матрица
distance_matrix_anti <- tapply(for_distance_matrix_grouped$distance, for_distance_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(distance_matrix_anti)[1]) {
  distance_matrix_anti[i, i] <- 0
}
View(distance_matrix_anti)


# сохраним результат
write.xlsx(distance_matrix_anti, file = "intermediate_data/distance_matrix_anti.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
primary_secondary_participation <- read_excel("final_data/primary_secondary_participation.xlsx") %>% 
  select(-c(1))


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs <- primary_secondary_participation %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs <- primary_secondary_participation %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться несколько минут)
for_distance_matrix <- raw_distance %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_distance_matrix_grouped <- for_distance_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_distance_matrix_grouped)[1]) {
  if (is.element(str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$user_loc[i] <- str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$fr_loc[i] <- str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по distance
for_distance_matrix_grouped <- ddply(for_distance_matrix_grouped, .(user_loc, fr_loc), summarise, distance = mean(distance))


#наконец, матрица
distance_matrix_ps <- tapply(for_distance_matrix_grouped$distance, for_distance_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(distance_matrix_ps)[1]) {
  distance_matrix_ps[i, i] <- 0
}
View(distance_matrix_ps)


# сохраним результат
write.xlsx(distance_matrix_ps, file = "intermediate_data/distance_matrix_ps.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
european_values_survey <- read_excel("intermediate_data/evs_EU_data_2008.xlsx") %>% 
  select(-c(1)) %>% rename(NUTS_ID = "X048b_n2")


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs <- european_values_survey %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs <- european_values_survey %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться несколько минут)
for_distance_matrix <- raw_distance %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_distance_matrix_grouped <- for_distance_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_distance_matrix_grouped)[1]) {
  if (is.element(str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$user_loc[i] <- str_sub(for_distance_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_distance_matrix_grouped$fr_loc[i] <- str_sub(for_distance_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по distance
for_distance_matrix_grouped <- for_distance_matrix_grouped %>% 
  group_by(user_loc) %>% 
  group_by(fr_loc, .add = TRUE) %>% 
  summarise(user_loc = user_loc[1], fr_loc = fr_loc[1], distance = mean(distance)) %>% 
  ungroup()

#наконец, матрица
distance_matrix_evs <- tapply(for_distance_matrix_grouped$distance, for_distance_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(distance_matrix_evs)[1]) {
  distance_matrix_evs[i, i] <- 0
}
View(distance_matrix_evs)


# сохраним результат
write.xlsx(distance_matrix_evs, file = "intermediate_data/distance_matrix_evs_2008.xlsx")

