# Цель: создать пространственные матрицы на основе SCI
# Inputs:  raw_data/gadm1_nuts2_gadm1_nuts2_Aug2020.tsv
#          final_data/trust_in_EU.xlsx
#          final_data/early_leavers_from_edu.xlsx
#          final_data/world_values_survey.xlsx
#          final_data/anti_EU_votes.xlsx
#          final_data/primary_secondary_participation.xlsx
# Outputs: intermediate_data/SCI_matrix.xlsx
#          intermediate_data/SCI_matrix_el.xlsx
#          intermediate_data/SCI_matrix_wvs.xlsx
#          intermediate_data/SCI_matrix_anti.xlsx
#          intermediate_data/SCI_matrix_ps.xlsx
# Дата: 2021-03-18




library(haven)
library(tidyverse)
library(xlsx)
library(readxl)


# функция для выбора NUTS2 тех регионов, которые NUTS1 в наборах данных
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


# данные для пространственной матрицы на основе SCI
raw_SCI <- read.table("raw_data/gadm1_nuts2_gadm1_nuts2_Aug2020.tsv", sep = '\t', header = TRUE)


eurobarometer_regress_dat <- read_excel("final_data/trust_in_EU.xlsx") %>% 
  select(-1)


# все используемые коды NUTS2
NUTS2_IDs <- eurobarometer_regress_dat %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- eurobarometer_regress_dat %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться около 15-20 минут)
for_SCI_matrix <- raw_SCI %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# данные для взвешивания SCI по населению
EU_regions_population_2019 <- read_excel("raw_data/EU_regions_population_2019.xlsx", 
                                         sheet = "Sheet 1", col_types = c("text", 
                                                                          "numeric", "skip", "skip", "skip"), 
                                         skip = 10) %>% 
  rename(NUTS = "GEO (Codes)", population = ...2)


# выберем строки, содержащие все нужные коды и создадим популяционные веса
EU_regions_population_2019 <- EU_regions_population_2019 %>% 
  filter(is.element(NUTS, NUTS2_IDs[[1]]) | (is.NUTS1_subregion(NUTS, NUTS1_IDs[[1]]) & !is.element(NUTS, NUTS1_IDs[[1]])))

EU_regions_population_2019$group <- EU_regions_population_2019$NUTS %>% str_sub(1, 3)

EU_regions_population_2019 <- EU_regions_population_2019 %>% 
  group_by(group) %>% 
  summarise(NUTS = NUTS, population_share = population / sum(population)) %>% 
  ungroup()

for (i in 1:dim(EU_regions_population_2019)[1]) {
  if (!is.element(EU_regions_population_2019$group[i], NUTS1_IDs[[1]])) {
    EU_regions_population_2019$population_share[i] <- 1
  }
}


# назначим популяционные веса парам регионов
for_SCI_matrix$population_weight <- NA
for (i in 1:dim(for_SCI_matrix)[1]) {
  u_loc <- for_SCI_matrix$user_loc[i]
  f_loc <- for_SCI_matrix$fr_loc[i]
  for_SCI_matrix$population_weight[i] <- 
    (EU_regions_population_2019 %>% filter(NUTS == u_loc))$population_share * 
    (EU_regions_population_2019 %>% filter(NUTS == f_loc))$population_share
}

for_SCI_matrix$weighted_sci <- for_SCI_matrix$scaled_sci * for_SCI_matrix$population_weight


# сгруппируем нужные NUTS2 регионы в NUTS1
for_SCI_matrix_grouped <- for_SCI_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_SCI_matrix_grouped)[1]) {
  if (is.element(str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$user_loc[i] <- str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$fr_loc[i] <- str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3)
  }
}


# произведём корректную группировку по NUTS1 регионам
for_SCI_matrix_grouped <- for_SCI_matrix_grouped %>% group_by(user_loc, fr_loc) %>% 
  summarise(user_loc = user_loc[1], fr_loc = fr_loc[1], sci = sum(weighted_sci)) %>% 
  ungroup()


# наконец, матрица
SCI_matrix <- tapply(for_SCI_matrix_grouped$sci, for_SCI_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(SCI_matrix)[1]) {
  SCI_matrix[i, i] <- 0
}
View(SCI_matrix)


# сохраним результат
write.xlsx(SCI_matrix, file = "intermediate_data/SCI_matrix.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
early_leavers_from_edu <- read_excel("final_data/early_leavers_from_edu.xlsx", 
                                     col_types = c("skip", "text", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric", 
                                                   "numeric", "numeric", "numeric"))


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs <- early_leavers_from_edu %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs <- early_leavers_from_edu %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды (может выполняться около 15-20 минут)
for_SCI_matrix <- raw_SCI %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_SCI_matrix_grouped <- for_SCI_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_SCI_matrix_grouped)[1]) {
  if (is.element(str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$user_loc[i] <- str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$fr_loc[i] <- str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по sci_scaled
for_SCI_matrix_grouped <- ddply(for_SCI_matrix_grouped, .(user_loc, fr_loc), summarise, sci = sum(scaled_sci))


# наконец, матрица
SCI_matrix_el <- tapply(for_SCI_matrix_grouped$sci, for_SCI_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(SCI_matrix_el)[1]) {
  SCI_matrix_el[i, i] <- 0
}
View(SCI_matrix_el)


# сохраним результат
write.xlsx(SCI_matrix_el, file = "intermediate_data/SCI_matrix_el.xlsx")


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


# выберем строки, содержащие все нужные коды (может выполняться около 15-20 минут)
for_SCI_matrix <- raw_SCI %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# данные для взвешивания SCI по населению
EU_regions_population_2019 <- read_excel("raw_data/EU_regions_population_2019.xlsx", 
                                         sheet = "Sheet 1", col_types = c("text", 
                                                                          "numeric", "skip", "skip", "skip"), 
                                         skip = 10) %>% 
  rename(NUTS = "GEO (Codes)", population = ...2)


# выберем строки, содержащие все нужные коды и создадим популяционные веса
EU_regions_population_2019 <- EU_regions_population_2019 %>% 
  filter(is.element(NUTS, NUTS2_IDs[[1]]) | (is.NUTS1_subregion(NUTS, NUTS1_IDs[[1]]) & !is.element(NUTS, NUTS1_IDs[[1]])))

EU_regions_population_2019$group <- EU_regions_population_2019$NUTS %>% str_sub(1, 3)

EU_regions_population_2019 <- EU_regions_population_2019 %>% 
  group_by(group) %>% 
  summarise(NUTS = NUTS, population_share = population / sum(population)) %>% 
  ungroup()

for (i in 1:dim(EU_regions_population_2019)[1]) {
  if (!is.element(EU_regions_population_2019$group[i], NUTS1_IDs[[1]])) {
    EU_regions_population_2019$population_share[i] <- 1
  }
}


# назначим популяционные веса парам регионов
for_SCI_matrix$population_weight <- NA
for (i in 1:dim(for_SCI_matrix)[1]) {
  u_loc <- for_SCI_matrix$user_loc[i]
  f_loc <- for_SCI_matrix$fr_loc[i]
  for_SCI_matrix$population_weight[i] <- 
    (EU_regions_population_2019 %>% filter(NUTS == u_loc))$population_share * 
    (EU_regions_population_2019 %>% filter(NUTS == f_loc))$population_share
}

for_SCI_matrix$weighted_sci <- for_SCI_matrix$scaled_sci * for_SCI_matrix$population_weight


# сгруппируем нужные NUTS2 регионы в NUTS1
for_SCI_matrix_grouped <- for_SCI_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_SCI_matrix_grouped)[1]) {
  if (is.element(str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$user_loc[i] <- str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$fr_loc[i] <- str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3)
  }
}


# произведём корректную группировку по NUTS1 регионам
for_SCI_matrix_grouped <- for_SCI_matrix_grouped %>% group_by(user_loc, fr_loc) %>% 
  summarise(user_loc = user_loc[1], fr_loc = fr_loc[1], sci = sum(weighted_sci)) %>% 
  ungroup()


# наконец, матрица
SCI_matrix <- tapply(for_SCI_matrix_grouped$sci, for_SCI_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(SCI_matrix)[1]) {
  SCI_matrix[i, i] <- 0
}
View(SCI_matrix)


# сохраним результат
write.xlsx(SCI_matrix, file = "intermediate_data/SCI_matrix_wvs.xlsx")


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


# выберем строки, содержащие все нужные коды (может выполняться около 15-20 минут)
for_SCI_matrix <- raw_SCI %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# данные для взвешивания SCI по населению
EU_regions_population_2019 <- read_excel("raw_data/EU_regions_population_2019.xlsx", 
                                         sheet = "Sheet 1", col_types = c("text", 
                                                                          "numeric", "skip", "skip", "skip"), 
                                         skip = 10) %>% 
  rename(NUTS = "GEO (Codes)", population = ...2)


# выберем строки, содержащие все нужные коды и создадим популяционные веса
EU_regions_population_2019 <- EU_regions_population_2019 %>% 
  filter(is.element(NUTS, NUTS2_IDs[[1]]) | (is.NUTS1_subregion(NUTS, NUTS1_IDs[[1]]) & !is.element(NUTS, NUTS1_IDs[[1]])))

EU_regions_population_2019$group <- EU_regions_population_2019$NUTS %>% str_sub(1, 3)

EU_regions_population_2019 <- EU_regions_population_2019 %>% 
  group_by(group) %>% 
  summarise(NUTS = NUTS, population_share = population / sum(population)) %>% 
  ungroup()

for (i in 1:dim(EU_regions_population_2019)[1]) {
  if (!is.element(EU_regions_population_2019$group[i], NUTS1_IDs[[1]])) {
    EU_regions_population_2019$population_share[i] <- 1
  }
}


# назначим популяционные веса парам регионов
for_SCI_matrix$population_weight <- NA
for (i in 1:dim(for_SCI_matrix)[1]) {
  u_loc <- for_SCI_matrix$user_loc[i]
  f_loc <- for_SCI_matrix$fr_loc[i]
  for_SCI_matrix$population_weight[i] <- 
    (EU_regions_population_2019 %>% filter(NUTS == u_loc))$population_share * 
    (EU_regions_population_2019 %>% filter(NUTS == f_loc))$population_share
}

for_SCI_matrix$weighted_sci <- for_SCI_matrix$scaled_sci * for_SCI_matrix$population_weight


# сгруппируем нужные NUTS2 регионы в NUTS1
for_SCI_matrix_grouped <- for_SCI_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_SCI_matrix_grouped)[1]) {
  if (is.element(str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$user_loc[i] <- str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$fr_loc[i] <- str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3)
  }
}


# произведём корректную группировку по NUTS1 регионам
for_SCI_matrix_grouped <- for_SCI_matrix_grouped %>% group_by(user_loc, fr_loc) %>% 
  summarise(user_loc = user_loc[1], fr_loc = fr_loc[1], sci = sum(weighted_sci)) %>% 
  ungroup()


# наконец, матрица
SCI_matrix <- tapply(for_SCI_matrix_grouped$sci, for_SCI_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(SCI_matrix)[1]) {
  SCI_matrix[i, i] <- 0
}
View(SCI_matrix)


# сохраним результат
write.xlsx(SCI_matrix, file = "intermediate_data/SCI_matrix_anti.xlsx")


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


# выберем строки, содержащие все нужные коды (может выполняться около 15-20 минут)
for_SCI_matrix <- raw_SCI %>% 
  filter((is.element(user_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(user_loc, NUTS1_IDs[[1]])) & 
           (is.element(fr_loc, NUTS2_IDs[[1]]) | is.NUTS1_subregion(fr_loc, NUTS1_IDs[[1]])))


# сгруппируем нужные NUTS2 регионы в NUTS1
for_SCI_matrix_grouped <- for_SCI_matrix
# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_SCI_matrix_grouped)[1]) {
  if (is.element(str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$user_loc[i] <- str_sub(for_SCI_matrix_grouped$user_loc[i], 1, 3)
  }
  if (is.element(str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3), NUTS1_IDs[[1]])) {
    for_SCI_matrix_grouped$fr_loc[i] <- str_sub(for_SCI_matrix_grouped$fr_loc[i], 1, 3)
  }
}
# просуммируем одинаковые строки по sci_scaled
for_SCI_matrix_grouped <- ddply(for_SCI_matrix_grouped, .(user_loc, fr_loc), summarise, sci = sum(scaled_sci))


# наконец, матрица
SCI_matrix_ps <- tapply(for_SCI_matrix_grouped$sci, for_SCI_matrix_grouped[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(SCI_matrix_ps)[1]) {
  SCI_matrix_ps[i, i] <- 0
}
View(SCI_matrix_ps)


# сохраним результат
write.xlsx(SCI_matrix_ps, file = "intermediate_data/SCI_matrix_ps.xlsx")





