# Цель: регрессионный анализ датасета world_values_survey.xlsx
# Inputs: final_data/world_values_survey.xlsx 
#         intermediate_data/SCI_matrix_wvs.xlsx
#         intermediate_data/distance_matrix_wvs.xlsx
# Outputs: -
# Дата: 2021-03-25




library(haven)
library(tidyverse)
library(plm)
library(splm)
library(readxl)




# функция для загрузки матрицы W из Excel
xlsx_matrix_to_R <- function(path) {
  W <- read_excel(path) %>% as.data.frame()
  row.names(W) <- W[, 1]
  W[, 1] <- NULL
  W <- as.matrix(W)
  return(W)
}



##################################################################################################
########################################  ЗАГРУЗКА ДАННЫХ  #######################################


# данные для регрессий
world_values_survey <- read_excel("final_data/world_values_survey.xlsx") %>% 
  select(-c(1))

world_values_survey <- pdata.frame(world_values_survey, index=c("NUTS_ID"), drop.index=TRUE, row.names=TRUE)

# загрузим пространственные матрицы
SCI_matrix_wvs <- xlsx_matrix_to_R("intermediate_data/SCI_matrix_wvs.xlsx")
SCI_W <- SCI_matrix_wvs / apply(SCI_matrix_wvs, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные расстояниям
distance_matrix_wvs <- xlsx_matrix_to_R("intermediate_data/distance_matrix_wvs.xlsx")
distance_matrix_wvs <- 1 / distance_matrix_wvs # величины, обратные расстояниям
for(i in 1:dim(distance_matrix_wvs)[1]) { # нули вместо бесконечностей на главной диагонали
  distance_matrix_wvs[i, i] <- 0
}
distance_W <- distance_matrix_wvs / apply(distance_matrix_wvs, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные квадратам расстояний
sq_distance_matrix_wvs <- xlsx_matrix_to_R("intermediate_data/distance_matrix_wvs.xlsx")
sq_distance_matrix_wvs <- 1 / (sq_distance_matrix_wvs)^2
for(i in 1:dim(sq_distance_matrix_wvs)[1]) {
  sq_distance_matrix_wvs[i, i] <- 0
}
sq_distance_W <- sq_distance_matrix_wvs / apply(sq_distance_matrix_wvs, 1, sum)




##################################################################################################
############################################ МОДЕЛИ ##############################################

# две спецификации с точки зрения регрессоров
formul_long <- Friends ~ EU_friends_abroad +
  ED3_4 + ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + unemp_rate + income_thous + 
  B + C + D + E + G + H + I + J + L + M + N + 
  ...38 + ...39 + ...40 +  ...41 + ...42 + ...43 + ...44 + ...45 + ...46 + ...47 + ...48 + ...49 +
  ...50 + ...51 + Health +
  Member_control2 + Member_control3 + Member_control4 + Member_control5 + Member_control6 +
  Member_control7 + Member_control8 + Member_control9 + Member_control11

formul_short <- Friends ~ EU_friends_abroad +
  ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + income_thous + 
  E + G + L + 
  ...38 + ...39 + ...40 +  ...41 + ...42 + ...43 + ...44 + ...45 + ...46 + ...47 + ...48 + ...49 +
  ...50 + ...51 + Health +
  Member_control2 + Member_control4 + Member_control7


# тесты на пространственную автокорреляцию
moran.mc(world_values_survey$Friends, mat2listw(SCI_W), nsim = 1000)
moran.mc(world_values_survey$Friends, mat2listw(distance_W), nsim = 1000)
moran.mc(world_values_survey$Friends, mat2listw(sq_distance_W), nsim = 1000)


# OLS-регрессии для сравнения спецификаций
OLS_long <- lm(formul_long, data = world_values_survey)
summary(OLS_long)

OLS_short <- lm(formul_short, data = world_values_survey)
summary(OLS_short)


# спецификационные тесты
lm.LMtests(OLS_long, listw = mat2listw(SCI_W), test = "all")
lm.LMtests(OLS_long, listw = mat2listw(distance_W), test = "all")
lm.LMtests(OLS_long, listw = mat2listw(sq_distance_W), test = "all")

lm.LMtests(OLS_short, listw = mat2listw(SCI_W), test = "all")
lm.LMtests(OLS_short, listw = mat2listw(distance_W), test = "all")
lm.LMtests(OLS_short, listw = mat2listw(sq_distance_W), test = "all")



