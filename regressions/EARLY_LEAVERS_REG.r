# Цель: регрессионный анализ датасета early_leavers_from_edu
# Inputs:  final_data/early_leavers_from_edu.xlsx
#          intermediate_data/SCI_matrix_el.xlsx
#          intermediate_data/distance_matrix_el.xlsx
# Outputs: -
# Дата: 2021-03-24




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
early_leavers_from_edu <- read_excel("final_data/early_leavers_from_edu.xlsx") %>% 
  select(-c(1))

early_leavers_from_edu$country <- early_leavers_from_edu$NUTS_ID %>% str_sub(1, 2)

early_leavers_from_edu <- pdata.frame(early_leavers_from_edu, index=c("NUTS_ID"), drop.index=TRUE, row.names=TRUE)

# загрузим пространственные матрицы
SCI_matrix_el <- xlsx_matrix_to_R("intermediate_data/SCI_matrix_el.xlsx")
SCI_W <- SCI_matrix_el / apply(SCI_matrix_el, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные расстояниям
distance_matrix_el <- xlsx_matrix_to_R("intermediate_data/distance_matrix_el.xlsx")
distance_matrix_el <- 1 / distance_matrix_el # величины, обратные расстояниям
for(i in 1:dim(distance_matrix_el)[1]) { # нули вместо бесконечностей на главной диагонали
  distance_matrix_el[i, i] <- 0
}
distance_W <- distance_matrix_el / apply(distance_matrix_el, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные квадратам расстояний
sq_distance_matrix_el <- xlsx_matrix_to_R("intermediate_data/distance_matrix_el.xlsx")
sq_distance_matrix_el <- 1 / (sq_distance_matrix_el)^2
for(i in 1:dim(sq_distance_matrix_el)[1]) {
  sq_distance_matrix_el[i, i] <- 0
}
sq_distance_W <- sq_distance_matrix_el / apply(sq_distance_matrix_el, 1, sum)




##################################################################################################
############################################ МОДЕЛИ ##############################################

# две спецификации с точки зрения регрессоров
formul_long <- early_leavers ~ EU_friends_abroad +
  ED3_4 + ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + unemp_rate + income_thous + 
  B + C + D + E + G + H + I + J + L + M + N +
  ...2 + ...3 + ...4 + ...5 + ...6 + ...7 + ...8 + ...9 + ...10 + ...11 + ...12 + ...13 +
  ...14 + ...15 + ...16 + ...17 + ...18

formul_short <- early_leavers ~ EU_friends_abroad +
  ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + income_thous + 
  E + G + L +
  ...2 + ...3 + ...4 + ...5 + ...6 + ...7 + ...8 + ...9 + ...10 + ...11 + ...12 + ...13 +
  ...14 + ...15 + ...16 + ...17 + ...18


# тесты на пространственную автокорреляцию
moran.mc(early_leavers_from_edu$early_leavers, mat2listw(SCI_W), nsim = 1000)
moran.mc(early_leavers_from_edu$early_leavers, mat2listw(distance_W), nsim = 1000)
moran.mc(early_leavers_from_edu$early_leavers, mat2listw(sq_distance_W), nsim = 1000)


# OLS-регрессии для сравнения спецификаций
OLS_long <- lm(formul_long, data = early_leavers_from_edu)
summary(OLS_long)

OLS_short <- lm(formul_short, data = early_leavers_from_edu)
summary(OLS_short)


# спецификационные тесты
lm.LMtests(OLS_long, listw = mat2listw(SCI_W), test = "all")
lm.LMtests(OLS_long, listw = mat2listw(distance_W), test = "all")
lm.LMtests(OLS_long, listw = mat2listw(sq_distance_W), test = "all")

lm.LMtests(OLS_short, listw = mat2listw(SCI_W), test = "all")
lm.LMtests(OLS_short, listw = mat2listw(distance_W), test = "all")
lm.LMtests(OLS_short, listw = mat2listw(sq_distance_W), test = "all")



