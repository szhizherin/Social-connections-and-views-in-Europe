# Цель: предварительный регрессионный анализ датасета world_values_survey.xlsx
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
formul_long <- Homo_neighbours ~ EU_friends_abroad +
  ED3_4 + ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + unemp_rate + income_thous + 
  B + C + D + E + G + H + I + J + L + M + N + 
  ...38 + ...39 + ...40 +  ...41 + ...42 + ...43 + ...44 + ...45 + ...46 + ...47 + ...48 + ...49 +
  ...50 + ...51 + Health +
  Member_control2 + Member_control3 + Member_control4 + Member_control5 + Member_control6 +
  Member_control7 + Member_control8 + Member_control9 + Member_control11

formul_short <- Homo_neighbours ~ EU_friends_abroad +
  ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + income_thous + 
  E + G + L + 
  ...38 + ...39 + ...40 +  ...41 + ...42 + ...43 + ...44 + ...45 + ...46 + ...47 + ...48 + ...49 +
  ...50 + ...51 + Health +
  Member_control2 + Member_control4 + Member_control7


# тесты на пространственную автокорреляцию
moran.mc(world_values_survey$Homo_neighbours, mat2listw(SCI_W), nsim = 1000)
moran.mc(world_values_survey$Homo_neighbours, mat2listw(distance_W), nsim = 1000)
moran.mc(world_values_survey$Homo_neighbours, mat2listw(sq_distance_W), nsim = 1000)


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




# SEM модели с матрицей SCI
SEM_SCI_long <- errorsarlm(formul_long, data = world_values_survey, listw = mat2listw(SCI_W))
summary(SEM_SCI_long, Nagelkerke = TRUE, signif.stars = TRUE)


# SAR модели с матрицей distance
SAR_distance_long <- lagsarlm(formul_long, data = world_values_survey, listw = mat2listw(distance_W))
summary(SAR_distance_long, Nagelkerke = TRUE, signif.stars = TRUE)


# SEM модели с матрицей sq_distance
SEM_sq_distance_long <- errorsarlm(formul_long, data = world_values_survey, listw = mat2listw(sq_distance_W))
summary(SEM_sq_distance_long, Nagelkerke = TRUE, signif.stars = TRUE)

SEM_sq_distance_short <- errorsarlm(formul_short, data = world_values_survey, listw = mat2listw(sq_distance_W))
summary(SEM_sq_distance_short, Nagelkerke = TRUE, signif.stars = TRUE)


Hausman.test(SEM_SCI_long)
Hausman.test(SEM_sq_distance_long)
Hausman.test(SEM_sq_distance_short)


# тестирование лучшей модели на гетероскедастичность
bptest.sarlm(SAR_distance_long)
bptest.sarlm(SEM_sq_distance_long)
bptest.sarlm(SEM_SCI_long)


##################################################################################################
##################################### ДОПОЛНИТЕЛЬНЫЕ ТЕСТЫ #######################################

# создание матрицы для плацебо-теста
placebo <- SCI_W
for (i in 1:dim(placebo)[1]) {
  for (j in 1:dim(placebo)[1]) {
    placebo[i, j] <- runif(1)
  }
}

for (i in 1:dim(placebo)[1]) {
  placebo[i, i] <- 0
}

placebo <- placebo / apply(placebo, 1, sum)


# плацебо-тест
SAR_placebo_long <- lagsarlm(formul_long, data = world_values_survey, listw = mat2listw(placebo))
summary(SAR_placebo_long, Nagelkerke = TRUE, signif.stars = TRUE)

SEM_placebo_long <- errorsarlm(formul_long, data = world_values_survey, listw = mat2listw(placebo))
summary(SEM_placebo_long, Nagelkerke = TRUE, signif.stars = TRUE)

SEM_placebo_short <- errorsarlm(formul_short, data = world_values_survey, listw = mat2listw(placebo))
summary(SEM_placebo_short, Nagelkerke = TRUE, signif.stars = TRUE)




# функции для проверки робастности результатов
subset <- function(data, mat, drop_regions) {
  mat <- mat[-drop_regions, -drop_regions]
  mat <- mat / apply(mat, 1, sum)
  return(list(data[-drop_regions, ], mat))
} 

check_results <- function(data, mat, est_function, est_formula, n_drop_regions, n_iter) {
  res <- c()
  for (i in 1:n_iter) {
    drop <- sample(1:dim(data)[1], n_drop_regions, replace = FALSE, prob = NULL)
    data_mat <- subset(data, mat, drop)
    dd <- data_mat[[1]]
    mm <- data_mat[[2]]
    mod <- est_function(est_formula, data = dd, listw = mat2listw(mm))
    res[i] <- mod$rho
  }
  return(res)
}


# проверка робастности результатов
rob_check_long_distance <- check_results(data = world_values_survey, mat = distance_W, est_function = lagsarlm, 
                                         est_formula = formul_long, n_drop_regions = 10, n_iter = 1000)

# визуализация проверки робастности
mod <- SAR_distance_long
check <- rob_check_long_distance

ggplot(data.frame(check), aes(x = check)) + 
  labs(x = "rho") + 
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept = mod$rho), color="black", size = 1.5) +
  geom_vline(aes(xintercept = mod$rho - mod$rho.se * 1.96), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mod$rho + mod$rho.se * 1.96), color="black", linetype="dashed", size=1)



