# Цель: регрессионный анализ датасета trust_in_EU.xlsx
# Inputs: final_data/trust_in_EU.xlsx 
#         intermediate_data/SCI_matrix.xlsx
#         intermediate_data/distance_matrix.xlsx
#         intermediate_data/contiguity_matrix.xlsx
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
trust_in_EU <- read_excel("final_data/trust_in_EU.xlsx") %>% 
  select(-c(1))

trust_in_EU$country <- trust_in_EU$NUTS_ID %>% str_sub(1, 2)

trust_in_EU <- pdata.frame(trust_in_EU, index=c("NUTS_ID"), drop.index=TRUE, row.names=TRUE)

# загрузим пространственные матрицы
SCI_matrix <- xlsx_matrix_to_R("intermediate_data/SCI_matrix.xlsx")
SCI_W <- SCI_matrix / apply(SCI_matrix, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные расстояниям
distance_matrix <- xlsx_matrix_to_R("intermediate_data/distance_matrix.xlsx")
distance_matrix <- 1 / distance_matrix # величины, обратные расстояниям
for(i in 1:dim(distance_matrix)[1]) { # нули вместо бесконечностей на главной диагонали
  distance_matrix[i, i] <- 0
}
distance_W <- distance_matrix / apply(distance_matrix, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные квадратам расстояний
sq_distance_matrix <- xlsx_matrix_to_R("intermediate_data/distance_matrix.xlsx")
sq_distance_matrix <- 1 / (sq_distance_matrix)^2
for(i in 1:dim(sq_distance_matrix)[1]) {
  sq_distance_matrix[i, i] <- 0
}
sq_distance_W <- sq_distance_matrix / apply(sq_distance_matrix, 1, sum)

# матрица на основе соседства
contiguity_matrix <- xlsx_matrix_to_R("intermediate_data/contiguity_matrix.xlsx")
contiguity_W <- contiguity_matrix / apply(contiguity_matrix, 1, sum) # нормировка (сумма по строке = 1)
contiguity_W[is.na(contiguity_W)] <- 0 # устраняем последствия деления на ноль


##################################################################################################
############################################ МОДЕЛИ ##############################################

# две спецификации с точки зрения регрессоров
formul_long <- Trust_in_EU ~ EU_friends_abroad +
  ED3_4 + ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + unemp_rate + income_thous + 
  B + C + D + E + G + H + I + J + L + M + N +
  ...2 + ...3 + ...4 + ...5 + ...6 + ...7 + ...8 + ...9 + ...10 + ...11 + ...12 + ...13 +
  ...14 + ...15 + ...16 + ...17 + ...18

formul_short <- Trust_in_EU ~ EU_friends_abroad +
  ED5_8 + 
  Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
  median_age + income_thous + 
  E + G + L +
  ...2 + ...3 + ...4 + ...5 + ...6 + ...7 + ...8 + ...9 + ...10 + ...11 + ...12 + ...13 +
  ...14 + ...15 + ...16 + ...17 + ...18


# тесты на пространственную автокорреляцию
moran.mc(trust_in_EU$Trust_in_EU, mat2listw(SCI_W), nsim = 1000)
moran.mc(trust_in_EU$Trust_in_EU, mat2listw(distance_W), nsim = 1000)
moran.mc(trust_in_EU$Trust_in_EU, mat2listw(sq_distance_W), nsim = 1000)
moran.mc(trust_in_EU$Trust_in_EU, mat2listw(contiguity_W), nsim = 1000, zero.policy = TRUE)


# OLS-регрессии для сравнения спецификаций
OLS_long <- lm(formul_long, data = trust_in_EU)
summary(OLS_long)

OLS_short <- lm(formul_short, data = trust_in_EU)
summary(OLS_short)


# спецификационные тесты
lm.LMtests(OLS_long, listw = mat2listw(SCI_W), test = "all")
lm.LMtests(OLS_long, listw = mat2listw(distance_W), test = "all")
lm.LMtests(OLS_long, listw = mat2listw(sq_distance_W), test = "all")
lm.LMtests(OLS_long, listw = mat2listw(contiguity_W), test = "all", zero.policy = TRUE)

lm.LMtests(OLS_short, listw = mat2listw(SCI_W), test = "all")
lm.LMtests(OLS_short, listw = mat2listw(distance_W), test = "all")
lm.LMtests(OLS_short, listw = mat2listw(sq_distance_W), test = "all")
lm.LMtests(OLS_short, listw = mat2listw(contiguity_W), test = "all", zero.policy = TRUE)


# SAR модели с матрицей SCI
SAR_SCI_long <- lagsarlm(formul_long, data = trust_in_EU, listw = mat2listw(SCI_W))
summary(SAR_SCI_long, Nagelkerke = TRUE, signif.stars = TRUE)

SAR_SCI_short <- lagsarlm(formul_short, data = trust_in_EU, listw = mat2listw(SCI_W))
summary(SAR_SCI_short, Nagelkerke = TRUE, signif.stars = TRUE)


# SAR модели с матрицей sq_distance
SAR_sq_distance_long <- lagsarlm(formul_long, data = trust_in_EU, listw = mat2listw(sq_distance_W))
summary(SAR_sq_distance_long, Nagelkerke = TRUE, signif.stars = TRUE)

SAR_sq_distance_short <- lagsarlm(formul_short, data = trust_in_EU, listw = mat2listw(sq_distance_W))
summary(SAR_sq_distance_short, Nagelkerke = TRUE, signif.stars = TRUE)


# SAR модели с матрицей contiguity
SAR_contiguity_long <- lagsarlm(formul_long, data = trust_in_EU, listw = mat2listw(contiguity_W), zero.policy = TRUE)
summary(SAR_contiguity_long, Nagelkerke = TRUE, signif.stars = TRUE)

SAR_contiguity_short <- lagsarlm(formul_short, data = trust_in_EU, listw = mat2listw(contiguity_W), zero.policy = TRUE)
summary(SAR_contiguity_short, Nagelkerke = TRUE, signif.stars = TRUE)


# прямые и непрямые эффекты
impacts(SAR_SCI_long, listw = mat2listw(SCI_W))
impacts(SAR_SCI_short, listw = mat2listw(SCI_W))


# тестирование лучшей модели на гетероскедастичность
bptest.sarlm(SAR_SCI_long, studentize = TRUE)


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
SAR_placebo_long <- lagsarlm(formul_long, data = trust_in_EU, listw = mat2listw(placebo))
summary(SAR_placebo_long, Nagelkerke = TRUE, signif.stars = TRUE)

SAR_placebo_short <- lagsarlm(formul_short, data = trust_in_EU, listw = mat2listw(placebo))
summary(SAR_placebo_short, Nagelkerke = TRUE, signif.stars = TRUE)




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
rob_check_short <- check_results(data = trust_in_EU, mat = SCI_W, est_function = lagsarlm, 
                                 est_formula = formul_short, n_drop_regions = 10, n_iter = 1000)

rob_check_long <- check_results(data = trust_in_EU, mat = SCI_W, est_function = lagsarlm, 
                                est_formula = formul_long, n_drop_regions = 10, n_iter = 1000)


# визуализация проверки робастности
mod <- SAR_SCI_short
check <- rob_check_short

ggplot(data.frame(check), aes(x = check)) + 
  labs(x = "rho") + 
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept = mod$rho), color="black", size = 1.5) +
  geom_vline(aes(xintercept = mod$rho - mod$rho.se * 1.96), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mod$rho + mod$rho.se * 1.96), color="black", linetype="dashed", size=1)


mod <- SAR_SCI_long
check <- rob_check_long

ggplot(data.frame(check), aes(x = check)) + 
  labs(x = "rho") + 
  geom_histogram(colour="black", fill="white") +
  geom_vline(aes(xintercept = mod$rho), color="black", size = 1.5) +
  geom_vline(aes(xintercept = mod$rho - mod$rho.se * 1.96), color="black", linetype="dashed", size=1) +
  geom_vline(aes(xintercept = mod$rho + mod$rho.se * 1.96), color="black", linetype="dashed", size=1)




