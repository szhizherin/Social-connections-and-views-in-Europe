# Цель: регрессионный анализ датасета wvs_whole.xlsx
# Inputs:  final_data/wvs_whole.xlsx
#          intermediate_data/SCI_matrix_wvs.xlsx
#          intermediate_data/distance_matrix_wvs.xlsx
#          intermediate_data/contiguity_matrix_wvs.xlsx
# Outputs: regressions/WVS_models_summary.xlsx
# Дата: 2021-08-21




library(haven)
library(tidyverse)
library(plm)
library(splm)
library(readxl)
library(xlsx)




# функция, выбирающая лучшую модель для каждой матрицы отдельно, на вход принимает
# OLS-модель с заданной формулой и матрицу (distance_W / sq_distance_W / contiguity_W / SCI_W)
best_model <- function(OLS_model, matrix) {
  
  LM <- lm.LMtests(OLS_model, listw = mat2listw(matrix), test = "all", zero.policy = T)
  
  p_err <- LM$LMerr$p.value
  p_lag <- LM$LMlag$p.value
  
  if (min(p_err, p_lag) > 0.1) {
    return("OLS")
  }
  if (p_err > 0.1) {
    return("SAR")
  }
  if (p_lag > 0.1) {
    return("SEM")
  }
  p_err_R <- LM$RLMerr$p.value
  p_lag_R <- LM$RLMlag$p.value
  
  if (p_err_R < p_lag_R ) {
    return("SEM")
  }
  return("SAR")
}


# функция для загрузки матрицы W из Excel
xlsx_matrix_to_R <- function(path) {
  W <- read_excel(path) %>% as.data.frame()
  row.names(W) <- W[, 1]
  W[, 1] <- NULL
  W <- as.matrix(W)
  return(W)
}


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


# матрица на основе соседства
contiguity_matrix <- xlsx_matrix_to_R("intermediate_data/contiguity_matrix_wvs.xlsx")
contiguity_W <- contiguity_matrix / apply(contiguity_matrix, 1, sum) # нормировка (сумма по строке = 1)
contiguity_W[is.na(contiguity_W)] <- 0 # устраняем последствия деления на ноль


# создаем датафрейм, в котором будем хранить ключевые результаты всех регрессий
WVS_models <- data.frame(
  variable = c(0),
  d = c(0),
  d_sq = c(0),
  cont = c(0),
  SCI = c(0))

# данные для регрессий
wvs_whole <- read_excel("final_data/wvs_whole.xlsx") %>% 
  select(-c(1))

wvs_whole <- pdata.frame(wvs_whole, index=c("NUTS_ID"), drop.index=TRUE, row.names=TRUE)

# выбираем по лучшей модели для каждой матрицы (SAR, SEM или OLS)
# выполнение цикла занимает некоторое время
for (var in colnames(wvs_whole)[1:139]) {
  VAR <- wvs_whole[, paste(var)]
  
  formul_long <- VAR ~ EU_friends_abroad +
    ED3_4 + ED5_8 + 
    Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
    median_age + unemp_rate + income_thous + 
    B + C + D + E + G + H + I + J + L + M + N + 
    ...141 + ...142 + ...143 +  ...144 + ...145 + ...146 + ...147 + ...148 + ...149 + ...150 +
    ...151 + ...152 + ...153 + ...154 + Health +
    Member_control2 + Member_control3 + Member_control4 + Member_control5 + Member_control6 +
    Member_control7 + Member_control8 + Member_control9 + Member_control11
  
  OLS_long <- lm(formul_long, data = wvs_whole)
  
  dist_model <- best_model(OLS_long, distance_W)
  dist_sq_model <- best_model(OLS_long, sq_distance_W)
  cont_model <- best_model(OLS_long, contiguity_W)
  sci_model <- best_model(OLS_long, SCI_W)
  
  WVS_models = rbind(WVS_models, c(var,
                 dist_model,
                 dist_sq_model,
                 cont_model,
                 sci_model))
}

WVS_models <- WVS_models[-c(1),]

WVS_models_summary <- cbind(count(WVS_models, d)$n,
                            count(WVS_models, d_sq)$n,
                            count(WVS_models, cont)$n,
                            count(WVS_models, SCI)$n) %>% as.data.frame()
colnames(WVS_models_summary) <- c("distance", "distance squared",
                                  "contiguity", "SCI")
rownames(WVS_models_summary) <- c("OLS", "SAR", "SEM")
WVS_models_summary <- WVS_models_summary / 139

write.xlsx(WVS_models_summary, file = "regressions/WVS_models_summary.xlsx")

All_OLS <- filter(WVS_models, d == "OLS" & d_sq == "OLS" &
                        cont == "OLS" & SCI == "OLS")
SAR_and_SCI <- filter(WVS_models, SCI == "SAR")