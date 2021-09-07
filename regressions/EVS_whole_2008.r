# Цель: регрессионный анализ датасета evs_2008.xlsx
# Inputs:  final_data/evs_2008.xlsx
#          intermediate_data/SCI_matrix_evs_2008.xlsx
#          intermediate_data/distance_matrix_evs_2008.xlsx
#          intermediate_data/contiguity_matrix_evs_2008.xlsx
# Outputs: regressions/EVS_models_long_summary.xlsx
#          regressions/EVS_models_short_summary.xlsx
# Дата: 2021-09-05




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
SCI_matrix_evs <- xlsx_matrix_to_R("intermediate_data/SCI_matrix_evs_2008.xlsx")
SCI_W <- SCI_matrix_evs / apply(SCI_matrix_evs, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные расстояниям
distance_matrix_evs <- xlsx_matrix_to_R("intermediate_data/distance_matrix_evs_2008.xlsx")
distance_matrix_evs <- 1 / distance_matrix_evs # величины, обратные расстояниям
for(i in 1:dim(distance_matrix_evs)[1]) { # нули вместо бесконечностей на главной диагонали
  distance_matrix_evs[i, i] <- 0
}
distance_W <- distance_matrix_evs / apply(distance_matrix_evs, 1, sum) # нормировка (сумма по строке = 1)

# здесь должны быть величины, обратные квадратам расстояний
sq_distance_matrix_evs <- xlsx_matrix_to_R("intermediate_data/distance_matrix_evs_2008.xlsx")
sq_distance_matrix_evs <- 1 / (sq_distance_matrix_evs)^2
for(i in 1:dim(sq_distance_matrix_evs)[1]) {
  sq_distance_matrix_evs[i, i] <- 0
}
sq_distance_W <- sq_distance_matrix_evs / apply(sq_distance_matrix_evs, 1, sum)


# матрица на основе соседства
contiguity_matrix <- xlsx_matrix_to_R("intermediate_data/contiguity_matrix_evs_2008.xlsx")
contiguity_W <- contiguity_matrix / apply(contiguity_matrix, 1, sum) # нормировка (сумма по строке = 1)
contiguity_W[is.na(contiguity_W)] <- 0 # устраняем последствия деления на ноль


# создаем датафреймы, в которых будем хранить ключевые результаты всех регрессий
EVS_models_long <- data.frame(
  variable = c(0),
  d = c(0),
  d_sq = c(0),
  cont = c(0),
  SCI = c(0))

EVS_models_short <- data.frame(
  variable = c(0),
  d = c(0),
  d_sq = c(0),
  cont = c(0),
  SCI = c(0))

# данные для регрессий
evs_2008 <- read_excel("final_data/evs_2008.xlsx") %>% 
  select(-c(1))

evs_2008 <- pdata.frame(evs_2008, index=c("NUTS_ID"), drop.index=TRUE, row.names=TRUE)

# выбираем по лучшей модели для каждой матрицы (SAR, SEM или OLS) в длинной и короткой постановках
# выполнение цикла занимает некоторое время
for (var in colnames(evs_2008)[1:176]) {
  VAR <- evs_2008[, paste(var)]
  
  formul_long <- VAR ~ EU_friends_abroad +
    ED3_4 + ED5_8 + 
    Y10_19 + Y20_29 + Y30_39 + Y40_49 + Y50_59 + Y60_69 + Y70_MAX + 
    median_age + unemp_rate + income_thous + 
    B + C + D + E + G + H + I + J + L + M + N +
    ...179 + ...180 + ...181 + ...182 + ...183 + ...184 + ...185 + ...186 +
    ...187 + ...188 + ...189 + ...190 + ...191 + ...192 + ...193 + ...194 +
    A064 + A065 + A066 + A067 + A068 + A069 + A070 + A071 + A072 + A073 + A074 + A075 + A076 +
    A077 + A079 + A080
  
  formul_short <- VAR ~ EU_friends_abroad +
    ED3_4 + ED5_8 + 
    Y10_19 + Y20_29 + Y30_39 + Y40_49 + 
    income_thous + 
    manufacturing_emp + construction_emp + professional_emp +
    ...179 + ...180 + ...181 + ...182 + ...183 + ...184 + ...185 + ...186 +
    ...187 + ...188 + ...189 + ...190 + ...191 + ...192 + ...193 + ...194 +
    A064 + A065 + A066 + A067 + A068 + A069 + A070 + A071 + A072 + A073 + A074 + A075 + A076 +
    A077 + A079 + A080
  
  OLS_long <- lm(formul_long, data = evs_2008)
  OLS_short <- lm(formul_short, data = evs_2008)
  
  dist_model_long <- best_model(OLS_long, distance_W)
  dist_sq_model_long <- best_model(OLS_long, sq_distance_W)
  cont_model_long <- best_model(OLS_long, contiguity_W)
  sci_model_long <- best_model(OLS_long, SCI_W)
  
  EVS_models_long = rbind(EVS_models_long, c(var,
                 dist_model_long,
                 dist_sq_model_long,
                 cont_model_long,
                 sci_model_long))
  
  dist_model_short <- best_model(OLS_short, distance_W)
  dist_sq_model_short <- best_model(OLS_short, sq_distance_W)
  cont_model_short <- best_model(OLS_short, contiguity_W)
  sci_model_short <- best_model(OLS_short, SCI_W)
  
  EVS_models_short = rbind(EVS_models_short, c(var,
                                        dist_model_short,
                                        dist_sq_model_short,
                                        cont_model_short,
                                        sci_model_short))
}

EVS_models_long <- EVS_models_long[-c(1),]

EVS_models_long_summary <- cbind(count(EVS_models_long, d)$n,
                            count(EVS_models_long, d_sq)$n,
                            count(EVS_models_long, cont)$n,
                            count(EVS_models_long, SCI)$n) %>% as.data.frame()
colnames(EVS_models_long_summary) <- c("distance", "distance squared",
                                  "contiguity", "SCI")
rownames(EVS_models_long_summary) <- c("OLS", "SAR", "SEM")
EVS_models_long_summary <- EVS_models_long_summary / 176

write.xlsx(EVS_models_long_summary, file = "regressions/EVS_models_long_summary.xlsx")

All_OLS_long <- filter(EVS_models_long, d == "OLS" & d_sq == "OLS" &
                        cont == "OLS" & SCI == "OLS")
SAR_and_SCI_long <- filter(EVS_models_long, SCI == "SAR")


EVS_models_short <- EVS_models_short[-c(1),]

EVS_models_short_summary <- cbind(count(EVS_models_short, d)$n,
                            count(EVS_models_short, d_sq)$n,
                            count(EVS_models_short, cont)$n,
                            count(EVS_models_short, SCI)$n) %>% as.data.frame()
colnames(EVS_models_short_summary) <- c("distance", "distance squared",
                                  "contiguity", "SCI")
rownames(EVS_models_short_summary) <- c("OLS", "SAR", "SEM")
EVS_models_short_summary <- EVS_models_short_summary / 176

write.xlsx(EVS_models_short_summary, file = "regressions/EVS_models_short_summary.xlsx")

All_OLS_short <- filter(EVS_models_short, d == "OLS" & d_sq == "OLS" &
                    cont == "OLS" & SCI == "OLS")
SAR_and_SCI_short <- filter(EVS_models_short, SCI == "SAR")