# Цель: создать SCI_and_exogenous_variables.csv
# Inputs:  borrowed_raw_data/nuts2_to_nuts2_regress_dat.csv
# Outputs: predict_SCI/SCI_and_exogenous_variables.csv
# Дата: 2021-08-16




library(tidyverse)


nuts2_to_nuts2_regress_dat <- read_csv("borrowed_raw_data/nuts2_to_nuts2_regress_dat.csv")

# выберем экзогенные переменные
SCI_and_exogenous_variables <- nuts2_to_nuts2_regress_dat %>% 
  select(
    -c(
      sci, user_country, fr_country, ctry_pair, user_country_1900, user_country_1930,
      user_country_1960, user_country_1990, fr_country_1900, fr_country_1930, log_sci,
      fr_country_1960, fr_country_1990, user_income, user_ED0_2, user_median_age,
      user_unemp_rate, fr_income, fr_ED0_2, fr_median_age, fr_unemp_rate, industry_sim,
      diff_share_low_edu, diff_median_age, diff_income_thous, diff_unemp_rate, log_distance,
      rlgdnm_user_w_none, rlgdnm_user_wo_none, rlgdnm_fr_w_none, rlgdnm_fr_wo_none,
      lnghom_user, lnghom_fr, same_rlgdnm_w_none, same_rlgdnm_wo_none, both_country
      )
    )

# преобразуем logiacal в numeric
for (i in 5:25) {
  SCI_and_exogenous_variables[, i] <- SCI_and_exogenous_variables[, i][[1]] %>% as.numeric()
}


# сохраним результат
write.csv(SCI_and_exogenous_variables, "predict_SCI/SCI_and_exogenous_variables.csv")

