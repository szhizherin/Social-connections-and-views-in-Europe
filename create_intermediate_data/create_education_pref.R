# Цель: Создать файл с переменными, характеризующими образовательные предпочтения
# Inputs:  raw_data/new_nuts2_eureg_data.xlsx
#          raw_data/qog_eureg_wide1_nov20.xlsx
#          borrowed_raw_data/brookings_regress_dat.dta
# Outputs: intermediate_data/education_pref.xlsx
# Дата: 2021-03-06




library(haven)
library(tidyverse)
library(xlsx)
library(plyr)
library(readxl)


brookings_regress_dat <- read_dta("borrowed_raw_data/brookings_regress_dat.dta")


# все используемые коды NUTS2
NUTS2_IDs_br <- brookings_regress_dat %>%
  filter(nchar(nuts) == 4) %>% 
  select(nuts)


# все используемые коды NUTS1
NUTS1_IDs_br <- brookings_regress_dat %>%
  filter(nchar(nuts) == 3) %>% 
  select(nuts)


# данные по NUTS2 регионам
new_nuts2_eureg_data <- read_excel("raw_data/new_nuts2_eureg_data.xlsx") %>% 
  filter(year >= 2015 & is.element(region_code, NUTS2_IDs_br[[1]])) %>% 
  select(-nuts2)


# данные по NUTS1 регионам
qog_eureg_wide1_nov20 <- read_excel("raw_data/qog_eureg_wide1_nov20.xlsx") %>% 
  filter(year >= 2015 & is.element(region_code, NUTS1_IDs_br[[1]])) %>% 
  select(-nuts1)


data_joined <- rbind(new_nuts2_eureg_data, qog_eureg_wide1_nov20) %>% 
  select(-c(n_o_deaths_self_harm_male, n_o_deaths_self_harm_fem, 
            early_leavers_educ_train_male, early_leavers_educ_train_fem))

data_joined[data_joined == "NA"] <- NA


# отберём интересующие нас данные
data_final <- data_joined %>% select(region_code, year, early_leavers_educ_train_total, 
                                     participation_rate_primary_secondary) %>% 
  filter(year == 2017) %>% select(-year)


# сохраним результат
write.xlsx(data_final, file = "intermediate_data/education_pref.xlsx")






