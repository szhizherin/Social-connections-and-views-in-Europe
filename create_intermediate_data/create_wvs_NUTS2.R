# Цель: преобразовать набор данных wvs_data, выбрав нужные регионы
# Inputs:  borrowed_raw_data/brookings_regress_dat.dta
#          raw_data/wvs_data.xlsx
#          raw_data/EU_preferences_renamed.xlsx
# Outputs: intermediate_data/wvs_EU_data.xlsx
#          intermediate_data/wvs_whole_EU_data.xlsx
# Дата: 2021-03-12




library(tidyverse)
library(xlsx)
library(readxl)
library(haven)


# функция для выбора NUTS2 тех регионов, которые NUTS1 в brookings_regress_dat
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


# предпочтения из World Values Survey
wvs_data <- read_excel("raw_data/wvs_data.xlsx")


# набор данных, содержащий контрольные переменные
brookings_regress_dat <- read_dta("borrowed_raw_data/brookings_regress_dat.dta")
eurobarometer_regress_dat <- read_dta("borrowed_raw_data/eurobarometer_regress_dat.dta")

# все используемые коды NUTS2
NUTS2_IDs_br <- brookings_regress_dat %>%
  filter(nchar(nuts) == 4) %>% 
  select(nuts) %>% rename(NUTS_ID = nuts)
NUTS2_IDs <- eurobarometer_regress_dat %>%
  filter(nuts1_level == 0) %>% 
  select(NUTS_ID)

# все используемые коды NUTS1
NUTS1_IDs_br <- brookings_regress_dat %>%
  filter(nchar(nuts) == 3) %>% 
  select(nuts) %>% rename(NUTS_ID = nuts)
NUTS1_IDs <- eurobarometer_regress_dat %>%
  filter(nuts1_level == 1) %>% 
  select(NUTS_ID)


# создадим наиболее подробное разбиение по кодам
NUTS2_IDs_all <- rbind(NUTS2_IDs, NUTS2_IDs_br) %>% distinct()
NUTS1_IDs_all <- rbind(NUTS1_IDs, NUTS1_IDs_br) %>% distinct()
NUTS1_IDs_all <- NUTS1_IDs_all %>% 
  filter(!is.element(NUTS_ID, NUTS2_IDs_all$NUTS_ID %>% str_sub(1, 3)))


# отберём интересующие нас регионы
wvs_data_EU <- wvs_data %>% 
  filter(is.element(`NUTS-2`, NUTS2_IDs_all[[1]]) | is.NUTS1_subregion(`NUTS-2`, NUTS1_IDs_all[[1]])) %>% 
  select(-c("Which political party appeals to you most (ISO 3166-1) (EVS5)",
            "Self positioning in political scale",
            "Democracy: Women have the same rights as men.",
            "Homosexual couples are as good parents as other couples",
            "Believe in: God",
            "Churches",
            "Armed Forces",
            "Labour Unions",
            "Parliament",
            "Major regional organization (combined from country-specific)",
            "Major Companies",
            "The United Nations",
            "Political system: Having a strong leader",
            "Political system: Having the army rule",
            "Political system: Having a democratic political system",
            "Political system: Having experts make decisions",
            "Member: Belong to other groups"))


wvs_data_EU %>% is.na() %>% colSums() # число пропусков по столбцам


# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(wvs_data_EU)[1]) {
  if (is.element(str_sub(wvs_data_EU$`NUTS-2`[i], 1, 3), NUTS1_IDs_all[[1]])) {
    wvs_data_EU$`NUTS-2`[i] <- str_sub(wvs_data_EU$`NUTS-2`[i], 1, 3)
  }
}


# проведём корректное взвешивание и сгруппируем
wvs_EU_grouped <- wvs_data_EU %>% select(`NUTS-2`) %>% 
  distinct()
wvs_EU_grouped <- wvs_EU_grouped[order(wvs_EU_grouped$`NUTS-2`),]

# сгруппируем по NUTS2 регионам
for (i in 6:40) {
  name <- colnames(wvs_data_EU)[i]
  batch <- wvs_data_EU[, c(2, 3, 5, i)] %>% drop_na()
  batch[, 4] <- batch[, 4]  # домножение на веса в данном случае некорректно

  batch <- batch %>% group_by(`NUTS-2`) %>% summarise(UQ(rlang::sym(name)) := mean(UQ(rlang::sym(name))))
  
  wvs_EU_grouped <- wvs_EU_grouped %>% 
    right_join(batch, by = c("NUTS-2" = "NUTS-2"))
}


# отнормируем на максимум по столбцу
for (i in 2:36) {
  wvs_EU_grouped[, i] <- wvs_EU_grouped[, i] / max(wvs_EU_grouped[, i])
}


# сохраним результат
write.xlsx(wvs_EU_grouped, file = "intermediate_data/wvs_EU_data.xlsx")


# проделаем аналогичные преобразования для полного списка предпочтений WVS
wvs_whole <- read_excel("raw_data/EU_preferences_renamed.xlsx")

wvs_whole_EU <- wvs_whole %>% 
  filter(is.element(`NUTS`, NUTS2_IDs_all[[1]]) | is.NUTS1_subregion(`NUTS`, NUTS1_IDs_all[[1]])) %>% 
  select(-c("Pray_to_God_outside_of_religious_services_(EVS5)"))


for (i in 1:dim(wvs_whole_EU)[1]) {
  if (is.element(str_sub(wvs_whole_EU$`NUTS`[i], 1, 3), NUTS1_IDs_all[[1]])) {
    wvs_whole_EU$`NUTS`[i] <- str_sub(wvs_whole_EU$`NUTS`[i], 1, 3)
  }
}


wvs_whole_EU_grouped <- wvs_whole_EU %>% select(`NUTS`) %>% 
  distinct()
wvs_whole_EU_grouped <- wvs_whole_EU_grouped[order(wvs_whole_EU_grouped$`NUTS`),]


for (i in 2:139) {
  name <- colnames(wvs_whole_EU)[i]
  batch <- wvs_whole_EU[, c(1, i)] %>% drop_na()
  batch[, 2] <- batch[, 2]
  
  batch <- batch %>% group_by(`NUTS`) %>% summarise(UQ(rlang::sym(name)) := mean(UQ(rlang::sym(name))))
  
  wvs_whole_EU_grouped <- wvs_whole_EU_grouped %>% 
    right_join(batch, by = c("NUTS" = "NUTS"))
}


for (i in 2:139) {
  wvs_whole_EU_grouped[, i] <- wvs_whole_EU_grouped[, i] / max(wvs_whole_EU_grouped[, i])
}

write.xlsx(wvs_whole_EU_grouped, file = "intermediate_data/wvs_whole_EU_data.xlsx")
