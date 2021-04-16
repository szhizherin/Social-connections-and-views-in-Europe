# Цель: создание финальных массивов данных
# Inputs:  borrowed_raw_data/eurobarometer_regress_dat.dta
#          borrowed_raw_data/brookings_regress_dat.dta
#          borrowed_raw_data/cluster_groups.csv
#          intermediate_data/education_pref.xlsx
#          intermediate_data/wvs_EU_data.xlsx
# Outputs: final_data/trust_in_EU.xlsx
#          final_data/anti_EU_votes.xlsx
#          final_data/early_leavers_from_edu.xlsx
#          final_data/primary_secondary_participation.xlsx
#          final_data/world_values_survey.xlsx
# Дата: 2021-03-18




library(haven)
library(tidyverse)
library(readxl)
library(xlsx)
library(readr)
library(sjmisc)


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


# результаты кластеризации европейских регионов по SCI
cluster_groups <- read_csv("borrowed_raw_data/cluster_groups.csv")


# данные для анализа уровня доверия ЕС
eurobarometer_regress_dat <- read_dta("borrowed_raw_data/eurobarometer_regress_dat.dta")
eurobarometer_regress_dat <- eurobarometer_regress_dat %>% select(!cntry_fe) # столбец дублируется

trust_in_EU <- eurobarometer_regress_dat %>% 
  select(-c(nuts1_level, income, unemp_rate_avg, country_fe)) %>% 
  rename(c("EU_friends_abroad" = "share_EU_connections_out_country"))


# все используемые коды NUTS2
NUTS2_IDs <- trust_in_EU %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- trust_in_EU %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды
for_clusters <- cluster_groups %>% 
  select(region, clusters19) %>% 
  filter(is.element(region, NUTS2_IDs[[1]]) | is.NUTS1_subregion(region, NUTS1_IDs[[1]]))


# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_clusters)[1]) {
  if (is.element(str_sub(for_clusters$region[i], 1, 3), NUTS1_IDs[[1]])) {
    for_clusters$region[i] <- str_sub(for_clusters$region[i], 1, 3)
  }
}


for_clusters <- for_clusters %>% distinct()


trust_in_EU <- trust_in_EU %>% 
  inner_join(for_clusters, by = c("NUTS_ID" = "region"))


trust_in_EU <- trust_in_EU %>% 
  to_dummy(clusters19, var.name = "cluster", suffix = "label") %>% 
  bind_cols(trust_in_EU) %>% 
  select(everything())


write.xlsx(trust_in_EU, file = "final_data/trust_in_EU.xlsx")


# данные для анализа голосов за anti-EU партии
brookings_regress_dat <- read_dta("borrowed_raw_data/brookings_regress_dat.dta")
brookings_regress_dat <- brookings_regress_dat %>% select(!cntry_fe) %>% # столбец дублируется
  rename(c("NUTS_ID" = "nuts", "EU_friends_abroad" = "share_EU_connections_out_country"))

anti_EU_votes <- brookings_regress_dat %>% 
  select(-c(income, unemp_rate_avg, country_fe))


# все используемые коды NUTS2
NUTS2_IDs <- anti_EU_votes %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- anti_EU_votes %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# выберем строки, содержащие все нужные коды
for_clusters <- cluster_groups %>% 
  select(region, clusters19) %>% 
  filter(is.element(region, NUTS2_IDs[[1]]) | is.NUTS1_subregion(region, NUTS1_IDs[[1]]))


# преобразуем названия нужных NUTS2 в NUTS1
for (i in 1:dim(for_clusters)[1]) {
  if (is.element(str_sub(for_clusters$region[i], 1, 3), NUTS1_IDs[[1]])) {
    for_clusters$region[i] <- str_sub(for_clusters$region[i], 1, 3)
  }
}


for_clusters <- for_clusters %>% distinct()


anti_EU_votes <- anti_EU_votes %>% 
  inner_join(for_clusters, by = c("NUTS_ID" = "region"))


anti_EU_votes <- anti_EU_votes %>% 
  to_dummy(clusters19, var.name = "cluster", suffix = "label") %>% 
  bind_cols(anti_EU_votes) %>% 
  select(everything())


write.xlsx(anti_EU_votes, file = "final_data/anti_EU_votes.xlsx")


# данные для анализа образовательных предпочтений
education_pref <- read_excel("intermediate_data/education_pref.xlsx", 
                             col_types = c("skip", "text", "text", "text"))

education_preferences <- anti_EU_votes %>% 
  inner_join(education_pref, by=c("NUTS_ID"="region_code")) %>% 
  select(-c(Anti_EU_vote, extreme_per, extreme_perel, extreme_perer, extreme_perp)) %>% 
  rename(c("early_leavers" = "early_leavers_educ_train_total", 
           "primary_secondary" = "participation_rate_primary_secondary"))

early_leavers_from_edu <- education_preferences
early_leavers_from_edu$primary_secondary <- NULL
early_leavers_from_edu <- early_leavers_from_edu %>% drop_na()
early_leavers_from_edu$early_leavers <- as.double(early_leavers_from_edu$early_leavers)

primary_secondary_participation <- education_preferences
primary_secondary_participation$early_leavers <- NULL
primary_secondary_participation <- primary_secondary_participation %>% drop_na()
primary_secondary_participation$primary_secondary <- as.double(primary_secondary_participation$primary_secondary)


write.xlsx(early_leavers_from_edu, file = "final_data/early_leavers_from_edu.xlsx")
write.xlsx(primary_secondary_participation, file = "final_data/primary_secondary_participation.xlsx")


# данные для анализа взглядов людей на основе World Values Survey
wvs_EU_data <- read_excel("intermediate_data/wvs_EU_data.xlsx") %>% 
  select(-c(1))


control_vars <- rbind(trust_in_EU %>% select(-c(Trust_in_EU)),
                      anti_EU_votes %>% select(-c(extreme_per, extreme_perel,
                                                  extreme_perer, extreme_perp, Anti_EU_vote))) %>% distinct()


world_values_survey <- wvs_EU_data %>% rename(NUTS_ID = `NUTS-2`) %>% 
  left_join(control_vars, by = c("NUTS_ID" = "NUTS_ID")) %>% 
  rename(c("Leisure_time" = "Leisure time", "Happiness" = "Feeling of happiness",
           "Health" = "State of health (subjective)", "Life_satisfaction" = "Satisfaction with your life",
           "Freedom" = "How much freedom of choice and control", "Trust" = "Most people can be trusted",
           "Anti_fem1" = "Jobs scarce: Men should have more right to a job than women (5-point scale)",
           "Conf_in_press" = "The Press", "Conf_in_police" = "The Police", "Conf_in_civil_services" = "The Civil Services",
           "Conf_in_EU" = "The European Union", "Conf_in_parties" = "The Political Parties",
           "Conf_in_eco" = "The Environmental Protection Movement", "Conf_in_courts" = "Justice System/Courts",
           "Interest_in_politics" = "Interest in politics", "Polit_sys_satisfaction" = "Satisfaction with the political system",
           "Homo_neighbours" = "Neighbours: Homosexuals", "Anti_fem2" = "Men make better political leaders than women do",
           "Anti_fem3" = "Men make better business executives than women do",
           "Member_control1" = "Member: Belong to religious organization",
           "Member_control2" = "Member: Belong to education, arts, music or cultural activities",
           "Member_control3" = "Member: Belong to labour unions",
           "Member_control4" = "Member: Belong to political parties",
           "Member_control5" = "Member: Belong to conservation, the environment, ecology, animal rights",
           "Member_control6" = "Member: Belong to professional associations",
           "Member_control7" = "Member: Belong to sports or recreation",
           "Member_control8" = "Member: Belong to consumer groups",
           "Member_control9" = "Member: Belong to humanitarian or charitable organization",
           "Member_control11" = "Member: Belong to self-help group, mutual aid group")) %>% 
  select(-c("...3", "...7"))


write.xlsx(world_values_survey, file = "final_data/world_values_survey.xlsx")




