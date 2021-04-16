# Цель: визуализация изучаемых переменных при помощи географических карт
# Inputs:  borrowed_raw_data/eurobarometer_regress_dat.dta
#          borrowed_raw_data/brookings_regress_dat.dta
#          borrowed_raw_data/gadm1_nuts2.rds
#          intermediate_data/education_pref.xlsx
#          borrowed_raw_data/cluster_groups.csv
# Outputs: plots/EU_trust_levels.png
#          plots/anti_EU_votes_levels.png
#          plots/early_edu_leavers.png
#          plots/19_clusters.png
# Дата: 2021-03-27




library(haven)
library(tidyverse)
library(sf)
library(readxl)
library(randomcoloR)
library(grid)
library(gridExtra)
library(cowplot)


# функция для выбора NUTS2 тех регионов, которые NUTS1 в eurobarometer_regress_dat
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


##############################################################################################


# данные о доверии ЕС
eurobarometer_regress_dat <- read_dta("borrowed_raw_data/eurobarometer_regress_dat.dta")
Trust <- eurobarometer_regress_dat %>%
  select(c(NUTS_ID, Trust_in_EU))

# все используемые коды NUTS2
NUTS2_IDs <- eurobarometer_regress_dat %>%
  filter(nuts1_level == 0) %>% 
  select(NUTS_ID)

# все используемые коды NUTS1
NUTS1_IDs <- eurobarometer_regress_dat %>%
  filter(nuts1_level == 1) %>% 
  select(NUTS_ID)


# данные для отображения регионов на карте
data <- readRDS('borrowed_raw_data/gadm1_nuts2.rds')
data <- data %>% filter(level == "nuts2")
data$key <- as.character(data$key)
#data <- data %>% filter(is.element(key, NUTS2_IDs[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs[[1]]))
data <- data[order(data$key),]
row.names(data) <- NULL


# техническая манипуляция (связано с тем, что часть регионов - не NUTS2, а NUTS1)
map_data <- data %>% 
  mutate(key = case_when(
    is.element(key, NUTS2_IDs[[1]]) ~ key,
    !is.element(key, NUTS2_IDs[[1]]) ~ str_sub(key, 1, 3)))


# создадим категориальный признак для визуализации
map_data <- Trust %>% 
  right_join(map_data, by=c("NUTS_ID"="key")) %>% 
  st_as_sf %>% 
  mutate(trust_levels = case_when(
    Trust_in_EU > .7 ~ ">70%",
    Trust_in_EU > .65 ~ "65-70%",
    Trust_in_EU > .6 ~ "60-65%",
    Trust_in_EU > .5 ~ "50-60%",
    Trust_in_EU > .4 ~ "40-50%",
    Trust_in_EU > 0 ~ "<40%")) %>% 
  mutate(trust_levels = factor(trust_levels,
                               levels = c(">70%", "65-70%", "60-65%", "50-60%", "40-50%", "<40%")))

# карта уровня доверия ЕС
ggplot(data = map_data) +
  geom_sf(aes(fill = trust_levels), size=0.3, col="#606060") +
  geom_sf(data = map_data$geometry, fill=NA, color="black", size=0.3) +
  theme_void() +
  scale_fill_brewer(palette = "RdYlGn", direction = -1) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55)) +
  labs(fill = "EU trust levels \nin different regions")

ggsave("plots/EU_trust_levels.png", last_plot(), width=8, height=5.75, units="in", dpi=500)


##############################################################################################


# визуализируем голоса за anti-EU партии
brookings_regress_dat <- read_dta("borrowed_raw_data/brookings_regress_dat.dta")
Anti_EU <- brookings_regress_dat %>%
  select(c(nuts, Anti_EU_vote))


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs_br <- brookings_regress_dat %>%
  filter(nchar(nuts) == 4) %>% 
  select(nuts)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs_br <- brookings_regress_dat %>%
  filter(nchar(nuts) == 3) %>% 
  select(nuts)


# данные для отображения регионов на карте
data_br <- readRDS('borrowed_raw_data/gadm1_nuts2.rds')
data_br <- data_br %>% filter(level == "nuts2")
data_br$key <- as.character(data_br$key)
#data_br <- data_br %>% filter(is.element(key, NUTS2_IDs_br[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs_br[[1]]))
data_br <- data_br[order(data_br$key),]
row.names(data_br) <- NULL


# техническая манипуляция (связано с тем, что часть регионов - не NUTS2, а NUTS1)
map_data_br <- data_br %>% 
  mutate(key = case_when(
    is.element(key, NUTS2_IDs_br[[1]]) ~ key,
    !is.element(key, NUTS2_IDs_br[[1]]) ~ str_sub(key, 1, 3)))


# создадим категориальный признак для визуализации
map_data_br <- Anti_EU %>% 
  right_join(map_data_br, by=c("nuts"="key")) %>% 
  st_as_sf %>% 
  mutate(vote_levels = case_when(
    Anti_EU_vote > .5 ~ ">50%",
    Anti_EU_vote > .4 ~ "40-50%",
    Anti_EU_vote > .3 ~ "30-40%",
    Anti_EU_vote > .2 ~ "20-30%",
    Anti_EU_vote > .1 ~ "10-20%",
    Anti_EU_vote >= 0 ~ "<10%")) %>% 
  mutate(vote_levels = factor(vote_levels,
                              levels = c(">50%", "40-50%", "30-40%", "20-30%", "10-20%", "<10%")))


# карта доли голосов за anti-EU партии
ggplot(data = map_data_br) +
  geom_sf(aes(fill = vote_levels), size=0.3, col="#606060") +
  geom_sf(data = map_data_br$geometry, fill=NA, color="black", size=0.3) +
  theme_void() +
  scale_fill_brewer(palette = "RdYlGn", direction = 1) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55)) +
  labs(fill = "Anti EU voting levels \nin different regions")


ggsave("plots/anti_EU_votes_levels.png", last_plot(), width=8, height=5.75, units="in", dpi=500)


##############################################################################################


# визуализируем образовательные предпочтения
education_pref <- read_excel("intermediate_data/education_pref.xlsx", col_types = c("skip", "text", "text", "text"))
early_educ_leavers <- education_pref %>% 
  select(c(region_code, early_leavers_educ_train_total))

early_educ_leavers$early_leavers_educ_train_total <- as.double(early_educ_leavers$early_leavers_educ_train_total)


# все используемые коды NUTS2 (в этом наборе данных другое территориальное деление)
NUTS2_IDs_ed <- education_pref %>%
  filter(nchar(region_code) == 4) %>% 
  select(region_code)


# все используемые коды NUTS1 (в этом наборе данных другое территориальное деление)
NUTS1_IDs_ed <- education_pref %>%
  filter(nchar(region_code) == 3) %>% 
  select(region_code)


# данные для отображения регионов на карте
data_ed <- readRDS('borrowed_raw_data/gadm1_nuts2.rds')
data_ed <- data_ed %>% filter(level == "nuts2")
data_ed$key <- as.character(data_ed$key)
#data_ed <- data_ed %>% filter(is.element(key, NUTS2_IDs_ed[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs_ed[[1]]))
data_ed <- data_ed[order(data_ed$key),]
row.names(data_ed) <- NULL


# техническая манипуляция (связано с тем, что часть регионов - не NUTS2, а NUTS1)
map_data_ed <- data_ed %>% 
  mutate(key = case_when(
    is.element(key, NUTS2_IDs_ed[[1]]) ~ key,
    !is.element(key, NUTS2_IDs_ed[[1]]) ~ str_sub(key, 1, 3)))


# создадим категориальный признак для визуализации
map_data_ed <- early_educ_leavers %>% 
  right_join(map_data_ed, by=c("region_code"="key")) %>% 
  st_as_sf %>% 
  mutate(early_levels = case_when(
    early_leavers_educ_train_total > 25 ~ ">25%",
    early_leavers_educ_train_total > 20 ~ "20-25%",
    early_leavers_educ_train_total > 15 ~ "15-25%",
    early_leavers_educ_train_total > 10 ~ "10-15%",
    early_leavers_educ_train_total > 5 ~ "5-10%",
    early_leavers_educ_train_total > 0 ~ "<5%")) %>% 
  mutate(early_levels = factor(early_levels,
                               levels = c(">25%", "20-25%", "15-25%", "10-15%", "5-10%", "<5%")))


# карта раннего прекращения образовательной деятельности
ggplot(data = map_data_ed) +
  geom_sf(aes(fill = early_levels), size=0.3, col="#606060") +
  geom_sf(data = map_data_ed$geometry, fill=NA, color="black", size=0.3) +
  theme_void() +
  scale_fill_brewer(palette = "RdPu", direction = -1) + 
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme(legend.position = c(0.9, 0.55)) +
  labs(fill = "Early leavers \nfrom education \nin different regions")


ggsave("plots/early_edu_leavers.png", last_plot(), width=8, height=5.75, units="in", dpi=500)


##############################################################################################

# карта кластеров для фиксированных эффектов
# данный код написан на основе кода из github-репозитория:
# https://github.com/social-connectedness-index/euro_sci/blob/master/_analysis_scripts/generate_cluster_maps.R


clusters_dat <- read_csv("borrowed_raw_data/cluster_groups.csv")


# данные для отображения регионов на карте
data <- readRDS('borrowed_raw_data/gadm1_nuts2.rds')
data <- data %>% filter(level == "nuts2")
data$NUTS_ID <- as.character(data$key)
#data_ed <- data_ed %>% filter(is.element(key, NUTS2_IDs_ed[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs_ed[[1]]))
data <- data[order(data$NUTS_ID),]
row.names(data) <- NULL


## 19 Clusters ##
map_dat_19 <- select(clusters_dat, region, group=clusters19) %>% 
  mutate(group = as.factor(group)) %>% 
  left_join(data, by=c("region"="NUTS_ID")) %>% 
  st_as_sf


## Color map
set.seed(1)
color_pal_19 <- distinctColorPalette(19)
color_pal_19 <- sample(color_pal_19, 19, replace=F)


ggplot(data=map_dat_19) +
  geom_sf(aes(fill=group), size=0.3, col="#606060") +
  scale_fill_manual(values = color_pal_19) +
  geom_sf(data=data, fill=NA, color="black", size=0.3) +
  coord_sf(xlim = c(-31, 60), ylim = c(34.25, 71), expand = FALSE) +
  theme_nothing()


ggsave("plots/19_clusters.png", last_plot(), width=8, height=5.75, units="in", dpi=500)



