# Цель: создать пространственные матрицы смежности
# Inputs:  borrowed_raw_data/gadm1_nuts2.rds
#          final_data/trust_in_EU.xlsx
#          final_data/world_values_survey.xlsx
#          final_data/anti_EU_votes.xlsx
#          intermediate_data/evs_EU_data_2008.xlsx
# Outputs: intermediate_data/contiguity_matrix.xlsx
#          intermediate_data/contiguity_matrix_wvs.xlsx
#          intermediate_data/contiguity_matrix_anti.xlsx
#          intermediate_data/contiguity_matrix_evs_2008.xlsx
# Дата: 2021-09-03




library(haven)
library(tidyverse)
library(xlsx)
library(readxl)
library(sf)
library(spdep)


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


eurobarometer_regress_dat <- read_excel("final_data/trust_in_EU.xlsx") %>% 
  select(-1)


# все используемые коды NUTS2
NUTS2_IDs <- eurobarometer_regress_dat %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- eurobarometer_regress_dat %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# данные для пространственной матрицы на основе соседства
data <- readRDS("borrowed_raw_data/gadm1_nuts2.rds")
data <- data %>% filter(level == "nuts2")
data$key <- as.character(data$key)
data <- data %>% filter(is.element(key, NUTS2_IDs[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs[[1]]))
data <- data[order(data$key),]
row.names(data) <- NULL
region_ids <- data$key
borders_poly <- data$geometry


# создадим матрицу соседства с большей детализацией, чем нужно
contiguity_nb <- poly2nb(borders_poly, row.names = region_ids)
contiguity_mat <- nb2mat(contiguity_nb, style = "B", zero.policy = TRUE)
colnames(contiguity_mat) <- rownames(contiguity_mat)


# преобразуем нужные NUTS2 в NUTS1 и произведём группировку
contiguity_df <- as.data.frame(contiguity_mat)
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

new_names <- contiguity_df$grouping_col
contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names

contiguity_df <- contiguity_df %>% as.matrix() %>% t() %>% as.data.frame()
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names


# наконец, матрица
contiguity_matrix <- contiguity_df %>% as.matrix()
for(i in 1:dim(contiguity_matrix)[1]) {
  contiguity_matrix[i, i] <- 0
}
View(contiguity_matrix)


# сохраним результат
write.xlsx(contiguity_matrix, file = "intermediate_data/contiguity_matrix.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
anti_EU_votes <- read_excel("final_data/anti_EU_votes.xlsx") %>% 
  select(-1)


# все используемые коды NUTS2
NUTS2_IDs <- anti_EU_votes %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- anti_EU_votes %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# данные для пространственной матрицы на основе соседства
data <- readRDS("borrowed_raw_data/gadm1_nuts2.rds")
data <- data %>% filter(level == "nuts2")
data$key <- as.character(data$key)
data <- data %>% filter(is.element(key, NUTS2_IDs[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs[[1]]))
data <- data[order(data$key),]
row.names(data) <- NULL
region_ids <- data$key
borders_poly <- data$geometry


# создадим матрицу соседства с большей детализацией, чем нужно
contiguity_nb <- poly2nb(borders_poly, row.names = region_ids)
contiguity_mat <- nb2mat(contiguity_nb, style = "B", zero.policy = TRUE)
colnames(contiguity_mat) <- rownames(contiguity_mat)


# преобразуем нужные NUTS2 в NUTS1 и произведём группировку
contiguity_df <- as.data.frame(contiguity_mat)
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

new_names <- contiguity_df$grouping_col
contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names

contiguity_df <- contiguity_df %>% as.matrix() %>% t() %>% as.data.frame()
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names


# наконец, матрица
contiguity_matrix <- contiguity_df %>% as.matrix()
for(i in 1:dim(contiguity_matrix)[1]) {
  contiguity_matrix[i, i] <- 0
}
View(contiguity_matrix)


# сохраним результат
write.xlsx(contiguity_matrix, file = "intermediate_data/contiguity_matrix_anti.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
world_values_survey <- read_excel("final_data/world_values_survey.xlsx") %>% 
  select(-1)


# все используемые коды NUTS2
NUTS2_IDs <- world_values_survey %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- world_values_survey %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# данные для пространственной матрицы на основе соседства
data <- readRDS("borrowed_raw_data/gadm1_nuts2.rds")
data <- data %>% filter(level == "nuts2")
data$key <- as.character(data$key)
data <- data %>% filter(is.element(key, NUTS2_IDs[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs[[1]]))
data <- data[order(data$key),]
row.names(data) <- NULL
region_ids <- data$key
borders_poly <- data$geometry


# создадим матрицу соседства с большей детализацией, чем нужно
contiguity_nb <- poly2nb(borders_poly, row.names = region_ids)
contiguity_mat <- nb2mat(contiguity_nb, style = "B", zero.policy = TRUE)
colnames(contiguity_mat) <- rownames(contiguity_mat)


# преобразуем нужные NUTS2 в NUTS1 и произведём группировку
contiguity_df <- as.data.frame(contiguity_mat)
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

new_names <- contiguity_df$grouping_col
contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names

contiguity_df <- contiguity_df %>% as.matrix() %>% t() %>% as.data.frame()
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names


# наконец, матрица
contiguity_matrix <- contiguity_df %>% as.matrix()
for(i in 1:dim(contiguity_matrix)[1]) {
  contiguity_matrix[i, i] <- 0
}
View(contiguity_matrix)


# сохраним результат
write.xlsx(contiguity_matrix, file = "intermediate_data/contiguity_matrix_wvs.xlsx")


######################################################################################################


# аналогичным образом создадим матрицу для другого набора данных
european_values_survey <- read_excel("intermediate_data/evs_EU_data_2008.xlsx") %>% 
  select(-c(1)) %>% rename(NUTS_ID = "X048b_n2")


# все используемые коды NUTS2
NUTS2_IDs <- european_values_survey %>%
  filter(nchar(NUTS_ID) == 4) %>% 
  select(NUTS_ID)


# все используемые коды NUTS1
NUTS1_IDs <- european_values_survey %>%
  filter(nchar(NUTS_ID) == 3) %>% 
  select(NUTS_ID)


# данные для пространственной матрицы на основе соседства
data <- readRDS("borrowed_raw_data/gadm1_nuts2.rds")
data <- data %>% filter(level == "nuts2")
data$key <- as.character(data$key)
data <- data %>% filter(is.element(key, NUTS2_IDs[[1]]) | is.NUTS1_subregion(key, NUTS1_IDs[[1]]))
data <- data[order(data$key),]
row.names(data) <- NULL
region_ids <- data$key
borders_poly <- data$geometry


# создадим матрицу соседства с большей детализацией, чем нужно
contiguity_nb <- poly2nb(borders_poly, row.names = region_ids)
contiguity_mat <- nb2mat(contiguity_nb, style = "B", zero.policy = TRUE)
colnames(contiguity_mat) <- rownames(contiguity_mat)


# преобразуем нужные NUTS2 в NUTS1 и произведём группировку
contiguity_df <- as.data.frame(contiguity_mat)
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

new_names <- contiguity_df$grouping_col
contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names

contiguity_df <- contiguity_df %>% as.matrix() %>% t() %>% as.data.frame()
contiguity_df$grouping_col <- rownames(contiguity_df)

for (i in 1:dim(contiguity_df)[1]) {
  if (is.element(str_sub(contiguity_df$grouping_col[i], 1, 3), NUTS1_IDs[[1]])) {
    contiguity_df$grouping_col[i] <- str_sub(contiguity_df$grouping_col[i], 1, 3)
  }
}

contiguity_df <- contiguity_df %>% 
  group_by(grouping_col) %>% 
  summarise_each(mean) %>% 
  ungroup()

contiguity_df$grouping_col <- NULL
contiguity_df[contiguity_df > 0] <- 1
rownames(contiguity_df) <- new_names


# наконец, матрица
contiguity_matrix <- contiguity_df %>% as.matrix()
for(i in 1:dim(contiguity_matrix)[1]) {
  contiguity_matrix[i, i] <- 0
}
View(contiguity_matrix)


# сохраним результат
write.xlsx(contiguity_matrix, file = "intermediate_data/contiguity_matrix_evs_2008.xlsx")




