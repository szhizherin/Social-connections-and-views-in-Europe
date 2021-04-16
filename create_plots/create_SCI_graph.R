# Цель: создать граф пространственной матрицы на основе SCI
# Inputs:  borrowed_raw_data/eurobarometer_regress_dat.dta
#          raw_data/2020-12-16_country_country.tsv
# Outputs: -
# Дата: 2021-03-04




library(haven)
library(tidyverse)
library(xlsx)
library(plyr)
library(igraph)


eurobarometer_regress_dat <- read_dta("borrowed_raw_data/eurobarometer_regress_dat.dta")
eurobarometer_regress_dat <- eurobarometer_regress_dat %>% select(!cntry_fe) # повторяющийся столбец


# данные для графа пространственной матрицы на основе SCI
raw_SCI <- read.table("raw_data/2020-12-16_country_country.tsv", sep = '\t', header = TRUE)


# все страны в выборке
country_IDs <- eurobarometer_regress_dat %>%
  select(country_fe) %>% 
  unique()

counrty_SCI <- raw_SCI %>% 
  filter(is.element(fr_loc, country_IDs[[1]]) & is.element(user_loc, country_IDs[[1]]))


# матрица
country_SCI_matrix <- tapply(counrty_SCI$scaled_sci, counrty_SCI[c("user_loc", "fr_loc")], mean)
for(i in 1:dim(country_SCI_matrix)[1]) {
  country_SCI_matrix[i, i] <- 0
}

# произведём нормировку на максимальное значение SCI между регионами
M <- country_SCI_matrix / max(country_SCI_matrix)
M[M < 0.07] <- 0 # сократим число рёбер в графе
M["PL", "IE"] <- 0.05085746
M["ES", "RO"] <- 0.06036627


# составим граф по матрице M
ge1 = graph.adjacency(adjmatrix = M, mode = "undirected", weighted = TRUE, diag = FALSE)


# открывает редактор, в нём можно рисовать граф
tkplot(ge1, edge.label = round(E(ge1)$weight, 2), vertex.size = 30, vertex.label.cex = 2, edge.label.cex = 2,
       vertex.label.color = "black", edge.label.color = "black")


# результирующий граф хранится в директории plots/SCI_graph

