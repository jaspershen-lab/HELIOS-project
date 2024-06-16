library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

library(tidymass)

load("3-data_analysis/1-data-preparation/4-food-data/food_data1.rda")

load("3-data_analysis/1-data-preparation/4-food-data/food_data2.rda")

dir.create("3-data_analysis/2-study-summary/4-food-data-summary")
setwd("3-data_analysis/2-study-summary/4-food-data-summary")

dim(food_data1)
dim(food_data2)

rownames(food_data1)
rownames(food_data2)

food_data1@variable_info$`Description of variable`

##heatmap to show the food
dim(food_data1)
sum(is.na(food_data1))

library(ComplexHeatmap)

which(is.na(expression_data), arr.ind = TRUE) %>%
  as.data.frame() %>%
  pull(row) %>%
  table()

library(circlize)
library(viridis)
col_fun <- circlize::colorRamp2(seq(-2, 2, length.out = 11), rev(RColorBrewer::brewer.pal(name = "RdYlBu", n = 11)))
na_col <- "grey"

range(expression_data, na.rm = TRUE)

expression_data[expression_data < -2] <- -2
expression_data[expression_data > 2] <- 2

rownames(expression_data) == clinical_lab_test_data@variable_info$variable_id
rownames(expression_data) <- clinical_lab_test_data@variable_info$`Description of variable`

plot <-
  Heatmap(
    as.matrix(t(expression_data)),
    name = "Z-score",
    show_row_names = FALSE,
    show_column_names = TRUE,
    col = col_fun,
    na_col = na_col,
    cluster_columns = TRUE,
    cluster_rows = TRUE,
    column_names_gp = gpar(fontsize = 6),
    row_names_gp = gpar(fontsize = 6),
    column_names_rot = 45,
    clustering_distance_columns = "euclidean",
    clustering_method_columns = "complete",
    clustering_distance_rows = "euclidean",
    clustering_method_rows = "complete"
  )

plot <- ggplotify::as.ggplot(plot)
plot
ggsave(
  plot,
  filename = "clinical_heatmap.png",
  width = 5,
  height = 10
)

ggsave(
  plot,
  filename = "clinical_heatmap.pdf",
  width = 5,
  height = 10
)

