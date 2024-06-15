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


