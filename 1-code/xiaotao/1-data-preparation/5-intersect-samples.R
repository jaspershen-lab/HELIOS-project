library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

library(tidymass)

load("3-data_analysis/1-data-preparation/1-phenotype-data/phenotype_data.rda")
load(
  "3-data_analysis/1-data-preparation/2-clinical-lab-test-data/clinical_lab_test_data.rda"
)
load(
  "3-data_analysis/1-data-preparation/3-gut-microbiome-data/gut_microbiome_data.rda"
)
load("3-data_analysis/1-data-preparation/4-food-data/food_data1.rda")
load("3-data_analysis/1-data-preparation/4-food-data/food_data2.rda")

intesect_sample_id <-
  Reduce(
    intersect,
    list(
      clinical_lab_test_data@sample_info$sample_id,
      gut_microbiome_data@sample_info$sample_id_old,
      food_data1@sample_info$sample_id,
      food_data2@sample_info$sample_id
    )
  )

clinical_lab_test_data <-
  clinical_lab_test_data %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(sample_id %in% intesect_sample_id)

gut_microbiome_data <-
  gut_microbiome_data %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(sample_id_old %in% intesect_sample_id)

food_data1 <-
  food_data1 %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(sample_id %in% intesect_sample_id)

food_data2 <-
  food_data2 %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(sample_id %in% intesect_sample_id)


save(clinical_lab_test_data, file = "3-data_analysis/1-data-preparation/2-clinical-lab-test-data/clinical_lab_test_data.rda")

save(gut_microbiome_data, file = "3-data_analysis/1-data-preparation/3-gut-microbiome-data/gut_microbiome_data.rda")

save(food_data1, file = "3-data_analysis/1-data-preparation/4-food-data/food_data1.rda")

save(food_data2, file = "3-data_analysis/1-data-preparation/4-food-data/food_data2.rda")
