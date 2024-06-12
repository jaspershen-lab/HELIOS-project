library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

library(tidymass)

load(
  "3-data_analysis/1-data-preparation/3-gut-microbiome-data/gut_microbiome_data.rda"
)

dir.create("3-data_analysis/2-study-summary")
setwd("3-data_analysis/2-study-summary/")

sample_info <-
  extract_sample_info(gut_microbiome_data)

sample_info <-
  sample_info %>%
  dplyr::filter(!is.na(subject_id))

###demographic data
