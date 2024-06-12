library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

data <-
  readxl::read_xlsx("2-data/from_ruwen/HELIOS_Core_redacted.xlsx")

dictionary <-
  readxl::read_xlsx("2-data/from_ruwen/SG100K Data Dictionary - v44_redacted.xlsx")

id2 <- readr::read_csv("2-data/from_ruwen/metadata_fil_01032024_redacted.csv")

dir.create(
  "3-data_analysis/1-data-preparation/1-phenotype-data",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3-data_analysis/1-data-preparation/1-phenotype-data/")

idx <-
  match(colnames(data),
        dictionary$`Variable Name - HELIOS Data Dictionary`)

dictionary$`HELIOS Export Table Name (Long)`[idx]

data <-
  data %>%
  dplyr::rename(subject_id = "FREG0_PID")

library(plyr)

data$sample_id <-
  paste(data$subject_id, data$FREG14_Visit_number, sep = "_")

data <-
  data %>%
  dplyr::select(subject_id, sample_id, everything())

data <-
  data %>%
  dplyr::rename(
    "visit_number" = "FREG14_Visit_number",
    "attended_date" = "FREG3_Attended_Date",
    "age" = "FREG8_Age",
    "gender" = "FREG7_Gender",
    "nationality" = "FREG4_Nationality",
    "ethnic_group" = "FREG5_Ethnic_Group",
    "ethnicity_NRIC" = "FREG6_Ethnicity_NRIC",
    "fasting_hours" = "FREG10_Fasting_Hours",
    "height" = "DBI13_Height",
    weight = "DBI14_Weight",
    waist1 = "FWH16_Waist1",
    waist2 = "FWH17_Waist2",
    waist3 = "FWH18_Waist3",
    hip1 = "FWH19_Hip1",
    hip2 = "FWH20_Hip2",
    hip3 = "FWH21_Hip3",
    caffeine = "FBP16_Caffeine",
    smoking = "FBP17_Smoking"
  )

data <-
  data %>%
  dplyr::select(subject_id:DLAB101_Lab, DLAB51_Pbf)

phenotype_data <-
  data

unique(id2$`Library ID`)

id2$`Original Sample name`

id2 <-
  id2 %>%
  dplyr::rename(library_id = `Library ID`, original_sample_name = "Original Sample name") %>% 
  dplyr::distinct(original_sample_name, .keep_all = TRUE)

phenotype_data <-
  phenotype_data %>%
  dplyr::mutate(FREG1_Barcode = as.character(FREG1_Barcode)) %>%
  dplyr::left_join(id2[, c("library_id", "original_sample_name")], 
                   by = c("FREG1_Barcode" = "original_sample_name")) %>%
  dplyr::select(subject_id, sample_id, library_id, everything())

save(phenotype_data, file = "phenotype_data.rda")
