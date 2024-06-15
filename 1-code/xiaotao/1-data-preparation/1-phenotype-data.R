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

idx1 <-
  match(colnames(data),
        dictionary$`Variable Name - HELIOS Data Dictionary`)

idx2 <-
  stringdist::stringdistmatrix(colnames(data),
                               dictionary$`Variable Name - HELIOS Data Dictionary`) %>%
  apply(1, function(x) {
    which.min(x)
  })

data.frame(
  colnames(data),
  dictionary$`Variable Name - HELIOS Data Dictionary`[idx1],
  dictionary$`Variable Name - HELIOS Data Dictionary`[idx2]
)

idx2[c(12, 24, 34)] <- NA

colnames(data)

new_colnames <- dictionary$`Variable Name - HELIOS Data Dictionary`[idx2]

new_colnames[which(is.na(new_colnames))] <- colnames(data)[which(is.na(new_colnames))]

colnames(data) <- new_colnames

new_colnames_info <-
  dictionary[idx2, ]

colnames(data) == new_colnames_info$`Variable Name - HELIOS Data Dictionary`

###remove the clinical information
unique(new_colnames_info$Subsection)

new_colnames_info$`Description of variable`[which(new_colnames_info$Subsection %in% c("Measurements"))]

##remove Measurements, Chemistry, CBC

data <-
  data[, -which(new_colnames_info$Subsection %in% c("Chemistry", "CBC"))]

new_colnames_info <-
  new_colnames_info[-which(new_colnames_info$Subsection %in% c("Measurements", "Chemistry", "CBC")), ]

library(plyr)

data$sample_id <-
  paste(data$FREG0_PID, data$FREG14_Visit_number, sep = "_")

data <-
  data %>%
  dplyr::select(FREG0_PID, sample_id, everything())

phenotype_data <-
  data %>%
  dplyr::rename(subject_id = FREG0_PID)

unique(id2$`Library ID`)

id2$`Original Sample name`

id2 <-
  id2 %>%
  dplyr::rename(library_id = `Library ID`, original_sample_name = "Original Sample name") %>%
  dplyr::distinct(original_sample_name, .keep_all = TRUE)

phenotype_data <-
  phenotype_data %>%
  dplyr::mutate(FREG1_Barcode = as.character(FREG1_Barcode)) %>%
  dplyr::left_join(id2[, c("library_id", "original_sample_name")], by = c("FREG1_Barcode" = "original_sample_name")) %>%
  dplyr::select(subject_id, sample_id, library_id, everything())

save(phenotype_data, file = "phenotype_data.rda")
