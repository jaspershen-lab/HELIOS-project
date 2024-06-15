library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

data <-
  readxl::read_xlsx("2-data/from_ruwen/HELIOS_Core_redacted.xlsx")

dictionary <-
  readxl::read_xlsx("2-data/from_ruwen/SG100K Data Dictionary - v44_redacted.xlsx")

load("3-data_analysis/1-data-preparation/1-phenotype-data/phenotype_data.rda")

dir.create(
  "3-data_analysis/1-data-preparation/2-clinical-lab-test-data",
  showWarnings = FALSE,
  recursive = TRUE
)
setwd("3-data_analysis/1-data-preparation/2-clinical-lab-test-data/")

data <-
  data %>%
  dplyr::rename(subject_id = "FREG0_PID")

data$sample_id <-
  paste(data$subject_id, data$FREG14_Visit_number, sep = "_")

data <-
  data %>%
  dplyr::select(subject_id, sample_id, everything())

data <-
  data %>%
  dplyr::select(subject_id:sample_id, DLAB15_Tc_Mmol_L:DLAB86_Oestrogen_E2) %>%
  dplyr::select(-DLAB51_Pbf)

expression_data <-
  data %>%
  dplyr::select(-c(subject_id:sample_id)) %>%
  as.data.frame()

rownames(expression_data) <- data$sample_id

expression_data <-
  expression_data %>%
  t() %>%
  as.data.frame()

sample_info <-
  phenotype_data

colnames(expression_data) == sample_info$sample_id

text_distance <-
  stringdist::stringdistmatrix(rownames(expression_data),
                               dictionary$`Variable Name - HELIOS Data Dictionary`) %>%
  apply(1, function(x) {
    which.min(x)
  })

data.frame(rownames(expression_data),
           dictionary$`Variable Name - HELIOS Data Dictionary`[text_distance])

rownames(expression_data) <-
  dictionary$`Variable Name - HELIOS Data Dictionary`[text_distance]

variable_info <-
  data.frame(variable_id = rownames(expression_data)) %>%
  dplyr::left_join(dictionary,
                   by = c("variable_id" = "Variable Name - HELIOS Data Dictionary"))

library(tidymass)

sample_info$class <- "Subject"

clinical_lab_test_data <-
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

save(clinical_lab_test_data, file = "clinical_lab_test_data.rda")
