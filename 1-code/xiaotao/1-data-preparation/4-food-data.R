library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

library(tidymass)

load("3-data_analysis/1-data-preparation/1-phenotype-data/phenotype_data.rda")

dictionary <-
  readxl::read_xlsx("2-data/from_ruwen/SG100K Data Dictionary - v44_redacted.xlsx")

food_data1 <- readxl::read_xlsx("2-data/from_ruwen/HELIOS_FFQA.xlsx")
food_data2 <- readxl::read_xlsx("2-data/from_ruwen/HELIOS_FFQB.xlsx")

load("3-data_analysis/1-data-preparation/1-phenotype-data/phenotype_data.rda")

dir.create(
  "3-data_analysis/1-data-preparation/4-food-data",
  recursive = TRUE,
  showWarnings = FALSE
)

setwd("3-data_analysis/1-data-preparation/4-food-data/")

food_data1$FREG0_PID
food_data2$FREG0_PID

###food data1
food_data1 <-
  food_data1 %>%
  group_by(FREG0_PID) %>%
  mutate(FREG14_Visit_number = row_number()) %>%
  ungroup()

table(food_data1$FREG14_Visit_number)

sample_info <-
  food_data1 %>%
  dplyr::select(FREG0_PID, FREG14_Visit_number) %>%
  dplyr::rename(subject_id = FREG0_PID)

sample_info$sample_id <-
  paste0(sample_info$subject_id, "_", sample_info$FREG14_Visit_number)

sample_info %>%
  dplyr::count(sample_id) %>%
  dplyr::filter(n > 1)

sample_info <-
  sample_info %>%
  dplyr::left_join(phenotype_data,
                   by = c("sample_id", "subject_id", "FREG14_Visit_number"))

sum(is.na(sample_info$sample_id))

expression_data <-
  food_data1 %>%
  dplyr::select(-FREG0_PID, -FREG14_Visit_number) %>%
  t() %>%
  as.data.frame()

colnames(expression_data) <-
  sample_info$sample_id

variable_info <-
  data.frame(variable_id = rownames(expression_data))

sample_info$class <- "Subject"

match(variable_info$variable_id,
      dictionary$`Variable Name - HELIOS Data Dictionary`)

variable_info <-
  variable_info %>%
  dplyr::left_join(dictionary,
                   by = c("variable_id" = "Variable Name - HELIOS Data Dictionary"))

food_data1 <-
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

save(food_data1, file = "food_data1.rda")


###food data2
food_data2 <-
  food_data2 %>%
  group_by(FREG0_PID) %>%
  mutate(FREG14_Visit_number = row_number()) %>%
  ungroup()

table(food_data2$FREG14_Visit_number)

sample_info <-
  food_data2 %>%
  dplyr::select(FREG0_PID, FREG14_Visit_number) %>%
  dplyr::rename(subject_id = FREG0_PID, FREG14_Visit_number = FREG14_Visit_number)

sample_info$sample_id <-
  paste0(sample_info$subject_id, "_", sample_info$FREG14_Visit_number)

sample_info %>%
  dplyr::count(sample_id) %>%
  dplyr::filter(n > 1)

sample_info <-
  sample_info %>%
  dplyr::left_join(phenotype_data,
                   by = c("sample_id", "subject_id", "FREG14_Visit_number"))

sum(is.na(sample_info$sample_id))

expression_data <-
  food_data2 %>%
  dplyr::select(-FREG0_PID, -FREG14_Visit_number) %>%
  t() %>%
  as.data.frame()

colnames(expression_data) <-
  sample_info$sample_id

variable_info <-
  data.frame(variable_id = rownames(expression_data))

sample_info$class <- "Subject"

match(variable_info$variable_id,
      dictionary$`Variable Name - HELIOS Data Dictionary`)

variable_info <-
  variable_info %>%
  dplyr::left_join(dictionary,
                   by = c("variable_id" = "Variable Name - HELIOS Data Dictionary"))

food_data2 <-
  create_mass_dataset(
    expression_data = expression_data,
    sample_info = sample_info,
    variable_info = variable_info
  )

save(food_data2, file = "food_data2.rda")
