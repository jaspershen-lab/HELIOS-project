library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

library(tidymass)

load("3-data_analysis/1-data-preparation/1-phenotype-data/phenotype_data.rda")

dictionary <-
  readxl::read_xlsx("2-data/from_ruwen/SG100K Data Dictionary - v44_redacted.xlsx")

###plylum
phylum_data <-
  readr::read_csv(
    "2-data/from_ruwen/abundance_bracken_standard_16_20221209_taxa_P_unfiltered_reads_decont2.csv"
  ) %>%
  as.data.frame() %>%
  dplyr::rename(sample_id = "SAM id")

###genus
genus_data <-
  readr::read_csv(
    "2-data/from_ruwen/abundance_bracken_standard_16_20221209_taxa_G_unfiltered_reads_decont2.csv"
  ) %>%
  as.data.frame() %>%
  dplyr::rename(sample_id = "SAM id")

###genus
species_data <-
  readr::read_csv(
    "2-data/from_ruwen/abundance_bracken_standard_16_20221209_taxa_S_unfiltered_reads_decont2.csv"
  ) %>%
  as.data.frame() %>%
  dplyr::rename(sample_id = "SAM id")

dir.create(
  "3-data_analysis/1-data-preparation/3-gut-microbiome-data",
  recursive = TRUE,
  showWarnings = FALSE
)

setwd("3-data_analysis/1-data-preparation/3-gut-microbiome-data/")

# ###remove some species, less than 10 samples
# zero_percentage <-
#   apply(species_data[, -1], 2, function(x) {
#     sum(x == 0) / nrow(species_data)
#   })

library("taxize")

species_name <- colnames(species_data)[-1]

final_info <-
  vector(mode = "list", length = length(species_name))

# for (i in 1:length(species_name)) {
#   cat(i, " ")
#   tax_id <- get_uid(sci_com = species_name[i], db = "ncbi")
#   taxonomy_info <- classification(tax_id, db = "ncbi")
#   final_info[[i]] <-
#     tryCatch(
#       expr = {
#         taxonomy_info[[1]] %>%
#           dplyr::filter(rank != "no rank") %>%
#           dplyr::select(name, rank) %>%
#           as.data.frame() %>%
#           dplyr::arrange(desc(row_number())) %>%
#           tibble::column_to_rownames("rank") %>%
#           t() %>%
#           as.data.frame()
#       },
#       error = function(e) {
#         data.frame(species = species_name[i], matrix(
#           NA,
#           nrow = 1,
#           ncol = 6,
#           dimnames = list(
#             NULL,
#             c(
#               "genus",
#               "family",
#               "order",
#               "class",
#               "phylum",
#               "superkingdom"
#             )
#           )
#         ))
#       }
#     )
# }

# save(final_info, file = "final_info.rda")
load("final_info.rda")

final_info[[1]]
length(final_info)

final_info %>%
  lapply(nrow) %>%
  unlist() %>%
  table()

final_info %>%
  lapply(ncol) %>%
  unlist() %>%
  table()

final_info %>%
  lapply(ncol) %>%
  unlist() %>%
  `<`(7) %>%
  which()

final_info[[416]]

required_columns <- c("species",
                      "genus",
                      "family",
                      "order",
                      "class",
                      "phylum",
                      "superkingdom")

final_info2 <-
  final_info %>%
  purrr::map(function(x) {
    x <-
      x[, colnames(x) %in% required_columns, drop = FALSE]
    cat(x$species, " ")
    if (ncol(x) == 7) {
      return(x)
    }
    
    if (ncol(x) < 7) {
      missing_columns <- setdiff(required_columns, colnames(x))
      x[, missing_columns] <- NA
      return(x[, required_columns])
    }
    
    if (ncol(x) > 7) {
      x %>%
        dplyr::select(species, genus, family, order, class, phylum, superkingdom) %>%
        return()
    }
    
  })

final_info2 <-
  final_info2 %>%
  do.call(rbind, .) %>%
  as.data.frame()

sum(final_info2$species == species_name, na.rm = TRUE)
length(species_name)
which(final_info2$species != species_name)

final_info2$species <-
  species_name

final_info2

library(microbiomedataset)

? create_microbiome_dataset

variable_info <-
  final_info2 %>%
  dplyr::select(rev(required_columns))

colnames(variable_info) <-
  c("Kingdom",
    "Phylum",
    "Class",
    "Order",
    "Family",
    "Genus",
    "Species")

expression_data <-
  species_data %>%
  tibble::column_to_rownames("sample_id") %>%
  t() %>%
  as.data.frame()

sum(rownames(expression_data) == variable_info$Species)

rownames(expression_data) <-
  paste("OTU", 1L:nrow(expression_data), sep = "_")

variable_info$variable_id <-
  rownames(expression_data)

sample_info <-
  data.frame(sample_id = colnames(expression_data)) %>%
  dplyr::left_join(phenotype_data, by = c("sample_id" = "library_id")) %>%
  dplyr::rename(sample_id_old = "sample_id.y")

colnames(phylum_data)[-1] %in%
  unique(variable_info$Phylum) %>%
  sum()

dim(phylum_data)

sort(setdiff(unique(variable_info$Phylum), colnames(phylum_data)[-1]))
sort(setdiff(colnames(phylum_data)[-1], unique(variable_info$Phylum)))

colnames(genus_data)[-1] %in%
  unique(variable_info$Genus) %>%
  sum()

dim(genus_data)

head(sort(setdiff(
  unique(variable_info$Genus), colnames(genus_data)[-1]
)))
head(sort(setdiff(
  colnames(genus_data)[-1], unique(variable_info$Genus)
)))

sample_info$class <- "Subject"

head(sample_info)

gut_microbiome_data <-
  create_microbiome_dataset(
    expression_data = expression_data,
    variable_info = variable_info,
    sample_info = sample_info
  )

getwd()

gut_microbiome_data <-
  gut_microbiome_data %>%
  activate_mass_dataset(what = "sample_info") %>%
  filter(!is.na(subject_id))

save(gut_microbiome_data, file = "gut_microbiome_data.rda")

##check if the data is same to the data from ruwen

test <-
  gut_microbiome_data %>%
  activate_microbiome_dataset(what = "variable_info") %>%
  dplyr::filter(!is.na(Genus)) %>%
  microbiomedataset::summarise_variables(group_by = "Genus")

test@variable_info

intersected_genus <-
  intersect(unique(test@variable_info$Genus), colnames(genus_data[-1]))

data1 <-
  genus_data %>%
  dplyr::select("sample_id", intersected_genus[6])

data <-
  test %>%
  extract_expression_data() %>%
  t() %>%
  as.data.frame()

colnames(data) <-
  test@variable_info$Genus

data2 <-
  data %>%
  dplyr::select(intersected_genus[6]) %>%
  tibble::rownames_to_column("sample_id")

temp_data <-
  data1 %>%
  dplyr::left_join(data2, by = "sample_id")

head(temp_data)

plot(temp_data[, 2], temp_data[, 3])
