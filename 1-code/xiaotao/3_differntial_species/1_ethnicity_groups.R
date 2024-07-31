library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)
library(tidymass)

load(
  "3-data_analysis/1-data-preparation/3-gut-microbiome-data/gut_microbiome_data.rda"
)

dir.create("3-data_analysis/3_differential_species/1_ethnicity_groups",
           recursive = TRUE)
setwd("3-data_analysis/3_differential_species/1_ethnicity_groups")

####remove some species who have more than 95% missing values

dim(gut_microbiome_data)

gut_microbiome_data <-
  gut_microbiome_data %>%
  activate_mass_dataset(what = "sample_info") %>%
  filter(!is.na(subject_id))

###translate them to relative abundance
gut_microbiome_data <-
  gut_microbiome_data %>%
  microbiomedataset::transform2relative_intensity()

dim(gut_microbiome_data)

gut_microbiome_data <-
  gut_microbiome_data %>%
  mutate_variable_zero_freq()

gut_microbiome_data <-
  gut_microbiome_data %>%
  activate_mass_dataset(what = "variable_info") %>%
  filter(zero_freq < 0.8)

dim(gut_microbiome_data)

library(microbiomeplot)

###
library(phyloseq)
library(vegan)
library(ggplot2)

library(microbiomedataset)

###

gut_microbiome_data <-
  gut_microbiome_data %>%
  activate_microbiome_dataset(what = "sample_info") %>%
  dplyr::rename(ethnicity = FREG5_Ethnic_Group) %>%
  dplyr::filter(ethnicity != "O") %>%
  activate_microbiome_dataset(what = "sample_info") %>%
  dplyr::arrange(ethnicity)

physeq <-
  gut_microbiome_data %>%
  microbiomedataset::convert2phyloseq()

# Calculate a distance matrix using Bray-Curtis distance
bray_dist <- distance(physeq, method = "bray")

# Perform PCoA
pcoa_results <- ordinate(physeq, method = "PCoA", distance = bray_dist)

# Plot the PCoA results
pcoa_plot <-
  plot_ordination(physeq, pcoa_results, color = "ethnicity") +
  geom_point(size = 3) +
  ggtitle("PCoA of Bray-Curtis distances") +
  theme_base +
  scale_color_manual(values = ethnicity_colors)

# Print the plot
pcoa_plot

ggsave(pcoa_plot,
       filename = "pcoa_plot.pdf",
       width = 8,
       height = 7)

####find the differentiation expression species for ethnicity
####https://www.nature.com/articles/s41467-022-28034-z

expression_data <-
  gut_microbiome_data@expression_data

sample_info <-
  gut_microbiome_data@sample_info

variable_info <-
  gut_microbiome_data@variable_info

# ###Chinese vs India
# fc_p_data <-
#   purrr::map(1:nrow(expression_data), function(i) {
#     cat(i, " ")
#     chinese_index <- which(sample_info$ethnicity == "C")
#     india_index <- which(sample_info$ethnicity == "I")
#     malay_index <- which(sample_info$ethnicity == "M")
#     chinese_india_test <-
#       wilcox.test(as.numeric(expression_data[i, chinese_index]),
#                   as.numeric(expression_data[i, india_index]))
#
#     chinese_malay_test <-
#       wilcox.test(as.numeric(expression_data[i, chinese_index]),
#                   as.numeric(expression_data[i, malay_index]))
#
#     india_malay_test <-
#       wilcox.test(as.numeric(expression_data[i, india_index]),
#                   as.numeric(expression_data[i, malay_index]))
#
#     chinese_india_fc <-
#       median(as.numeric(expression_data[i, chinese_index])) /
#       median(as.numeric(expression_data[i, india_index]))
#
#     chinese_malay_fc <-
#       median(as.numeric(expression_data[i, chinese_index])) /
#       median(as.numeric(expression_data[i, malay_index]))
#
#     india_malay_fc <-
#       median(as.numeric(expression_data[i, india_index])) /
#       median(as.numeric(expression_data[i, malay_index]))
#
#
#     data.frame(
#       variable_id = variable_info$variable_id[i],
#       chinese_india_p = chinese_india_test$p.value,
#       chinese_malay_p = chinese_malay_test$p.value,
#       india_malay_p = india_malay_test$p.value,
#       chinese_india_fc = chinese_india_fc,
#       chinese_malay_fc = chinese_malay_fc,
#       india_malay_fc = india_malay_fc
#     )
#
#   }) %>%
#   do.call(rbind, .) %>%
#   as.data.frame()
#
# fc_p_data$chinese_india_fdr <- p.adjust(fc_p_data$chinese_india_p, method = "fdr")
# fc_p_data$chinese_malay_fdr <- p.adjust(fc_p_data$chinese_malay_p, method = "fdr")
# fc_p_data$india_malay_fdr <- p.adjust(fc_p_data$india_malay_p, method = "fdr")
#
# sum(fc_p_data$chinese_india_fdr < 0.05)
# sum(fc_p_data$chinese_malay_fdr < 0.05)
# sum(fc_p_data$india_malay_fdr < 0.05)
#
#
# fc_p_data$chinese_india_fc[is.na(fc_p_data$chinese_india_fc)] <- 1
# fc_p_data$chinese_malay_fc[is.na(fc_p_data$chinese_malay_fc)] <- 1
# fc_p_data$india_malay_fc[is.na(fc_p_data$india_malay_fc)] <- 1
# save(fc_p_data, file = "fc_p_data.rda")
load("fc_p_data.rda")

###Heatmap
library(ComplexHeatmap)

marker_index <-
  unique(c(
    which(fc_p_data$chinese_india_fdr < 0.05 &
            abs(log(
              fc_p_data$chinese_india_fc, 2
            )) > 1),
    which(fc_p_data$chinese_malay_fdr < 0.05 &
            abs(log(
              fc_p_data$chinese_malay_fc, 2
            )) > 1),
    which(fc_p_data$india_malay_fdr < 0.05 &
            abs(log(
              fc_p_data$india_malay_fc, 2
            )) > 1)
  ))

####PCoA
gut_microbiome_data2 <-
  gut_microbiome_data[marker_index, ]

physeq2 <-
  gut_microbiome_data2 %>%
  microbiomedataset::convert2phyloseq()

# Calculate a distance matrix using Bray-Curtis distance
bray_dist <- distance(physeq2, method = "bray")

# Perform PCoA
pcoa_results <- ordinate(physeq2, method = "PCoA", distance = bray_dist)

# Plot the PCoA results
pcoa_plot2 <-
  plot_ordination(physeq2, pcoa_results, color = "ethnicity") +
  geom_point(size = 3) +
  ggtitle("PCoA of Bray-Curtis distances") +
  theme_base +
  scale_color_manual(values = ethnicity_colors)

# Print the plot
pcoa_plot2

ggsave(pcoa_plot2,
       filename = "pcoa_plot2.pdf",
       width = 8,
       height = 7)


###heatmap
temp_data <-
  expression_data[marker_index, ] %>%
  as.data.frame() %>%
  apply(1, function(x) {
    (x - mean(x)) / sd(x)
  }) %>%
  t()

range(temp_data)

temp_data[temp_data > 1] <- 1
temp_data[temp_data < -1] <- -1

# Create column annotations
column_annotation <- HeatmapAnnotation(
  Group = sample_info$ethnicity,
  col = list(Group = ethnicity_colors),
  show_annotation_name = TRUE,
  annotation_name_side = "left",
  annotation_label = "Ethnicity"
)

plot <-
  Heatmap(
    matrix = as.matrix(temp_data),
    cluster_columns = FALSE,
    cluster_rows = TRUE,
    show_row_names = FALSE,
    show_column_names = FALSE,
    column_split = sample_info$ethnicity,
    border = TRUE,
    top_annotation = column_annotation,
  )

plot <-
  ggplotify::as.ggplot(plot)

plot

ggsave(plot,
       filename = "heatmap.pdf",
       width = 10,
       height = 7)
