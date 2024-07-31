library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)
library(tidymass)

load(
  "3-data_analysis/1-data-preparation/3-gut-microbiome-data/gut_microbiome_data.rda"
)

dir.create("3-data_analysis/2-study-summary/3-microbiome-data-summary")
setwd("3-data_analysis/2-study-summary/3-microbiome-data-summary")

####remove some species who have more than 95% missing values
gut_microbiome_data <-
  gut_microbiome_data %>%
  activate_mass_dataset(what = "sample_info") %>%
  filter(!is.na(subject_id))

gut_microbiome_data <-
  gut_microbiome_data %>%
  mutate_variable_zero_freq()

plot_species_zero_freq <-
  gut_microbiome_data %>%
  activate_mass_dataset(what = "variable_info") %>%
  ggplot(aes(x = zero_freq)) +
  geom_histogram(color = "black", fill = ggsci::pal_flatui()(n = 6)[6]) +
  theme_base +
  labs(x = "Zero Frequency for Species", y = "Count")

plot_species_zero_freq

ggsave(
  plot_species_zero_freq,
  filename = "plot_species_zero_freq.png",
  width = 8,
  height = 6
)

ggsave(
  plot_species_zero_freq,
  filename = "plot_species_zero_freq.pdf",
  width = 8,
  height = 6
)

gut_microbiome_data <-
  gut_microbiome_data %>%
  activate_mass_dataset(what = "variable_info") %>%
  filter(zero_freq <= 0.95)

dim(gut_microbiome_data)

plot <-
  gut_microbiome_data %>%
  extract_variable_info() %>%
  dplyr::count(Kingdom) %>%
  mutate(
    Kingdom = case_when(
      Kingdom == "Bacteria" ~ "Bacteria",
      Kingdom == "Archaea" ~ "Archaea",
      Kingdom == "Viruses" ~ "Viruses",
      TRUE ~ "NA"
    )
  ) %>%
  mutate(percentage = n / sum(n) * 100) %>%
  ggplot(aes(x = "", y = n, fill = Kingdom)) +
  geom_bar(stat = "identity",
           width = 1,
           color = "black") +
  geom_text(aes(label = paste0(
    Kingdom, ": ", n, " (", round(percentage, 1), "%)"
  )),
  position = position_stack(vjust = 0.5),
  color = "white") +
  scale_fill_manual(values = microbe_class_color) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.title = element_blank())
plot
ggsave(plot,
       filename = "microbiome_kingdom.png",
       width = 7,
       height = 7)

ggsave(plot,
       filename = "microbiome_kingdom.pdf",
       width = 7,
       height = 7)

plot <-
  gut_microbiome_data %>%
  extract_variable_info %>%
  mutate(
    Kingdom = case_when(
      Kingdom == "Bacteria" ~ "Bacteria",
      Kingdom == "Archaea" ~ "Archaea",
      Kingdom == "Viruses" ~ "Viruses",
      TRUE ~ "NA"
    )
  ) %>%
  dplyr::mutate(Kingdom = factor(Kingdom, levels = c("Archaea", "Bacteria", "Viruses", "NA"))) %>%
  ggplot(aes(x = Kingdom, y = zero_freq)) +
  coord_cartesian(ylim = c(0.5, 1)) +
  geom_boxplot(aes(fill = Kingdom), outlier.shape = NA) +
  # geom_half_violin(aes(fill = Kingdom), side = "l") +
  # geom_half_boxplot(aes(fill = Kingdom), side = "r") +
  stat_summary(
    fun = median,
    geom = "point",
    position = position_dodge(width = 0.9),
    size = 3,
    aes(color = Kingdom)
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = median(y), label = paste0("Median: ", round(median(y), 2)))
    },
    geom = "text",
    vjust = -0.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = quantile(y, 0.25), label = paste0("25%: ", round(quantile(y, 0.25), 2)))
    },
    geom = "text",
    vjust = 1.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = quantile(y, 0.75), label = paste0("75%: ", round(quantile(y, 0.75), 2)))
    },
    geom = "text",
    vjust = -0.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  theme_base +
  labs(x = "", y = "Zero Frequency")

plot

ggsave(plot,
       filename = "kingdom_zero_freq.png",
       width = 8,
       height = 6)

ggsave(plot,
       filename = "kingdom_zero_freq.pdf",
       width = 8,
       height = 6)

plot <-
  gut_microbiome_data %>%
  mutate_mean_intensity() %>%
  extract_variable_info() %>%
  mutate(
    Kingdom = case_when(
      Kingdom == "Bacteria" ~ "Bacteria",
      Kingdom == "Archaea" ~ "Archaea",
      Kingdom == "Viruses" ~ "Viruses",
      TRUE ~ "NA"
    )
  ) %>%
  dplyr::mutate(Kingdom = factor(Kingdom, levels = c("Archaea", "Bacteria", "Viruses", "NA"))) %>%
  ggplot(aes(x = Kingdom, y = mean_intensity)) +
  geom_boxplot(aes(fill = Kingdom), outlier.shape = NA) +
  # geom_half_violin(aes(fill = Kingdom), side = "l") +
  # geom_half_boxplot(aes(fill = Kingdom), side = "r", outlier.shape = NA) +
  coord_cartesian(ylim = c(0, 350)) +
  stat_summary(
    fun = median,
    geom = "point",
    position = position_dodge(width = 0.9),
    size = 3,
    aes(color = Kingdom)
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = median(y), label = paste0("Median: ", round(median(y), 2)))
    },
    geom = "text",
    vjust = -0.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = quantile(y, 0.25), label = paste0("25%: ", round(quantile(y, 0.25), 2)))
    },
    geom = "text",
    vjust = 1.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  stat_summary(
    fun.data = function(y) {
      data.frame(y = quantile(y, 0.75), label = paste0("75%: ", round(quantile(y, 0.75), 2)))
    },
    geom = "text",
    vjust = -0.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  theme_base +
  labs(x = "", y = "Mean counts")

ggsave(plot,
       filename = "kingdom_mean_count.png",
       width = 8,
       height = 6)

ggsave(plot,
       filename = "kingdom_mean_count.pdf",
       width = 8,
       height = 6)


library(microbiomeplot)

# gut_microbiome_data %>%
#   activate_microbiome_dataset(what = "variable_info") %>%
#   filter(Kingdom == "Bacteria") %>%
#   plot_circlepack(label_level = "Class")

##barplot
plot <-
  gut_microbiome_data %>%
  plot_barplot(
    fill = "Phylum",
    relative = TRUE,
    re_calculate_relative = TRUE,
    show.legend = TRUE,
    color = NA,
    top_n = 3
  ) +
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  labs("Participant") +
  ggsci::scale_fill_aaas()
plot

ggsave(plot,
       filename = "top3_phylum_barplot.png",
       width = 10,
       height = 5)
ggsave(plot,
       filename = "top3_phylum_barplot.pdf",
       width = 10,
       height = 5)

####physeq tree
##https://bioconductor.org/packages/release/bioc/vignettes/MicrobiotaProcess/inst/doc//MicrobiotaProcess.html
