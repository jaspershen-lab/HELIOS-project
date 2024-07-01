library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

library(tidymass)

load(
  "3-data_analysis/1-data-preparation/2-clinical-lab-test-data/clinical_lab_test_data.rda"
)

load(
  "3-data_analysis/1-data-preparation/3-gut-microbiome-data/gut_microbiome_data.rda"
)

dir.create("3-data_analysis/2-study-summary/2-clinical-lab-test-summary")
setwd("3-data_analysis/2-study-summary/2-clinical-lab-test-summary")

sample_info <-
  extract_sample_info(gut_microbiome_data)

sample_info <-
  sample_info %>%
  dplyr::filter(!is.na(subject_id))

clinical_lab_test_data <-
  clinical_lab_test_data %>%
  activate_mass_dataset(what = "sample_info") %>%
  dplyr::filter(sample_id %in% sample_info$sample_id_old)

####missing value imputation
clinical_lab_test_data %>%
  extract_expression_data() %>%
  apply(1, function(x)
    sum(is.na(x)) / ncol(clinical_lab_test_data)) %>%
  `>`(0.5) %>%
  which()

clinical_lab_test_data <- clinical_lab_test_data[-c(35, 36, 37), ]

# clinical_lab_test_data <-
#   clinical_lab_test_data %>%
#   masscleaner::impute_mv(method = "zero")

expression_data <-
  clinical_lab_test_data %>%
  masscleaner::impute_mv(method = "zero") %>%
  massstat::scale_data() %>%
  extract_expression_data()

library(ComplexHeatmap)

which(is.na(expression_data), arr.ind = TRUE) %>%
  as.data.frame() %>%
  pull(row) %>%
  table()

library(circlize)
library(viridis)
col_fun <- circlize::colorRamp2(seq(-2, 2, length.out = 11), rev(RColorBrewer::brewer.pal(name = "RdYlBu", n = 11)))
na_col <- "grey"

range(expression_data, na.rm = TRUE)

expression_data[expression_data < -2] <- -2
expression_data[expression_data > 2] <- 2

rownames(expression_data) == clinical_lab_test_data@variable_info$variable_id
rownames(expression_data) <- clinical_lab_test_data@variable_info$`Description of variable`

plot <-
  Heatmap(
    as.matrix(t(expression_data)),
    name = "Z-score",
    show_row_names = FALSE,
    show_column_names = TRUE,
    col = col_fun,
    na_col = na_col,
    cluster_columns = TRUE,
    cluster_rows = TRUE,
    column_names_gp = gpar(fontsize = 6),
    row_names_gp = gpar(fontsize = 6),
    column_names_rot = 45,
    clustering_distance_columns = "euclidean",
    clustering_method_columns = "complete",
    clustering_distance_rows = "euclidean",
    clustering_method_rows = "complete"
  )

plot <- ggplotify::as.ggplot(plot)
plot
ggsave(plot,
       filename = "clinical_heatmap.png",
       width = 5,
       height = 10)

ggsave(plot,
       filename = "clinical_heatmap.pdf",
       width = 5,
       height = 10)

####box plot for each clinical lab test
expression_data <-
  clinical_lab_test_data %>%
  extract_expression_data()

rownames(expression_data) <- clinical_lab_test_data@variable_info$`Description of variable`

variable_info <-
  clinical_lab_test_data@variable_info

dir.create("individual_boxplot")

for (i in 1:nrow(expression_data)) {
  cat(i, " ")
  temp_data <-
    expression_data[i, ] %>%
    t() %>%
    as.data.frame() %>%
    dplyr::mutate(class = "class")
  colnames(temp_data)[1] <- "value"
  
  plot <-
    temp_data %>%
    ggplot(aes(x = "class", y = value)) +
    geom_half_violin(
      fill = ggsci::pal_cosmic()(n = 10)[2],
      color = "black",
      alpha = 0.5
    ) +
    geom_half_boxplot(
      fill = ggsci::pal_cosmic()(n = 10)[2],
      alpha = 0.5,
      color = "black",
      side = "r"
    ) +
    stat_summary(
      fun.data = function(y) {
        return(data.frame(y = median(y), label = paste("Median:", round(median(
          y
        ), 2))))
      },
      geom = "text",
      vjust = -1.5,
      color = "black"
    ) +
    stat_summary(
      fun.data = function(y) {
        return(data.frame(y = quantile(y, 0.25), label = paste("25%:", round(
          quantile(y, 0.25), 2
        ))))
      },
      geom = "text",
      vjust = 1.5,
      hjust = -0.2,
      color = "black"
    ) +
    stat_summary(
      fun.data = function(y) {
        return(data.frame(y = quantile(y, 0.75), label = paste("75%:", round(
          quantile(y, 0.75), 2
        ))))
      },
      geom = "text",
      vjust = -1.5,
      hjust = 1.2,
      color = "black"
    ) +
    stat_summary(
      fun.data = function(y) {
        return(data.frame(y = min(y), label = paste("Min:", round(min(
          y
        ), 2))))
      },
      geom = "text",
      vjust = 1.5,
      hjust = -1,
      color = "black"
    ) +
    stat_summary(
      fun.data = function(y) {
        return(data.frame(y = max(y), label = paste("Max:", round(max(
          y
        ), 2))))
      },
      geom = "text",
      vjust = -1.5,
      hjust = 1.5,
      color = "black"
    ) +
    theme_base +
    labs(x = "", y = rownames(expression_data)[i]) +
    scale_x_discrete(expand = expansion(mult = c(0, 0))) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank()
    )
  plot
  
  ggsave(
    plot,
    filename = file.path(
      "individual_boxplot",
      paste(variable_info$variable_id[i], ".pdf")
    ),
    width = 4,
    height = 7
  )
  
  ggsave(
    plot,
    filename = file.path(
      "individual_boxplot",
      paste(variable_info$variable_id[i], ".png")
    ),
    width = 4,
    height = 7
  )
}

####total plot
plot <-
  expression_data %>%
  tibble::rownames_to_column(var = "variable_id") %>%
  tidyr::pivot_longer(-variable_id, names_to = "sample_id", values_to = "value") %>%
  ggplot(aes(x = "class", y = value)) +
  geom_violin(
    fill = ggsci::pal_cosmic()(n = 10)[2],
    color = "black",
    alpha = 0.5
  ) +
  facet_wrap(~ variable_id, scales = "free_y", nrow = 7) +
  geom_text(
    stat = "summary",
    fun = median,
    aes(label = paste0("Median: ", round(..y.., 2))),
    vjust = -0.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  geom_text(
    stat = "summary",
    fun = function(y)
      quantile(y, 0.25),
    aes(label = paste0("25%: ", round(..y.., 2))),
    vjust = 1.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  geom_text(
    stat = "summary",
    fun = function(y)
      quantile(y, 0.75),
    aes(label = paste0("75%: ", round(..y.., 2))),
    vjust = -0.5,
    hjust = 0.5,
    color = "black",
    size = 3
  ) +
  theme_base +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_text(size = 9),
    axis.ticks.x = element_blank()
  ) +
  labs(x = "", y = "")
plot
ggsave(plot,
       filename = "total_boxplot.pdf",
       width = 10,
       height = 12)
ggsave(plot,
       filename = "total_boxplot.png",
       width = 10,
       height = 12)
