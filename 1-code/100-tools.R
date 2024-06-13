library(tidyverse)
library(ggplot2)

library(ggsci)
library(scales)

gender_colors <- c("F" = "#00A1D5FF", "M" = "#B24745FF")

ethnicity_colors <-
  c(
    "C" = "#B24745FF",
    "I" = "#79AF97FF",
    "M" = "#DF8F44FF",
    "O" = "#80796BFF"
  )

#
#
# scales::show_col(ggsci::pal_jama()(7))
# ggsci::pal_jama()(7)

theme_base <-
  theme_bw() +
  theme(
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.x = element_text(size = 14),
    axis.title.y = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 14),
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(0.5, "cm"),
    legend.key.height = unit(0.5, "cm"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14, hjust = 0.5)
  )