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
