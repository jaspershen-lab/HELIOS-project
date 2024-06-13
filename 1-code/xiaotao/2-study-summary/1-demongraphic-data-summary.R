library(r4projects)
setwd(get_project_wd())
rm(list = ls())
source('1-code/100-tools.R')
library(tidyverse)

library(tidymass)

load(
  "3-data_analysis/1-data-preparation/3-gut-microbiome-data/gut_microbiome_data.rda"
)

dir.create("3-data_analysis/2-study-summary")
setwd("3-data_analysis/2-study-summary/")

sample_info <-
  extract_sample_info(gut_microbiome_data)

sample_info <-
  sample_info %>%
  dplyr::filter(!is.na(subject_id))

sample_info %>%
  count(subject_id) %>%
  filter(n > 1)

sample_info %>%
  dplyr::filter(subject_id == "SHA-1577-4")

subject_info <-
  sample_info %>%
  dplyr::distinct(subject_id, .keep_all = TRUE)

###demographic data
#####circos plot
library(circlize)

##age, gender, ethnic_group, bmi
subject_info$height
subject_info$weight
subject_info$bmi <-
  subject_info$weight / (subject_info$height / 100) ^ 2

df <-
  data.frame(
    factors = subject_info$subject_id,
    x = 1,
    y = 1,
    subject_info,
    stringsAsFactors = TRUE
  ) %>%
  # dplyr::arrange(age) %>%
  dplyr::mutate(factors = factor(factors, levels = factors))

circos.par(
  "track.height" = 0.2,
  start.degree = 90,
  clock.wise = TRUE,
  gap.after = c(rep(0, nrow(df) - 1), 90),
  cell.padding = c(0, 0, 0, 0)
)

circos.initialize(factors = df$factors,
                  x = df$x,
                  xlim = c(0.5, 1.5))

##age
range(df$age, na.rm = TRUE)
temp_value <- df$age

circos.track(
  factors = df$factors,
  # x = df$x,
  y = temp_value,
  ylim = c(0.8 * min(temp_value), 1.1 * max(temp_value, na.rm = TRUE)),
  bg.border = "black",
  # bg.col = NA,
  track.height = 0.2,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    circos.yaxis(
      side = "left",
      at = c(0.8 * min(temp_value), round((
        min(temp_value, na.rm = TRUE) + max(temp_value, na.rm = TRUE)
      ) / 2, 2), round(max(
        temp_value, na.rm = TRUE
      ), 2)),
      sector.index = get.all.sector.index()[1],
      labels.cex = 0.4,
      labels.niceFacing = FALSE
    )
    
    circos.lines(
      x = mean(xlim, na.rm = TRUE),
      y =  temp_value[i],
      pch = 16,
      cex = 8,
      type = "h",
      col = ggsci::pal_aaas()(n = 10)[4],
      lwd = 2
    )
    
    #plot country labels
    # circos.text(
    #   x = 1,
    #   y = 105,
    #   labels = name,
    #   facing = "clockwise",
    #   niceFacing = TRUE,
    #   cex = 0.5
    #   # adj = aa
    # )
    
    # circos.points(
    #   x = mean(xlim),
    #   y =  temp_value[i],
    #   pch = 16,
    #   cex = 0.8,
    #   col = ggsci::pal_aaas()(n = 10)[4]
    # )
  }
)

##BMI
range(df$bmi, na.rm = TRUE)
temp_value <- df$bmi

circos.track(
  factors = df$factors,
  # x = df$x,
  y = temp_value,
  ylim = c(
    0.8 * min(temp_value, na.rm = TRUE),
    1.1 * max(temp_value, na.rm = TRUE)
  ),
  bg.border = "black",
  # bg.col = NA,
  track.height = 0.2,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    circos.yaxis(
      side = "left",
      at = c(
        0.8 * min(temp_value, na.rm = TRUE),
        round((
          min(temp_value, na.rm = TRUE) + max(temp_value, na.rm = TRUE)
        ) / 2, 2),
        round(max(temp_value, na.rm = TRUE), 2)
      ),
      sector.index = get.all.sector.index()[1],
      labels.cex = 0.4,
      labels.niceFacing = FALSE
    )
    
    circos.lines(
      x = mean(xlim, na.rm = TRUE),
      y =  temp_value[i],
      pch = 16,
      cex = 8,
      type = "h",
      col = ggsci::pal_tron()(n = 10)[1],
      lwd = 2
    )
    
    # circos.points(
    #   x = mean(xlim),
    #   y =  temp_value[i],
    #   pch = 16,
    #   cex = 0.8,
    #   col = ggsci::pal_tron()(n = 10)[1]
    # )
  }
)

## sex
temp_sex <- df$gender
temp_sex[is.na(temp_sex)] <- "grey"
temp_sex[temp_sex == "F"] <- gender_colors["F"]
temp_sex[temp_sex == "M"] <- gender_colors["M"]

circos.track(
  factors = df$factors,
  # x = df$x,
  y = df$y,
  ylim = c(0, 1),
  bg.border = NA,
  # bg.col = NA,
  track.height = 0.1,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    #text direction (dd) and adjusmtents (aa)
    theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd <-
      ifelse(theta < 90 ||
               theta > 270, "clockwise", "reverse.clockwise")
    aa = c(0.5, 1)
    # if(theta < 90 || theta > 270)  aa = c(0, 0.5)
    
    circos.rect(
      xleft = xlim[1],
      ybottom = ylim[1],
      xright = xlim[2],
      ytop = ylim[2],
      col = temp_sex[i],
      border = NA
    )
  }
)

## Ethnicity
temp_ethnicity <- df$ethnic_group
# temp_ethnicity[is.na(temp_ethnicity)] <- "grey"
temp_ethnicity[temp_ethnicity == "Caucasian"] <-
  ethnicity_colors["Caucasian"]
temp_ethnicity[temp_ethnicity == "C"] <-
  ethnicity_colors["C"]
temp_ethnicity[temp_ethnicity == "I"] <-
  ethnicity_colors["I"]
temp_ethnicity[temp_ethnicity == "M"] <-
  ethnicity_colors["M"]
temp_ethnicity[temp_ethnicity == "O"] <-
  ethnicity_colors["O"]

circos.track(
  factors = df$factors,
  # x = df$x,
  y = df$y,
  ylim = c(0, 1),
  bg.border = NA,
  # bg.col = NA,
  track.height = 0.1,
  panel.fun = function(x, y) {
    name = get.cell.meta.data("sector.index")
    i = get.cell.meta.data("sector.numeric.index")
    xlim = get.cell.meta.data("xlim")
    ylim = get.cell.meta.data("ylim")
    
    #text direction (dd) and adjusmtents (aa)
    theta = circlize(mean(xlim), 1.3)[1, 1] %% 360
    dd <-
      ifelse(theta < 90 ||
               theta > 270, "clockwise", "reverse.clockwise")
    aa = c(0.5, 1)
    # if(theta < 90 || theta > 270)  aa = c(0, 0.5)
    #plot country labels
    
    circos.rect(
      xleft = xlim[1],
      ybottom = ylim[1],
      xright = xlim[2],
      ytop = ylim[2],
      col = temp_ethnicity[i],
      border = NA
    )
  }
)

###age
#####age
age <-
  df$age
library(gghalves)
plot_age <-
  age %>%
  data.frame(class = "class", value = .) %>%
  ggplot(aes(x = class, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_dotplot(
    binaxis = "y",
    color = ggsci::pal_aaas()(n = 10)[4],
    fill = ggsci::pal_aaas()(n = 10)[4],
    shape = 16,
    binwidth = 1,
    stackdir = "center"
  ) +
  theme_bw() +
  labs(x = "", y = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
plot_age

# ggsave(plot_age,
#        filename = "plot_age.pdf",
#        width = 3,
#        height = 10)

# ggsave(plot_age,
#        filename = "plot_age.png",
#        width = 3,
#        height = 10)

###BMI

bmi <-
  df$bmi

plot_bmi <-
  bmi %>%
  data.frame(class = "class", value = .) %>%
  ggplot(aes(x = class, y = value)) +
  geom_boxplot(outlier.shape = NA) +
  geom_dotplot(
    binaxis = "y",
    color = ggsci::pal_tron()(n = 10)[1],
    fill = ggsci::pal_tron()(n = 10)[1],
    shape = 16,
    binwidth = 0.6,
    stackdir = "center"
  ) +
  theme_bw() +
  labs(x = "", y = "") +
  # scale_y_continuous(expand = expansion(mult = c(0.1, 0.1))) +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
plot_bmi

# ggsave(plot_bmi,
#        filename = "plot_bmi.pdf",
#        width = 3,
#        height = 10)
# ggsave(plot_bmi,
#        filename = "plot_bmi.png",
#        width = 3,
#        height = 10)


##sex

sex <-
  df$gender

plot_sex <-
  sex %>%
  data.frame(class = "class", value = .) %>%
  dplyr::mutate(value = factor(value, levels = c("F", "M"))) %>%
  ggplot(aes(x = class)) +
  geom_bar(
    aes(fill = value),
    color = "black",
    position = "stack",
    show.legend = FALSE,
    width = 2
  ) +
  scale_fill_manual(values = gender_colors) +
  theme_bw() +
  labs(x = "", y = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plot_sex

# ggsave(plot_sex,
#        filename = "plot_sex.pdf",
#        width = 1.5,
#        height = 10)
# ggsave(plot_sex,
#        filename = "plot_sex.png",
#        width = 1.5,
#        height = 10)


##ethnicity
ethnicity <-
  df$ethnic_group

plot_ethnicity <-
  ethnicity %>%
  data.frame(class = "class", value = .) %>%
  dplyr::mutate(value = factor(value, levels = c("C", "M", "I", "O"))) %>%
  ggplot(aes(x = class)) +
  geom_bar(
    aes(fill = value),
    color = "black",
    position = "stack",
    show.legend = FALSE,
    width = 2
  ) +
  scale_fill_manual(values = ethnicity_colors) +
  theme_bw() +
  labs(x = "", y = "") +
  scale_y_continuous(expand = expansion(mult = c(0, 0))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )

plot_ethnicity

# ggsave(
#   plot_ethnicity,
#   filename = "plot_ethnicity.pdf",
#   width = 1.5,
#   height = 10
# )

# ggsave(
#   plot_ethnicity,
#   filename = "plot_ethnicity.png",
#   width = 1.5,
#   height = 10
# )

###age and sex distributation
plot <-
  df %>%
  ggplot() +
  geom_histogram(aes(x = age, fill = gender),
                 binwidth = 2,
                 color = "black") +
  scale_fill_manual(values = gender_colors) +
  theme_bw() +
  labs(x = "Age (yreas)", y = "Count") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))
plot
ggsave(plot,
       filename = "age_sex.pdf",
       width = 7,
       height = 7)

###age and ethnicity distributation
plot <-
  df %>%
  ggplot() +
  geom_histogram(aes(x = age, fill = ethnic_group),
                 binwidth = 2,
                 color = "black") +
  scale_fill_manual(values = ethnicity_colors) +
  theme_bw() +
  labs(x = "Age (yreas)", y = "Count") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1))

plot
# ggsave(plot,
#        filename = "age_ethnicity.pdf",
#        width = 7,
#        height = 7)

###age and BMI distributation
plot <-
  df %>%
  ggplot(aes(age, bmi)) +
  geom_point(aes(age, bmi), size = 5) +
  theme_bw() +
  geom_smooth(method = "lm", color = "red") +
  labs(x = "Age (yreas)", y = "BMI") +
  theme(legend.position = c(0, 1),
        legend.justification = c(0, 1)) +
  geom_text(
    label = paste(
      "Correlation:",
      cor.test(df$age, df$bmi, method = "spearman")$estimate,
      "\nP-value:",
      cor.test(df$age, df$bmi, method = "spearman")$p.value
    ),
    x = Inf,
    y = Inf,
    hjust = 1.1,
    vjust = 1.1,
    size = 5
  )

plot

cor.test(df$age, df$bmi, method = "spearman")

ggsave(plot,
       filename = "age_bmi.pdf",
       width = 7,
       height = 7)

###all the other demongraphics data
colnames(df)

##heatmap to show the demongraphis data
###age, gender, ethnic_group, bmi, waist, hip, Sbp, dbp
##hr (heart rate),

# df %>%
#   dplyr::mutate(sample_id = factor(sample_id)) %>%
#   ggplot(aes(x = age, y = sample_id)) +
#   # geom_line(aes(y = sample_id, x = age, group = sample_id), color = "black") +
#   geom_segment(aes(
#     x = 0,
#     y = sample_id,
#     xend = age,
#     yend = sample_id
#   )) +
#   labs(x = "Age (years)", y = "Subjects") +
#   theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())


library(gghalves)

plot_age <-
  df %>%
  ggplot(aes(x = "class", y = age)) +
  geom_half_violin(fill = ggsci::pal_aaas()(n = 10)[4], 
                   color = "black",
                   alpha = 0.5) +
  geom_half_boxplot(
    fill = ggsci::pal_aaas()(n = 10)[4],
    alpha = 0.5,
    color = "black",
    side = "r"
  ) +
  theme_bw() +
  labs(x = "", y = "Age (years)") +
  scale_x_discrete(expand = expansion(mult = c(0, 0))) +
  theme(
    panel.grid = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank()
  )
plot_age
