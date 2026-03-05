# Analysis: Updating parameter plots by race across age groups
# Input metric: Weight/Beta estimates (from offir_updating_results.csv)
# Key parameters: AgeGroup, Race, Model
# Output plots: output/plots/offir_updating_own_race.png; output/plots/offir_updating_other_race.png

# Load packages and shared resources
library(ggplot2)
library(dplyr)
library(showtext)

source("R/paths.R")
source("R/theme_pub.R")

# Configure plot text rendering
font_add("Times New Roman", regular = "times.ttf")
showtext_opts(dpi = 300)
showtext_auto()

# Define race color mapping
pal_race <- c("Own-Race"   = "#A6D8C3",
              "Other-Race" = "#F6B48F")

# Read and prepare input data
betas <- read.csv(data_path("offir_updating_results.csv"))

betas <- betas %>%
  mutate(
    AgeGroup = factor(
      AgeGroup,
      levels = c("Children", "Young adults", "Middle-aged", "Elderly")
    ),
    Model = factor(Model, levels = c("t-inf_m1", "t-1_m1")),
    Race  = factor(Race,  levels = c("Own-Race", "Other-Race"))
  )

# Build plotting helper by race
plot_by_race <- function(df, race_label) {
  df_r <- df %>% filter(Race == race_label)
  
  ggplot(
    df_r,
    aes(x = AgeGroup,
        y = Weight,
        group = Model,
        linetype = Model,
        shape = Model)
  ) +
    geom_errorbar(aes(ymin = Weight - SE, ymax = Weight + SE),
                  width = 0.15, linewidth = 0.5) +
    geom_line(linewidth = 0.7,
              color = pal_race[[race_label]]) +
    geom_point(size = 2.6,
               color = pal_race[[race_label]]) +
    scale_linetype_manual(
      values = c("t-inf_m1" = "solid",
                 "t-1_m1"   = "dashed"),
      name = "Model"
    ) +
    scale_shape_manual(
      values = c("t-inf_m1" = 16,
                 "t-1_m1"   = 17),
      name = "Model"
    ) +
    labs(
      x = NULL,
      y = "Beta",
      title = race_label
    ) +
    coord_cartesian(ylim = c(-0.1, 0.5)) +
    theme_pub() +
    theme(
      plot.title      = element_text(hjust = 0.5),
      legend.position = "bottom"
    )
}

# Generate race-specific plots
p_own   <- plot_by_race(betas, "Own-Race")
p_other <- plot_by_race(betas, "Other-Race")

# Save output plots
ggsave(plot_path("offir_updating_own_race.png"),   p_own,
       width = 5, height = 4, dpi = 300, bg = "white")
ggsave(plot_path("offir_updating_other_race.png"), p_other,
       width = 5, height = 4, dpi = 300, bg = "white")
