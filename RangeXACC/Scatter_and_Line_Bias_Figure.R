# Analysis: ACC trajectories across Range faceted by regression and grouped by age
# Input metric: ACC (from full_data_life_span.csv)
# Key parameters: Group, ExperimentName (race), Range, Regression, Subject
# Output plot: output/plots/acc_range_lifespan_bias_facet.png

# Load packages and shared resources
library(ggplot2)
library(dplyr)
library(showtext)
library(scales)

source("R/paths.R")
source("R/theme_pub.R")

# Configure plot text rendering
font_add("Times New Roman", regular = "times.ttf")
showtext_opts(dpi = 300)
showtext_auto()



# Read and prepare subject-level summary data
df_summary <- read.csv(data_path("full_data_life_span.csv")) %>%
  filter(Range != 0) %>%
  group_by(Subject, ExperimentName, Range, Regression, Group) %>%
  summarize(
    mean_ACC = mean(ACC, na.rm = TRUE),
    .groups  = "drop" 
  ) %>%
  # Set factor order for plotting panels
  mutate(
    Group = factor(Group, levels = c("Children","Young Adults", "Middle-Aged", "Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian"))
  )

# Standardize regression labels
df_summary <- df_summary %>%
  mutate(
    Regression = recode(Regression, "biasP" = "biasp", "biasM" = "biasm"),
    Regression = factor(Regression, levels = c( "biasm", "biasp"))
  )

# Compute panel-level means and standard errors
stats_df_all <- df_summary %>%
  group_by(Group, ExperimentName, Range, Regression) %>%
  summarize(
    mean_val = mean(mean_ACC),
    se       = sd(mean_ACC) / sqrt(n()),
    .groups  = "drop"
  )

# Build multi-panel plot (race-colored lines, regression facets)
p_all <- ggplot() +
  
  # Optional raw points layer kept disabled
  # geom_jitter(
  #   data     = df_summary,
  #   aes(
  #     x     = factor(Range),
  #     y     = mean_ACC,
  #     color = ExperimentName   # CHANGED
  #   ),
  #   size      = 1.5,
  #   alpha     = 0.5,
  #   position  = position_jitterdodge(
  #     jitter.width = 0.2,
  #     dodge.width  = 0
  #   )
  # ) +
  # 
  # Mean lines
  geom_line(
    data     = stats_df_all,
    aes(
      x     = factor(Range),
      y     = mean_val,
      group = ExperimentName,
      color = ExperimentName
    ),
    size = 1.2
  ) +
  
  # Mean points
  geom_point(
    data     = stats_df_all,
    aes(
      x     = factor(Range),
      y     = mean_val,
      color = ExperimentName
    ),
    size = 3
  ) +
  
  # Standard error bars
  geom_errorbar(
    data     = stats_df_all,
    aes(
      x     = factor(Range),
      ymin  = mean_val - se,
      ymax  = mean_val + se,
      color = ExperimentName
    ),
    width = 0.1,
    size  = 0.8
  ) +
  
  # Facet by regression
  facet_grid(Regression ~ Group,
             labeller = labeller(
               Regression = c("biasp" = "Bias+", "biasm" = "Bias−")
             )) +
  
  scale_x_discrete(
    breaks = c("15", "21", "27", "33", "39", "45"),
    labels = c("15", "21", "27", "33", "39", "45")
  ) +
  
  # Colors represent race
  scale_colour_manual(
    values = c("Caucasian" = "#A6D8C3", "Asian" = "#F6B48F"),
    labels = c("Caucasian" = "Own-Race", "Asian" = "Other-Race"),
    name   = "Race"
  ) +
  
  labs(
    x = "Difference in morph levels",
    y = "Accuracy"
  ) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01)) +
  theme_pub()


# Save output plot
ggsave(
  filename = plot_path("acc_range_lifespan_bias_facet.png"),
  plot     = p_all,
  width    = 8,
  height   = 6,
  dpi      = 300
)


