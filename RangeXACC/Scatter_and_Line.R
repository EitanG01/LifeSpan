# Analysis: ACC trajectories across Range by group, race, and regression
# Input metric: ACC (from full_data_life_span.csv)
# Key parameters: Group, ExperimentName (race), Range, Regression, Subject
# Output plot: output/plots/acc_range_lifespan.png

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
df_summary <- read.csv(
  data_path("full_data_life_span.csv")
  #data_path("Full Data ASD TD Aug 20.csv") # ASD data
  # data_path("full_data_delay.csv")
) %>%
  filter(Range != 0) %>%
  group_by(Subject, ExperimentName, Range, Regression, Group) %>%
  summarize(
    mean_ACC = mean(ACC, na.rm = TRUE),
    .groups  = "drop" 
  ) %>%
  # Set factor order for plotting panels
  mutate(
    Group = factor(Group, levels = c("Children", "Young Adults", "Middle-Aged", "Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian"))
  )
# Standardize regression labels
df_summary <- df_summary %>%
  mutate(
    Regression = recode(Regression, "biasP" = "biasp", "biasM" = "biasm"),
    Regression = factor(Regression, levels = c("biasp", "biasm"))
  )
# Compute panel-level means and standard errors
stats_df_all <- df_summary %>%
  group_by(Group, ExperimentName, Range, Regression) %>%
  summarize(
    mean_val = mean(mean_ACC),
    se       = sd(mean_ACC) / sqrt(n()),
    .groups  = "drop"
  )


# Build multi-panel plot
p_all <- ggplot() +
  
  # Raw points
  geom_jitter(
    data     = df_summary,
    aes(
      x     = factor(Range),
      y     = mean_ACC,
      color = Regression
    ),
    size      = 1.5,
    alpha     = 0.5,
    position  = position_jitterdodge(
      jitter.width = 0.2,
      dodge.width  = 0
    )
  ) +
  
  # Mean lines
  geom_line(
    data     = stats_df_all,
    aes(
      x     = factor(Range),
      y     = mean_val,
      group = Regression,
      color = Regression
    ),
    size      = 1.2
  ) +
  
  # Mean points
  geom_point(
    data     = stats_df_all,
    aes(
      x     = factor(Range),
      y     = mean_val,
      color = Regression
    ),
    size      = 3
  #  position  = position_dodge(width = 0.2)
  ) +
  
  # Standard error bars
  geom_errorbar(
    data     = stats_df_all,
    aes(
      x     = factor(Range),
      ymin  = mean_val - se,
      ymax  = mean_val + se,
      color = Regression
    ),
    width     = 0.1,
    size      = 0.8
 #   position  = position_dodge(width = 0.7)a
  ) +
  
  # Facets, scales, labels, and theme
  facet_grid(ExperimentName ~ Group,
             labeller = labeller(
               ExperimentName = c("Asian" = "Other-Race", "Caucasian" = "Own-Race")
             )) +
  scale_x_discrete(
    breaks = c("15","21","27","33","39","45"),
    labels = c("15","21","27","33","39","45")
  ) +
  scale_colour_manual(
    values = c(biasp = "#7FB3D5", biasm = "#F08080"),
    labels = c(biasp = "Bias+", biasm = "Bias-")
  ) +
  labs(
    x     = "Difference in morph levels",
    y     = "Accuracy",
    
    
  ) +
  theme_pub() 

# Save output plot
ggsave(
  filename = plot_path("acc_range_lifespan.png"),
  plot     = p_all,
  width    = 8,
  height   = 6,
  dpi      = 300
)



