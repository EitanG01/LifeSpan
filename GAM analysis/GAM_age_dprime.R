# Analysis: Lifespan GAM for d-prime
# Input metric: Dprime (from dprime_results.csv)
# Key parameters: Age, ExperimentName (race), Regression (biasp/biasm), Subject
# Output plot: output/plots/GAM_all_groups_dprime.png
# Output console: GAM model summaries, smooth term significance, AIC model comparisons

# Load packages and shared resources
library(dplyr)
library(ggplot2)
library(mgcv)
library(showtext)
library(scales)
library(purrr)

source("R/paths.R")
source("R/theme_pub.R")

# Configure plot text rendering
font_add("Times New Roman", regular = "times.ttf")
showtext_opts(dpi = 300)
showtext_auto()

# Read input data
full_data <- read.csv(
  data_path("dprime_results.csv"),
  stringsAsFactors = FALSE
)

# Prepare analysis dataset
df_grouped <- full_data %>%
  group_by(Subject, ExperimentName, Regression, Age) %>%
  summarize(Dprime = mean(Dprime, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")),
    Regression     = factor(Regression,     levels = c("biasp", "biasm"))
  )


# Build lifespan trajectory plot
p <- ggplot(
  df_grouped,
  aes(x = Age, y = Dprime, color = Regression, fill = Regression)
) +
  geom_smooth(
    method      = "gam",
    formula     = y ~ s(x, k = 8),
    method.args = list(method = "REML"),
    se          = TRUE,
    size        = 0.8,
    alpha       = 0.2 
  ) +
  facet_grid(
    Regression ~ ExperimentName,
    labeller = labeller(
      Regression = c("biasp" = "Bias+", "biasm" = "Bias-"),
      ExperimentName = c("Asian" = "Other-Race", "Caucasian" = "Own-Race")
    )
  )+
  scale_color_manual(
    values = c(biasp = "#7FB3D5", biasm = "#F08080"),
    labels = c(biasp = "Bias+", biasm = "Bias-")
    
    
  ) +
  scale_fill_manual(
    values = c(biasp = "#7FB3D5", biasm = "#F08080"),
    labels = c(biasp = "Bias+", biasm = "Bias-")
    
  ) +
  labs(x = "Age", y = "D-prime") +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_pub() 
  

# Save output plot
ggsave(
  filename = plot_path("GAM_all_groups_dprime.png"),
  plot     = p,
  width    = 6,
  height   = 5,
  dpi      = 300,
)



# Fit panel-wise GAM summaries
gam_results <- df_grouped %>%
  group_by(ExperimentName, Regression) %>%
  group_modify(~ {
    
    gam_fit <- gam(
      Dprime ~ s(Age, k = 8),
      data   = .x,
      method = "REML"
    )
    
    s_tab <- summary(gam_fit)$s.table
    
    tibble(
      EDF                = s_tab[,"edf"],
      Smooth_p           = s_tab[,"p-value"],
      Deviance_explained = summary(gam_fit)$dev.expl
    )
  }) %>%
  ungroup() %>%
  mutate(
    Smooth_sig = case_when(
      Smooth_p < .001 ~ "***",
      Smooth_p < .01  ~ "**",
      Smooth_p < .05  ~ "*",
      TRUE            ~ "ns"
    )
  )

print(gam_results)

# Compare linear, quadratic, and spline age models
model_comps <- df_grouped %>%
  group_by(ExperimentName, Regression) %>%
  nest() %>%
  mutate(
    linear_mod = map(data, ~ gam(Dprime ~ Age,            data = .x, method = "REML")),
    quad_mod   = map(data, ~ gam(Dprime ~ Age + I(Age^2), data = .x, method = "REML")),
    spline_mod = map(data, ~ gam(Dprime ~ s(Age, k = 8),  data = .x, method = "REML"))
  ) %>%
  ungroup() %>%
  mutate(
    AIC_linear           = map_dbl(linear_mod,  AIC),
    AIC_quadratic        = map_dbl(quad_mod,    AIC),
    AIC_spline           = map_dbl(spline_mod,  AIC),
    anova_lin_vs_spline  = map2(linear_mod, spline_mod, ~ anova(.x, .y, test = "Chisq")),
    anova_quad_vs_spline = map2(quad_mod,   spline_mod, ~ anova(.x, .y, test = "Chisq"))
  )

model_comps %>%
  select(ExperimentName, Regression, AIC_linear, AIC_quadratic, AIC_spline) %>%
  arrange(ExperimentName, Regression) %>%
  print()

walk2(
  model_comps$anova_lin_vs_spline,
  paste(model_comps$ExperimentName, model_comps$Regression, "- linear vs spline"),
  ~ { cat("\n---", .y, "---\n"); print(.x) }
)

walk2(
  model_comps$anova_quad_vs_spline,
  paste(model_comps$ExperimentName, model_comps$Regression, "- quadratic vs spline"),
  ~ { cat("\n---", .y, "---\n"); print(.x) }
)


