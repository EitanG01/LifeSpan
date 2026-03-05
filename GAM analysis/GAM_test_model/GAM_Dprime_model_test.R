# ─── 0) PACKAGES ───────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(purrr)
library(mgcv)
library(broom)      
library(ggplot2)
library(showtext)
library(scales)

# ─── 1) FONTS ─────────────────────────────────────────────────────────────────
font_add("Times New Roman", regular = "times.ttf")
showtext_opts(dpi = 300)
showtext_auto()

# ─── 2) READ & PREPARE ────────────────────────────────────────────────────────
df_grouped <- read.csv(
  "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/GAM analysis/dprime_results.csv",
  stringsAsFactors = FALSE
) %>%
  group_by(Subject, ExperimentName, Regression, Age) %>%
  summarize(Dprime = mean(Dprime, na.rm = TRUE), .groups = "drop") %>%
  mutate(
    ExperimentName = factor(ExperimentName, levels = c("Caucasian","Asian"))
  )

# ─── 3) NEST & FIT ────────────────────────────────────────────────────────────
# Nest by the two grouping factors, then fit a GAM to each
gam_results <- df_grouped %>%
  group_by(ExperimentName, Regression) %>%
  nest() %>%
  mutate(
    gam_mod = map(data, ~ 
                    gam(Dprime ~ s(Age, k = 8), data = .x, method = "REML")
    )
  )

# ─── 4) EXTRACT TIDY STATISTICS ───────────────────────────────────────────────
# Use broom::tidy to get edf, F‐statistic, and p‐value for each smooth
gam_stats <- gam_results %>%
  mutate(
    smooth_terms = map(gam_mod, ~ tidy(.x, parametric = FALSE, smooth = TRUE))
  ) %>%
  select(ExperimentName, Regression, smooth_terms) %>%
  unnest(smooth_terms)

# View the table
print(gam_stats)
# You’ll see columns:
#   term   | edf | ref.df | statistic | p.value
# plus your ExperimentName and Regression

# ─── 5) OPTIONAL: PRINT A FULL summary() FOR EACH MODEL ───────────────────────
# If you want the full console summary (including R², deviance explained, GCV):
walk(gam_results$gam_mod, summary)
#summary for everymodel 
summary_list <- map(gam_results$gam_mod, summary)
# ─── 6) PLOTTING ───────────────────────────────────────────────────────────────
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
      Regression = c(biasp = "Bias+", biasm = "Bias-"),
      ExperimentName = c(Asian = "Other-Race", Caucasian = "Own-Race")
    )
  ) +
  scale_color_manual(
    values = c(biasp = "#7FB3D5", biasm = "#F08080"),
    labels = c(biasp = "Bias+", biasm = "Bias-")
  ) +
  scale_fill_manual(
    values = c(biasp = "#7FB3D5", biasm = "#F08080"),
    labels = c(biasp = "Bias+", biasm = "Bias-")
  ) +
  labs(x = "Age", y = "D-prime") +
  theme_pub()

# ─── 7) SAVE PLOT ─────────────────────────────────────────────────────────────
ggsave(
  filename = "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/Most Updated Figueres/GAM_all_groups_dprime.png",
  plot     = p,
  width    = 6, height = 5, dpi = 300
)







# 2) Quadratic as a GAM (method = "ML")
quad_gam_ml <- gam(Dprime ~ Age + I(Age^2),
                   data   = df_grouped,
                   method = "ML")

# 3) Pure spline GAM
spline_gam_ml <- gam(Dprime ~ s(Age, k = 8),
                     data   = df_grouped,
                     method = "ML")

# 4) Model comparison
AIC(quad_gam_ml, spline_gam_ml)
anova(quad_gam_ml, spline_gam_ml, test = "Chisq")


quad_lm    <- lm(Dprime ~ Age + I(Age^2), data = df_grouped)
spline_gam <- gam(Dprime ~ s(Age, k=8),    data = df_grouped, method = "ML")
linar_model <- lm(Dprime ~ Age, data = df_grouped)
print("Polynial vs GAM model")
anova(quad_lm, spline_gam)  
AIC(quad_lm, spline_gam)
print("Linear vs GAM model")
anova(linar_model, spline_gam)
AIC(linar_model, spline_gam)

