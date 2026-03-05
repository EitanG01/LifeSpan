# ─── 0) PACKAGES ───────────────────────────────────────────────────────────────
library(dplyr)
library(tidyr)
library(purrr)
library(mgcv)

# ─── 1) READ & PREPARE ─────────────────────────────────────────────────────────
df_grouped <- read.csv(
  "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/Weibull Project/full_results.csv",
  stringsAsFactors = FALSE
) %>%
  # Replace `slope` below with the actual column name of your slope metric if different
  group_by(Subject, ExperimentName, Regression, Age) %>%
  summarize(
    slope = mean(slope, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")),
    Regression     = factor(Regression,     levels = c("biasp", "biasm"))
  )

# ─── 2) NEST & FIT MODELS ──────────────────────────────────────────────────────
model_comps <- df_grouped %>%
  group_by(ExperimentName, Regression) %>%
  nest() %>%
  mutate(
    linear_mod = map(data, ~ gam(slope ~ Age,
                                 data = .x,
                                 method = "ML")),
    quad_mod   = map(data, ~ gam(slope ~ Age + I(Age^2),
                                 data = .x,
                                 method = "ML")),
    spline_mod = map(data, ~ gam(slope ~ s(Age, k = 8),
                                 data = .x,
                                 method = "ML"))
  ) %>%
  ungroup()

# ─── 3) EXTRACT AIC VALUES ─────────────────────────────────────────────────────
model_comps <- model_comps %>%
  mutate(
    AIC_linear    = map_dbl(linear_mod,   AIC),
    AIC_quadratic = map_dbl(quad_mod,     AIC),
    AIC_spline    = map_dbl(spline_mod,   AIC)
  )

# ─── 4) NESTED‐MODEL χ² ANOVAS ─────────────────────────────────────────────────
model_comps <- model_comps %>%
  mutate(
    anova_lin_vs_spline  = map2(linear_mod, spline_mod,  ~ anova(.x, .y, test = "Chisq")),
    anova_quad_vs_spline = map2(quad_mod,   spline_mod,  ~ anova(.x, .y, test = "Chisq"))
  )

# ─── 5) PRINT AIC COMPARISONS ──────────────────────────────────────────────────
model_comps %>%
  select(ExperimentName, Regression,
         AIC_linear, AIC_quadratic, AIC_spline) %>%
  arrange(ExperimentName, Regression) %>%
  print()

# ─── 6) PRINT ANOVA TABLES ─────────────────────────────────────────────────────
# Linear vs. spline
walk2(
  model_comps$anova_lin_vs_spline,
  paste(model_comps$ExperimentName, model_comps$Regression, "— linear vs spline"),
  ~ {
    cat("\n---", .y, "---\n")
    print(.x)
  }
)

# Quadratic vs. spline
walk2(
  model_comps$anova_quad_vs_spline,
  paste(model_comps$ExperimentName, model_comps$Regression, "— quadratic vs spline"),
  ~ {
    cat("\n---", .y, "---\n")
    print(.x)
  }
)


