# ──────────────────────────────────────────────────────────────
# 0) Libraries
# ──────────────────────────────────────────────────────────────
library(dplyr)
library(ggplot2)
library(mgcv)
library(showtext)
library(scales)

# ──────────────────────────────────────────────────────────────
# 1) Fonts & plotting defaults
# ──────────────────────────────────────────────────────────────
font_add("Times New Roman", regular = "times.ttf")
showtext_opts(dpi = 300)
showtext_auto()

# ──────────────────────────────────────────────────────────────
# 2) Read data (trial-level)
# ──────────────────────────────────────────────────────────────
full_data <- read.csv(
  "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/data/life_span_updating.csv",
  stringsAsFactors = FALSE
)

# ──────────────────────────────────────────────────────────────
# 3) Preprocess — KEEP TRIALS
# ──────────────────────────────────────────────────────────────
df_trial <- full_data %>%
  filter(Range != 0) %>%  # keep as in your previous scripts
  mutate(
    ExperimentName = factor(
      ExperimentName,
      levels = c("Caucasian", "Asian")
    ),
    Regression = factor(
      Regression,
      levels = c("biasp", "biasm")
    ),
    Subject = factor(Subject),
    ACC     = as.numeric(ACC)
  )

# ──────────────────────────────────────────────────────────────
# 4) BAM FOR PLOTTING (REML)
# ──────────────────────────────────────────────────────────────
bam_plot <- bam(
  ACC ~
    s(Age, k = 8) +
    s(Subject, bs = "re") +
    Regression * ExperimentName,
  family = binomial,
  method = "REML",
  data   = df_trial
)

# ──────────────────────────────────────────────────────────────
# 5) PREDICTION GRID (MARGINAL OVER SUBJECT)
# ──────────────────────────────────────────────────────────────
pred_grid <- expand.grid(
  Age            = seq(
    min(df_trial$Age),
    max(df_trial$Age),
    length.out = 200
  ),
  Regression     = levels(df_trial$Regression),
  ExperimentName = levels(df_trial$ExperimentName)
)

# add a valid Subject level (required, but will be excluded)
pred_grid$Subject <- levels(df_trial$Subject)[1]

# ──────────────────────────────────────────────────────────────
# 6) PREDICT (EXCLUDE RANDOM EFFECT)
# ──────────────────────────────────────────────────────────────
pred <- predict(
  bam_plot,
  newdata = pred_grid,
  type    = "response",
  se.fit  = TRUE,
  exclude = "s(Subject)"
)

pred_grid$fit <- pred$fit
pred_grid$se  <- pred$se.fit

# ──────────────────────────────────────────────────────────────
# 7) PLOT — ADJUSTED AGE CURVES (REML)
# ──────────────────────────────────────────────────────────────
p <- ggplot(
  pred_grid,
  aes(x = Age, y = fit, color = Regression, fill = Regression)
) +
  geom_ribbon(
    aes(
      ymin = fit - 1.96 * se,
      ymax = fit + 1.96 * se
    ),
    alpha = 0.2,
    color = NA
  ) +
  geom_line(size = 0.9) +
  facet_grid(
    Regression ~ ExperimentName,
    labeller = labeller(
      Regression = c(
        "biasp" = "Bias+",
        "biasm" = "Bias−"
      ),
      ExperimentName = c(
        "Caucasian" = "Own-Race",
        "Asian"     = "Other-Race"
      )
    )
  ) +
  scale_color_manual(
    values = c(
      biasp = "#7FB3D5",
      biasm = "#F08080"
    )
  ) +
  scale_fill_manual(
    values = c(
      biasp = "#7FB3D5",
      biasm = "#F08080"
    )
  ) +
  labs(
    x = "Age",
    y = "Adjusted accuracy (probability of “different”)"
  ) +
  theme_pub()

# ──────────────────────────────────────────────────────────────
# 8) SAVE FIGURE
# ──────────────────────────────────────────────────────────────
ggsave(
  filename =
    "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/Most Updated Figueres/GAM_ACC_trial_level_subject_adjusted.png",
  plot   = p,
  width  = 6,
  height = 5,
  dpi    = 300
)

# ──────────────────────────────────────────────────────────────
# 9) MODEL COMPARISON (ML)
# ──────────────────────────────────────────────────────────────

bam_linear <- bam(
  ACC ~
    Age +
    s(Subject, bs = "re") +
    Regression * ExperimentName,
  family = binomial,
  method = "ML",
  data   = df_trial
)

bam_quad <- bam(
  ACC ~
    Age + I(Age^2) +
    s(Subject, bs = "re") +
    Regression * ExperimentName,
  family = binomial,
  method = "ML",
  data   = df_trial
)

bam_spline <- bam(
  ACC ~
    s(Age, k = 6) +
    s(Subject, bs = "re") +
    Regression * ExperimentName,
  family = binomial,
  method = "ML",
  data   = df_trial
)

# ──────────────────────────────────────────────────────────────
# 10) COMPARE AGE SHAPES
# ──────────────────────────────────────────────────────────────
AIC(
  bam_linear,
  bam_quad,
  bam_spline
)

anova(bam_linear, bam_spline, test = "Chisq")
anova(bam_quad,   bam_spline, test = "Chisq")

# ──────────────────────────────────────────────────────────────
# 11) DIAGNOSTICS
# ──────────────────────────────────────────────────────────────
gam.check(bam_plot)
