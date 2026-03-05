library(dplyr)
library(purrr)
library(tidyr)
library(mgcv)
library(ggplot2)
library(tibble)

full_data <- read.csv(
  "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/updating plots/Updating_Aging.csv",
  stringsAsFactors = FALSE
)

# 1) CENTERING HELPER (unchanged)
center_scale <- function(x) scale(x, center = TRUE, scale = FALSE)[,1]

# 2) ONE GLM per Subject × Race (no Range loops, no trial‐count checks)
combos_subj <- full_data %>%
  distinct(ExperimentName, Subject) %>%
  rename(exp_name = ExperimentName,
         subj     = Subject)

fit_subj <- function(exp_name, subj) {
  dat <- full_data %>%
    filter(ExperimentName == exp_name,
           Subject        == subj)
  m  <- glm(ACC ~ bias_inf + bias_new_t1,
            data   = dat,
            family = binomial(link = "probit"))
  co <- coef(m)
  tibble(exp_name,
         subj,
         Group      = dat$Group[1],
         Age        = dat$Age[1],
         weight_inf = co["bias_inf"],
         weight_t1  = co["bias_new_t1"])
}

results_subj <- pmap_dfr(combos_subj, fit_subj)



safe_gamm <- function(formula, data) {
  tryCatch(
    gamm(formula, data = data, method = "REML")$gam,
    error = function(e) NULL
  )
}

nested_age <- results_subj %>%
  group_by(exp_name) %>%
  nest() %>%
  mutate(
    gam_t1  = map(data, ~ safe_gamm(weight_t1  ~ s(Age, k = 6), .x)),
    gam_inf = map(data, ~ safe_gamm(weight_inf ~ s(Age, k = 6), .x))
  )


# 4) PREDICTIONS over Age (unchanged)
make_pred_age <- function(gam_mod, df) {
  newx <- seq(min(df$Age), max(df$Age), length = 200)
  pr   <- predict(gam_mod,
                  newdata = data.frame(Age = newx),
                  se.fit  = TRUE)
  tibble(Age = newx,
         fit  = pr$fit,
         se   = pr$se.fit)
}

plots_df <- nested_age %>%
  filter(!map_lgl(gam_t1, is.null)) %>%    # keep only successful fits
  mutate(
    pred_t1  = map2(gam_t1,  data, make_pred_age),
    pred_inf = map2(gam_inf, data, make_pred_age)
  ) %>%
  unnest(c(pred_t1, pred_inf), names_sep = "_")


# 5) PLOTTING (unchanged)
ggplot(plots_df, aes(x = pred_t1_Age, y = pred_t1_fit, color = exp_name)) +
  geom_ribbon(aes(ymin = pred_t1_fit - 2 * pred_t1_se,
                  ymax = pred_t1_fit + 2 * pred_t1_se),
              alpha = .2, fill = NA) +
  geom_line(size = 1) +
  labs(x     = "Age (years)",
       y     = expression(hat(beta)[t1]),
       title = "Smooth weight of previous‐trial bias (β[t1]) by Age") +
  theme_bw()

ggplot(plots_df, aes(x = pred_inf_Age, y = pred_inf_fit, color = exp_name)) +
  geom_ribbon(aes(ymin = pred_inf_fit - 2 * pred_inf_se,
                  ymax = pred_inf_fit + 2 * pred_inf_se),
              alpha = .2, fill = NA) +
  geom_line(size = 1) +
  labs(x     = "Age (years)",
       y     = expression(hat(beta)[infinity]),
       title = "Smooth weight of life‐long mean bias (β[∞]) by Age") +
  theme_bw()
