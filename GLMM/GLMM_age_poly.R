# Analysis: Lifespan GLMM for trial-level accuracy
# Input metric: ACC (from full_data_life_span.csv)
# Key parameters: Age, ExperimentName (race), Regression (biasm/biasp), Range, Subject
# Output plot: output/plots/age_accuracy_plot_lifespan_final2.png
# Output data file: output/loso_results.csv
# Output console: model summary, diagnostics, model comparisons, LOSO aggregates, EMM contrasts

# Load packages and shared resources
library(glmmTMB)
library(dplyr)
library(emmeans)
library(DHARMa)
library(sjPlot)
library(showtext)
library(scales)
library(ggplot2)
library(performance)


source(file.path("R", "paths.R"))
source("R/theme_pub.R")


# Configure plot text rendering
font_add("Times New Roman", regular = "times.ttf")
showtext_opts(dpi = 300)
showtext_auto()





# Read and prepare analysis data
df <- read.csv(data_path("full_data_life_span.csv")) %>%
  filter(Range != 0)
# center & scale Range in one step
df$Range_c <- as.numeric(scale(df$Range, center = TRUE, scale = TRUE))

# Make column Subject a factor
df <- df %>%
  mutate(
    Regression     = factor(Regression,     levels = c("biasm","biasp")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian","Asian"), labels = c("Own-Race", "Other-Race")), 
    Subject        = factor(Subject),
    GrpExp         = interaction(Regression, ExperimentName, drop = TRUE)
  )


## ── Prepare centred raw age terms (easier to interpret) ──────────────
df$Age_c  <- scale(df$Age, scale = FALSE)
df$Age_c2 <- df$Age_c^2           # curvature



# Fit main GLMM
m_simple <- glmmTMB(
  ACC ~ 
    poly(Age, 2) * Regression      # main age + age×Regression (linear & quad)
  + poly(Age, 2) * ExperimentName  # main age + age×ExperimentName
  + Regression:ExperimentName  
  + Range
  + (1 | Subject),                 # keep your random intercept
  data   = df,
  family = binomial(link = "probit")
)
summary(m_simple)
r2(m_simple)


subs <- ranef(m_simple)$Subject
print(subs)

subs_coef <- coef(m_simple)$Subject
print(subs_coef)









# Build and save model-based prediction plot


p <- plot_model(
  m_simple, type = "pred",
  terms = c("Age [8:79]", "Regression", "ExperimentName"),
  transform = "response",
  line.size = 0.8,     # makes the line as thick as GAM
  ci.lvl = 0.95        # keep your confidence level
)

final_plot <- p +
  labs(title = NULL, y = "Accuracy") +
  scale_color_manual(
    values = c(biasp = "#7FB3D5", biasm = "#F08080"),
    labels = c(biasp = "Bias+", biasm = "Bias−")
  ) +
  scale_fill_manual(
    values = c(biasp = "#7FB3D5", biasm = "#F08080"),
    labels = c(biasp = "Bias+", biasm = "Bias−")
  ) +
  # --- format y-axis as 0.00 ---
  scale_y_continuous(
    limits = c(0.5, 0.9),
    labels = scales::number_format(accuracy = 0.01)
  ) +  guides(fill = "none") +
  theme_pub()

ggsave(
  plot_path("age_accuracy_plot_lifespan_final2.png"),
  plot = final_plot,
  width = 8, height = 4, dpi = 300, bg = "white"
)
    













# Run model diagnostics

# 1) Convergence & basic summary
summ <- summary(m_simple)
print(summ$fit$convergence)  # 0 means OK
print(summ$fit$messages)     # should be NULL/empty
print(summ$coefficients)     # fixed effects table

# 2) Random-effects sanity
print(VarCorr(m_simple))     # variances should be > 0 and not insanely tiny
# If you see extremely small RE variance or warnings -> mention and justify RI-only choice.

# 3) DHARMa residual diagnostics
set.seed(123)
sim <- simulateResiduals(m_simple, n = 1000)  # parametric sims
plot(sim)                                     # uniformity QQ + residual vs fitted

testUniformity(sim)       # global fit (should be ns)
testDispersion(sim)       # over/under-dispersion (ns desirable)
testZeroInflation(sim)    # zero inflation (ns desirable for binomial here)







# Run leave-one-subject-out (LOSO) evaluation

# ---- metrics ----
brier   <- function(y, p) mean((y - p)^2)
logloss <- function(y, p, eps = 1e-15) {
  p <- pmin(pmax(p, eps), 1 - eps)
  -mean(y * log(p) + (1 - y) * log(1 - p))
}

# ---- helper: refit on train subjects, predict on the held-out subject (population-level preds) ----
loso_once <- function(test_id) {
  train_ids <- setdiff(unique(df$Subject), test_id)
  train_df  <- df %>% filter(Subject %in% train_ids)
  test_df   <- df %>% filter(Subject == test_id)
  
  # refit the *same* model structure as m_simple on the training subjects
  fit <- update(m_simple, data = train_df)
  
  # predict probabilities for the held-out subject WITHOUT random effects
  p <- predict(fit, newdata = test_df, type = "response", re.form = NA)
  
  data.frame(
    Subject = test_id,
    Brier   = brier(test_df$ACC, p),
    LogLoss = logloss(test_df$ACC, p)
  )
}

# ---- run LOSO across all subjects ----
set.seed(1)
subjects <- unique(df$Subject)
loso_results <- do.call(rbind, lapply(subjects, loso_once))

# per-subject scores (useful for a suppl. table)
print(loso_results[1:10, ])  # peek first rows
# save loso result as csv
write.csv(loso_results, file.path(OUTPUT_DIR, "loso_results.csv"), row.names = FALSE)

# aggregate (report these)
loso_summary <- summarise(loso_results,
                          Brier_mean = mean(Brier),
                          Brier_se   = sd(Brier)/sqrt(n()),
                          LogLoss_mean = mean(LogLoss),
                          LogLoss_se   = sd(LogLoss)/sqrt(n()))
print(loso_summary)




# Fit model comparison set
complexity_compare <- glmmTMB(
  ACC ~ 
    poly(Age, 2) * Regression * ExperimentName
  + Range
  + (1 | Subject),                 # keep your random intercept
  data   = df,
  family = binomial(link = "probit")
) 
summary(complexity_compare)
anova(m_simple, complexity_compare)  # compare models


simplicity_compare <- glmmTMB(
  ACC ~ 
    poly(Age, 2)
  + Regression
  + ExperimentName
  + Range
  + (1 | Subject),                 # keep your random intercept
  data   = df,
  family = binomial(link = "probit")
) 
summary(simplicity_compare)
anova(m_simple, simplicity_compare)  


linear_compare <- glmmTMB(
  ACC ~ 
    Age * Regression      # main age + age×Regression (linear & quad)
  + Age * ExperimentName  # main age + age×ExperimentName
  + Regression:ExperimentName  
  + Range
  + (1 | Subject),                 # keep your random intercept
  data   = df,
  family = binomial(link = "probit")
)
summary(linear_compare)
anova(m_simple, linear_compare)  


# Compare nested model families
m_main <- glmmTMB(
  ACC ~ poly(Age, 2) + Regression + ExperimentName + Range +
    Regression:ExperimentName +
    (1 | Subject),
  data   = df,
  family = binomial(link = "probit")
)

# [2] Linear Age interactions (Age linear × Regression/ExperimentName)
m_linearAge <- glmmTMB(
  ACC ~ Age * Regression +
    Age * ExperimentName +
    Regression:ExperimentName +
    Range +
    (1 | Subject),
  data   = df,
  family = binomial(link = "probit")
)



# Main-effects vs Final (Age interactions)
lrt_main_vs_final <- anova(m_main, m_simple)  # χ² test
print(lrt_main_vs_final)

# Linear Age vs Final (quadratic Age)
lrt_linear_vs_final <- anova(m_linearAge, m_simple)
print(lrt_linear_vs_final)









# Estimate marginal means at representative ages
ages <- c(Children = 9.25, `Young Adults` = 23.63, `Middle-Aged` = 40.82, Elderly = 65.18)

# If model term is Range_c (centered):
emm_by_age <- emmeans(
  m_simple,
  ~ Regression * ExperimentName | Age,
  at   = list(Age = unname(ages), Range_c = 0),
  type = "response"   # probabilities
)


emm_tab <- as.data.frame(emm_by_age)
lab_map <- setNames(names(ages), unname(ages))
emm_tab$AgeGroup <- factor(lab_map[as.character(emm_tab$Age)],
                           levels = names(ages))



# Simple-effects contrasts that spell out the 2×2 interaction at each age
# 1) BiasP vs BiasM within each Race × Age
pairs(emm_by_age, by = c("Age","ExperimentName"), adjust = "holm")

# 2) Asian vs Caucasian within each Regression × Age
pairs(emm_by_age, by = c("Age","Regression"), adjust = "holm")
