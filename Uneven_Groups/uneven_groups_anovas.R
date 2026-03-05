# Load all necessary packages
library(dplyr)
library(purrr)
library(rstatix)
library(car)
library(broom)
library(onewaytests)
library(ggplot2)
library(tidyr)
library(heplots)

# ------------------------------------------------------------------
# 1) Load & preprocess data
# ------------------------------------------------------------------
file_path <- "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/full_data.csv"

full_data <- read.csv(file_path, stringsAsFactors = FALSE) %>%
  filter(Range != 0) %>%
  mutate(
    Group          = factor(Group, levels = c("Children","Young Adults","Middle-Aged","Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian","Asian")),
    Regression     = factor(Regression, levels = c("biasp","biasm")),
    Range          = factor(Range, levels = c(15, 21, 27, 33, 39, 45))
  )

# 1a) Create subject-level summary for homogeneity & group tests
subject_acc <- full_data %>%
  group_by(Subject, Group) %>%
  summarise(
    ACC_subj = mean(ACC, na.rm = TRUE),
    .groups  = "drop"
  )

# 1b) Create cell-level summary for repeated measures ANOVA (include Group)
cell_acc <- full_data %>%
  group_by(Subject, Group, Regression, ExperimentName, Range) %>%
  summarise(
    ACC_cell = mean(ACC, na.rm = TRUE),
    .groups  = "drop"
  )

# ------------------------------------------------------------------
# 2) Variance homogeneity and ratio check functions
# ------------------------------------------------------------------
check_variance_homogeneity <- function(subj_data) {
  lev <- car::leveneTest(ACC_subj ~ Group,
                         data   = subj_data,
                         center = mean) %>%
    broom::tidy() %>%
    mutate(Test = "Levene", .before = 1)
  
  bf_res <- bf.test(ACC_subj ~ Group,
                    data  = subj_data,
                    alpha = 0.05,
                    verbose = FALSE)
  bf <- tibble(
    Test      = "Brown-Forsythe",
    statistic = bf_res$statistic,
    parameter = bf_res$parameter,
    p.value   = bf_res$p.value
  )
  
  bind_rows(lev, bf)
}

check_variance_ratio <- function(subj_data) {
  var_ratio <- subj_data %>%
    group_by(Group) %>%
    summarise(var = var(ACC_subj), .groups = "drop") %>%
    summarise(ratio = max(var) / min(var)) %>%
    pull(ratio)
  cat("Variance ratio (max/min) =", round(var_ratio, 2), "\n")
  invisible(var_ratio)
}

# ------------------------------------------------------------------
# 3) Distribution & normality analysis function
# ------------------------------------------------------------------
analyze_distribution_and_normality <- function(subj_data) {
  p <- ggplot(subj_data, aes(x = Group, y = ACC_subj)) +
    geom_boxplot(outlier.colour = "red", outlier.size = 2) +
    labs(title = "Subject Accuracy by Group",
         x     = "Group",
         y     = "Accuracy (ACC)") +
    theme_minimal()
  print(p)
  
  subj_data %>%
    group_by(Group) %>%
    summarise(
      shapiro_p = shapiro.test(ACC_subj)$p.value,
      .groups   = "drop"
    )
}

# ------------------------------------------------------------------
# 4) Between-group Welch's ANOVA function
# ------------------------------------------------------------------
run_welch_between <- function(subj_data) {
  subj_data %>%
    welch_anova_test(ACC_subj ~ Group) %>%
    mutate(Test = "Welch ANOVA", .before = 1)
}

# ------------------------------------------------------------------
# 5) Sphericity check for within- and mixed-effects via repeated-measures ANOVA
# ------------------------------------------------------------------
run_rm_anova_with_sphericity <- function(cell_data) {
  rm_aov <- anova_test(
    data   = cell_data,
    dv     = ACC_cell,
    wid    = Subject,
    within = c(Regression, ExperimentName, Range),
    type   = 3
  )
  list(
    ANOVA                     = rm_aov$ANOVA,
    Mauchly_Test              = rm_aov[["Mauchly's Test for Sphericity"]],
    Sphericity_Corrections    = rm_aov[["Sphericity Corrections"]]
  )
}

# ------------------------------------------------------------------
# 6) Covariance homogeneity across groups (Box's M test)
# ------------------------------------------------------------------
run_boxM <- function(cell_data, within_factor) {
  # Pivot to one column per level of the specified within‐factor
  wide <- cell_data %>%
    select(Subject, Group, all_of(within_factor), ACC_cell) %>%
    pivot_wider(names_from = all_of(within_factor),
                values_from = ACC_cell)
  
  # Pull off the grouping factor, drop it and Subject, then run Box's M
  grp <- wide$Group
  data_mat <- wide %>% select(-Subject, -Group)
  
  boxM(data_mat, grp)
}

# ------------------------------------------------------------------
# Example usage
# ------------------------------------------------------------------
homogeneity_results <- check_variance_homogeneity(subject_acc)
variance_ratio      <- check_variance_ratio(subject_acc)
normality_results   <- analyze_distribution_and_normality(subject_acc)
welch_results       <- run_welch_between(subject_acc)
rm_results          <- run_rm_anova_with_sphericity(cell_acc)
# boxM_range          <- run_boxM(cell_acc, "Range")
# boxM_reg            <- run_boxM(cell_acc, "Regression")
# boxM_exp            <- run_boxM(cell_acc, "ExperimentName")


print(homogeneity_results)
print(normality_results)
print(welch_results)
print(rm_results$ANOVA)
print(rm_results$Mauchly_Test)
print(rm_results$Sphericity_Corrections)
#print(boxM_range)
# print(boxM_reg)
# print(boxM_exp)
