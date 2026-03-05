# Analysis: Grouped ACC histogram with within-group comparisons
# Input metric: ACC (from life_span_updating.csv)
# Key parameters: Group, ExperimentName (race), Regression (biasp/biasm), Subject
# Output plot: output/plots/ACC_range.png
# Output console: paired t-tests (regression and group contrasts with adjusted p-values)

# Load packages and shared resources
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tibble)
library(showtext)

source("R/paths.R")
source("R/theme_pub.R")

# Configure plot text rendering
font_add("Times New Roman", regular = "times.ttf") 
showtext_opts(dpi = 300)
showtext_auto()

# Read and filter input data
df <- read.csv(data_path("life_span_updating.csv"))

df <- filter(df, Range != 0, !Subject %in% c(229, 241))


# Prepare subject-level summary dataset
df_grouped <- df %>%
  group_by(Subject, ExperimentName, Regression) %>%
  summarize(
    ACC  = mean(ACC, na.rm = TRUE),
    Age  = first(Age),
    Group = first(Group),
    .groups = "drop"
  ) %>%
  mutate(
    Group = factor(Group, levels = c("Children", "Young Adults", "Middle-Aged", "Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian"))
  )

# Build plotting summary dataset
df_summary <- df_grouped %>%
  group_by(ExperimentName, Regression, Group) %>%
  summarize(
    meanACC = mean(ACC, na.rm = TRUE),
    sdACC   = sd(ACC, na.rm = TRUE),
    n       = n(),
    seACC   = sdACC / sqrt(n),
    .groups = "drop"
  ) %>%
  mutate(
    Group = factor(Group, levels = c("Children", "Young Adults", "Middle-Aged", "Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian"))
  )

# Build base plot
p <- ggplot(df_summary, aes(x = Group, y = meanACC)) +
  geom_col(
    aes(fill = Regression),
    position = position_dodge(width = 0.8),
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin  = meanACC - seACC,
      ymax  = meanACC + seACC,
      group = Regression
    ),
    width    = 0.2,
    position = position_dodge(0.8)
  ) +
  facet_wrap(
    ~ ExperimentName,
    labeller = labeller(
      ExperimentName = c("Asian" = "Other-Race", "Caucasian" = "Own-Race")
    )
  ) +
  labs(x = "Group", y = "Accuracy") +
  scale_fill_manual(
    name   = "Regression",
    values = c("biasp" = "#7FB3D5", "biasm" = "#F08080"),
    labels = c("biasp" = "Bias+", "biasm" = "Bias-")
  ) +
  scale_y_continuous(limits = c(0, 1), expand = expansion(mult = c(0, 0))) +
  theme_pub()

# Run paired comparisons across regression levels
stat_test_reg <- df_grouped %>%
  group_by(ExperimentName, Group) %>%
  t_test(ACC ~ Regression, paired = TRUE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p") %>%
  mutate(p.signif = ifelse(nchar(p.signif) > 3, "***", p.signif)) %>%
  add_xy_position(x = "Group", dodge = 0.5, step.increase = 0)

# Set y-position for regression comparison brackets
df_max_reg <- df_summary %>%
  group_by(ExperimentName, Group) %>%
  summarize(max_bar = max(meanACC), .groups = "drop")

stat_test_reg <- stat_test_reg %>%
  left_join(df_max_reg, by = c("ExperimentName", "Group")) %>%
  mutate(y.position = max_bar + 0.05)

# Run sequential group comparisons within each race
group_levels <- levels(df_grouped$Group)
stat_list <- list()

for (exp in unique(df_grouped$ExperimentName)) {
  df_exp <- filter(df_grouped, ExperimentName == exp)
  for (i in seq_len(length(group_levels) - 1)) {
    g1 <- group_levels[i]
    candidates <- group_levels[(i + 1):length(group_levels)]
    
    raw_ps <- sapply(candidates, function(g2) {
      dat <- filter(df_exp, Group %in% c(g1, g2))
      t.test(dat$ACC[dat$Group == g1],
             dat$ACC[dat$Group == g2],
             paired = FALSE)$p.value
    })
    
    adj_ps <- p.adjust(raw_ps, method = "bonferroni")
    sig_idx <- which(adj_ps < 0.05)
    if (length(sig_idx)) {
      j   <- sig_idx[1]
      g2  <- candidates[j]
      p_a <- adj_ps[j]
      
      p_star <- case_when(
        p_a < 0.001 ~ "***",
        p_a < 0.01  ~ "**",
        p_a < 0.05  ~ "*"
      )
      
      stat_list[[length(stat_list) + 1]] <- tibble(
        ExperimentName = exp,
        group1         = g1,
        group2         = g2,
        p.adj          = p_a,
        p.signif       = p_star
      )
    }
  }
}

stat_test_group_custom <- bind_rows(stat_list)

# Set y-position for group comparison brackets
bar_heights <- df_summary %>%
  group_by(ExperimentName, Group) %>%
  summarise(max_bar = max(meanACC), .groups = "drop")

stat_test_group_custom <- stat_test_group_custom %>%
  left_join(bar_heights, by = c("ExperimentName", "group1" = "Group")) %>%
  rename(h1 = max_bar) %>%
  left_join(bar_heights, by = c("ExperimentName", "group2" = "Group")) %>%
  rename(h2 = max_bar) %>%
  group_by(ExperimentName) %>%
  mutate(
    offset     = (row_number() - 1) * 0.03,
    y.position = pmax(h1, h2) + 0.1 + offset
  ) %>%
  ungroup()

# Add statistical brackets and save output plot
stat_test_reg <- stat_test_reg %>%
  mutate(ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")))

stat_test_group_custom <- stat_test_group_custom %>%
  mutate(ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")))

final_plot_ACC <- p +
  stat_pvalue_manual(
    stat_test_reg,
    label        = "p.signif",
    tip.length   = 0.005,
    bracket.size = 0.8,
    size         = 7
  ) +
  stat_pvalue_manual(
    stat_test_group_custom,
    label        = "p.signif",
    tip.length   = 0.005,
    bracket.size = 0.8,
    size         = 7
  )

ggsave(plot_path("ACC_range.png"),
       final_plot_ACC,
       width = 8, height = 6, dpi = 300)
