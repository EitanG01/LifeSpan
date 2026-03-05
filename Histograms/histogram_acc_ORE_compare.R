# Analysis: ACC histogram faceted by regression with race and group comparisons
# Input metric: ACC (from life_span_updating.csv)
# Key parameters: Group, ExperimentName (race), Regression (biasm/biasp), Subject
# Output plot: output/plots/ACC_range_Regression_facet.png
# Output console: paired t-tests (race and group contrasts within each regression facet)

# ----------------------------------------------------
# Load packages and shared resources
# ----------------------------------------------------
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tibble)
library(showtext)
library(scales)

source("R/paths.R")
source("R/theme_pub.R")

# Configure plot text rendering
font_add("Times New Roman", regular = "times.ttf") 
showtext_opts(dpi = 300)
showtext_auto()

# ----------------------------------------------------
# Read and filter input data
# ----------------------------------------------------
df <- read.csv(
  data_path("life_span_updating.csv")
)%>%
  filter(Range != 0)

# ----------------------------------------------------
# Prepare subject-level summary dataset
# ----------------------------------------------------
df_grouped <- df %>%
  group_by(Subject, ExperimentName, Regression) %>%
  summarize(
    ACC   = mean(ACC, na.rm = TRUE),
    Age   = first(Age),
    Group = first(Group),
    .groups = "drop"
  ) %>%
  mutate(
    Group = factor(Group, levels = c("Children", "Young Adults", "Middle-Aged", "Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")),
    Regression = factor(Regression, levels = c("biasm", "biasp"))
  )

# ----------------------------------------------------
# Build plotting summary dataset
# ----------------------------------------------------
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
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")),
    Regression = factor(Regression, levels = c("biasm", "biasp"))
  )

# ----------------------------------------------------
# Build base plot
# ----------------------------------------------------
p <- ggplot(df_summary, aes(x = Group, y = meanACC)) +
  geom_col(
    aes(fill = ExperimentName),
    position = position_dodge(width = 0.8),
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin  = meanACC - seACC,
      ymax  = meanACC + seACC,
      group = ExperimentName
    ),
    width = 0.2,
    position = position_dodge(0.8)
  ) +
  facet_wrap(
    ~ Regression,
    labeller = labeller(
      Regression = c("biasp" = "Bias+", "biasm" = "Bias−")
    )
  ) +
  labs(x = "Group", y = "Accuracy") +
  scale_fill_manual(
    name   = "Race",
    values = c("Caucasian" = "#A6D8C3", "Asian" = "#F6B48F"),
    labels = c("Caucasian" = "Own-Race", "Asian" = "Other-Race")
  ) +
  scale_y_continuous(limits = c(0, 1)) +
  theme_pub()

# ----------------------------------------------------
# Run paired race comparisons within each regression facet and group
# ----------------------------------------------------
stat_test_race <- df_grouped %>%
  group_by(Regression, Group) %>%
  t_test(ACC ~ ExperimentName, paired = TRUE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p") %>%
  mutate(p.signif = ifelse(nchar(p.signif) > 3, "***", p.signif)) %>%
  add_xy_position(x = "Group", dodge = 0.5, step.increase = 0)

df_max_race <- df_summary %>%
  group_by(Regression, Group) %>%
  summarize(max_bar = max(meanACC), .groups = "drop")

stat_test_race <- stat_test_race %>%
  left_join(df_max_race, by = c("Regression", "Group")) %>%
  mutate(y.position = max_bar + 0.05)

# ----------------------------------------------------
# Run sequential group comparisons within each regression facet
# ----------------------------------------------------
group_levels <- levels(df_grouped$Group)
stat_list <- list()

for (bias in unique(df_grouped$Regression)) {
  df_bias <- filter(df_grouped, Regression == bias)
  
  for (i in seq_len(length(group_levels) - 1)) {
    g1 <- group_levels[i]
    candidates <- group_levels[(i + 1):length(group_levels)]
    
    raw_ps <- sapply(candidates, function(g2) {
      dat <- filter(df_bias, Group %in% c(g1, g2))
      t.test(
        dat$ACC[dat$Group == g1],
        dat$ACC[dat$Group == g2],
        paired = FALSE
      )$p.value
    })
    
    adj_ps <- p.adjust(raw_ps, method = "bonferroni")
    sig_idx <- which(adj_ps < 0.05)
    
    if (length(sig_idx)) {
      j  <- sig_idx[1]
      g2 <- candidates[j]
      p_a <- adj_ps[j]
      
      p_star <- case_when(
        p_a < 0.001 ~ "***",
        p_a < 0.01  ~ "**",
        p_a < 0.05  ~ "*"
      )
      
      stat_list[[length(stat_list) + 1]] <- tibble(
        Regression = bias,
        group1     = g1,
        group2     = g2,
        p.adj      = p_a,
        p.signif   = p_star
      )
    }
  }
}

stat_test_group_custom <- bind_rows(stat_list)

bar_heights <- df_summary %>%
  group_by(Regression, Group) %>%
  summarize(max_bar = max(meanACC), .groups = "drop")

stat_test_group_custom <- stat_test_group_custom %>%
  left_join(bar_heights, by = c("Regression", "group1" = "Group")) %>%
  rename(h1 = max_bar) %>%
  left_join(bar_heights, by = c("Regression", "group2" = "Group")) %>%
  rename(h2 = max_bar) %>%
  group_by(Regression) %>%
  mutate(
    offset     = (row_number() - 1) * 0.1,
    y.position = pmax(h1, h2) + 0.12 + offset
  ) %>%
  ungroup()






# ----------------------------------------------------
# Add statistical brackets
# ----------------------------------------------------
final_plot_ACC <- p +
  stat_pvalue_manual(
    stat_test_race,
    label        = "p.signif",
    tip.length   = 0.005,
    bracket.size = 0.8,
    size         = 7,
    facet.by     = "Regression"
  ) +
  stat_pvalue_manual(
    stat_test_group_custom,
    label        = "p.signif",
    tip.length   = 0.005,
    bracket.size = 0.8,
    size         = 7,
    facet.by     = "Regression"
  )

# ----------------------------------------------------
# 8) Save
# ----------------------------------------------------
ggsave(
  plot_path("ACC_range_Regression_facet.png"),
  final_plot_ACC,
  width = 8,
  height = 6,
  dpi = 300
)
