# Analysis: D-prime histogram faceted by regression with race comparisons
# Input metric: Dprime (from Final D-prime & Criterion.csv)
# Key parameters: Group, ExperimentName (race), Regression (biasm/biasp), Subject
# Output plot: output/plots/Dprime_range_Regression_facet.png
# Output console: paired t-tests (race and group contrasts within each regression facet)

# Load packages and shared resources
library(dplyr)
library(ggplot2)
library(rstatix)
library(ggpubr)
library(tibble)

library(showtext)
# Configure plot text rendering
font_add("Times New Roman", regular = "times.ttf") 
source("R/paths.R")
source("R/theme_pub.R")
showtext_opts(dpi = 300)
showtext_auto()

# Read input data
df <- read.csv(data_path("Final D-prime & Criterion.csv"))

# Prepare subject-level summary dataset
df_grouped <- df %>%
  group_by(Subject, ExperimentName, Regression) %>%
  summarize(
    Dprime = mean(Dprime, na.rm = TRUE),
    CR     = mean(CR, na.rm = TRUE),
    Age    = first(Age),
    Group  = first(Group),
    .groups = "drop"
  )   %>%
  mutate(
    Group = factor(Group, levels = c("Children", "Young Adults", "Middle-Aged", "Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")),
    Regression = factor(Regression, levels = c( "biasm", "biasp"))
    
  )


# Build plotting summary dataset
df_summary <- df_grouped %>%
  group_by(ExperimentName, Regression, Group) %>%
  summarize(
    meanDprime = mean(Dprime, na.rm = TRUE),
    sdDprime   = sd(Dprime, na.rm = TRUE),
    n          = n(),
    seDprime   = sdDprime / sqrt(n),
    .groups    = "drop"
  )  %>%
  mutate(
    Group = factor(Group, levels = c("Children", "Young Adults", "Middle-Aged", "Elderly")),
    ExperimentName = factor(ExperimentName, levels = c("Caucasian", "Asian")),
    Regression = factor(Regression, levels = c( "biasm", "biasp"))
    
  )



# Build base plot (race colors, regression facets)
p <- ggplot(df_summary, aes(x = Group, y = meanDprime)) +
  geom_col(
    # Color bars by race
    aes(fill = ExperimentName),
    position = position_dodge(width = 0.8),
    color = "black"
  ) +
  geom_errorbar(
    aes(
      ymin  = meanDprime - seDprime,
      ymax  = meanDprime + seDprime,
      group = ExperimentName
    ),
    width    = 0.2,
    position = position_dodge(0.8)
  ) +
  facet_wrap(
    ~ Regression,
    labeller = labeller(
      Regression = c("biasp" = "Bias+", "biasm" = "Bias−")
    )
  ) +
  labs(x = "Group", y = "D-prime") +
  # Color palette corresponds to race
  scale_fill_manual(
    name   = "Race",
    values = c("Caucasian" = "#A6D8C3", "Asian" = "#F6B48F"),
    labels = c("Caucasian" = "Own-Race", "Asian" = "Other-Race")
  ) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +
  theme_pub()

# Run paired race comparisons within each regression facet and group
stat_test_race <- df_grouped %>%
  group_by(Regression, Group) %>%
  t_test(Dprime ~ ExperimentName, paired = TRUE) %>%
  adjust_pvalue(method = "bonferroni") %>%
  add_significance("p") %>%
  mutate(p.signif = ifelse(nchar(p.signif) > 3, "***", p.signif)) %>%
  add_xy_position(x = "Group", dodge = 0.5, step.increase = 0)

# Set y-position for race comparison brackets
df_max_race <- df_summary %>%
  group_by(Regression, Group) %>%
  summarize(max_bar = max(meanDprime), .groups = "drop")

stat_test_race <- stat_test_race %>%
  left_join(df_max_race, by = c("Regression", "Group")) %>%
  mutate(y.position = max_bar + 0.1)

# Run sequential group comparisons within each regression facet
group_levels <- levels(df_grouped$Group)
stat_list <- list()

for (bias in unique(df_grouped$Regression)) {
  df_bias <- dplyr::filter(df_grouped, Regression == bias)
  
  for (i in seq_len(length(group_levels) - 1)) {
    g1 <- group_levels[i]
    candidates <- group_levels[(i + 1):length(group_levels)]
    
    raw_ps <- sapply(candidates, function(g2) {
      dat <- dplyr::filter(df_bias, Group %in% c(g1, g2))
      t.test(dat$Dprime[dat$Group == g1],
             dat$Dprime[dat$Group == g2],
             paired = FALSE)$p.value
    })
    
    adj_ps <- p.adjust(raw_ps, method = "bonferroni")
    sig_idx <- which(adj_ps < 0.05)
    if (length(sig_idx)) {
      j   <- sig_idx[1]
      g2  <- candidates[j]
      p_a <- adj_ps[j]
      
      p_star <- dplyr::case_when(
        p_a < 0.001 ~ "***",
        p_a < 0.01  ~ "**",
        p_a < 0.05  ~ "*"
      )
      
      stat_list[[length(stat_list) + 1]] <- tibble::tibble(
        Regression = bias,
        group1     = g1,
        group2     = g2,
        p.adj      = p_a,
        p.signif   = p_star
      )
    }
  }
}

stat_test_group_custom <- dplyr::bind_rows(stat_list)

# Set y-position for group comparison brackets
bar_heights <- df_summary %>%
  group_by(Regression, Group) %>%
  summarise(max_bar = max(meanDprime), .groups = "drop")

stat_test_group_custom <- stat_test_group_custom %>%
  left_join(bar_heights, by = c("Regression", "group1" = "Group")) %>%
  rename(h1 = max_bar) %>%
  left_join(bar_heights, by = c("Regression", "group2" = "Group")) %>%
  rename(h2 = max_bar) %>%
  group_by(Regression) %>%
  mutate(
    offset     = (dplyr::row_number() - 1) * 0.5,
    y.position = pmax(h1, h2) + 1.2 + offset
  ) %>%
  ungroup()

# Add statistical brackets and save output plot
final_plot3 <- p +
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

ggsave(plot_path("Dprime_range_Regression_facet.png"),
       final_plot3, width = 8, height = 6, dpi = 300)
