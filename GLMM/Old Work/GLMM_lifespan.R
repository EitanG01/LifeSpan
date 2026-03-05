library(lme4)
library(dplyr)
library(emmeans)


df <- read.csv(
  "C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/full_data.csv"
) %>%
  filter(Range != 0)


# center scale range variable
df$Range_c <- scale(df$Range, center = TRUE, scale = TRUE)

# Fit the GLMM model

m0 <- glmer(
  ACC ~ Group
  + Regression
  + ExperimentName
  + Range_c
  + (1 | Subject),
  data    = df,
  family=binomial(link="probit"),
  control = glmerControl(optimizer = "bobyqa")
)

summary(m0)


m1 <- update(m0, . ~ . + Group:Regression)

anova(m0, m1)
summary(m1)



# m2: add AgeGroup × ExperimentName
m2 <- update(m1, . ~ . + Group:ExperimentName)
anova(m1, m2)   # LRT: does ethnic‐match vary with age?
summary(m2)


# m3: add AgeGroup × Range_c
m3 <- update(m2, . ~ . + Group:Range)
anova(m2, m3)   # LRT: does intensity effect change across ages?
summary(m3)



# m4: add Group × Regression × ExperimentName
m4 <- update(m3, . ~ . + Group:Regression:ExperimentName)
anova(m3, m4)   # LRT: is there a three‐way bias × race × age interaction?
summary(m4)
# 
# # m5: add Group × Regression × Range_c
# m5 <- update(m4, . ~ . + Group:Regression:Range_c)
# anova(m4, m5)   # LRT: does the bias effect × intensity differ by age?
# 
# # m6: add Group × ExperimentName × Range_c
# m6 <- update(m5, . ~ . + Group:ExperimentName:Range_c)
# anova(m5, m6)   # LRT: does the race effect × intensity differ by age?





# 1) Simple‐slopes for Regression (biasp vs. biasm)
#    at each combination of Group × ExperimentName, holding Range at its mean
df_reg <- emmeans(m4,
                  ~ Regression | Group * ExperimentName,
                  at = list(Range_c = 0)
) %>%
  contrast(method = "pairwise") %>%
  as.data.frame() %>%
  mutate(OR = exp(estimate))

df_reg

def_race <- emmeans(m4,
                    ~ ExperimentName | Group * Regression,
                    at = list(Range_c = 0)
) %>%
  contrast(method = "pairwise") %>%
  as.data.frame() %>%
  mutate(OR = exp(estimate))

def_race

# def_range <- emmeans(m4,
#                     ~ Range_c | Group * Regression * ExperimentName,
#                     
# ) %>%
#   contrast(method = "pairwise") %>%
#   as.data.frame() %>%
#   mutate(OR = exp(estimate))
# 
# def_range



# 1. Compute the trend (slope) of Range_c within each Group×Regression×ExperimentName
range_trends <- emtrends(
  m4,
  ~ Group * Regression * ExperimentName,  # stratify by these
  var = "Range_c"                        # get the slope for Range_c
)

# 2. Coerce to a data frame and exponentiate to get an OR per 1-unit increase in Range_c
df_range <- as.data.frame(range_trends) %>%
  mutate(OR = exp(Range_c.trend))

# 3. Take a look
df_range