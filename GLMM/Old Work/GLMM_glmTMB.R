# Load required packages
library(glmmTMB)
library(dplyr)
library(emmeans)
library(DHARMa)
library(marginaleffects)



# 1. Read & preprocess
df <- read.csv("C:/Users/eitas/OneDrive/University/Semester D/Galia's Lab/R/Aging Experiment/full_data.csv") %>%
  filter(Range != 0)

# center & scale Range in one step
df$Range_c <- scale(df$Range, center = TRUE, scale = TRUE)


#----older models 
# # Base model (random intercept only)
# m0_tmb <- glmmTMB(
#   ACC ~ Group
#   + Regression
#   + ExperimentName
#   + Range_c
#   + (1 | Subject),
#   data   = df,
#   family = binomial(link = "probit")
# )
# summary(m0_tmb)
# 
# # m1: add Group × Regression
# m1_tmb <- update(m0_tmb, . ~ . + Group:Regression)
# anova(m0_tmb, m1_tmb)
# summary(m1_tmb)
# 
# # m2: add Group × ExperimentName
# m2_tmb <- update(m1_tmb, . ~ . + Group:ExperimentName)
# anova(m1_tmb, m2_tmb)
# summary(m2_tmb)
# 
# # m3: add Group × Range_c
# m3_tmb <- update(m2_tmb, . ~ . + Group:Range_c)
# anova(m2_tmb, m3_tmb)
# summary(m3_tmb)
# 
# # m4: add the three‐way Group × Regression × ExperimentName
# m4_tmb <- update(m3_tmb, . ~ . + Group:Regression:ExperimentName)
# anova(m3_tmb, m4_tmb)
# summary(m4_tmb)


#-----current model

m5_tmb <- glmmTMB(
  ACC ~ Group
  + Regression
  + ExperimentName
  + Range_c
  + Group:Regression
  + Group:ExperimentName
  + Group:Range_c
  + Group:Regression:ExperimentName
  + (1 + Regression + ExperimentName + Range_c | Subject),  # intercept + all slopes
  data    = df,
  family  = binomial(link = "probit")
)

summary(m5_tmb)




#---- compare to other link function 
# m5_tmb_cloglog <- glmmTMB(
#   ACC ~ Group
#   + Regression
#   + ExperimentName
#   + Range_c
#   + Group:Regression
#   + Group:ExperimentName
#   + Group:Range_c
#   + Group:Regression:ExperimentName
#   + (1 + Regression + ExperimentName + Range_c | Subject),  # intercept + all slopes
#   data    = df,
#   family  = binomial(link = "cloglog")
# )
# anova(m5_tmb, m5_tmb_cloglog)  # compare probit vs cloglog link





#----- Final model analysis 

# Check residuals model diagnostics
sim <- simulateResiduals(m5_tmb)
plot(sim)            # QQ‐plot & residuals vs. fitted
testUniformity(sim)  # Kolmogorov–Smirnov for uniformity
testDispersion(sim)  # over/under‐dispersion check


# model summary
summary(m5_tmb)


# biasp vs biasm in each Group×Race at mean Range
emmeans(m5_tmb, ~ Regression | Group*ExperimentName, at=list(Range_c=0)) %>%
  contrast("pairwise") %>% summary(infer=TRUE, type="response")

# biasp vs biasm in each Group×Race at mean Range
emmeans(m5_tmb, ~ ExperimentName | Group*Regression, at=list(Range_c=0)) %>%
  contrast("pairwise") %>% summary(infer=TRUE, type="response")

# Range sensitivity in each cell
model_trends <- emtrends(m5_tmb, ~ Group*Regression*ExperimentName, var="Range_c") %>%
  summary(infer=TRUE)




# # marginaltrends package
# slopes_of_range <- slopes(
#   m5_tmb,
#   variables = "Range_c",
#   by = c("Group", "Regression", "ExperimentName")
# )
# # Step 2: Perform pairwise comparisons
# pairwise_comparisons <- hypotheses(slopes_of_range, hypothesis = "pairwise")
# print(pairwise_comparisons)






# 1. Estimate the slopes (trends) of Range_c for each condition
em_slopes <- emtrends(
  m5_tmb,
  specs = ~ Group | ExperimentName | Regression,
  var = "Range_c"
)

print(em_slopes)
# 2. Perform pairwise comparisons of those slopes
# This performs all pairwise comparisons with p-value adjustment
contrast_results <- pairs(em_slopes, adjust = "tukey")

# 3. View the final results
print(contrast_results)



slopes_of_range <- slopes(
  m5_tmb,
  variables = "Range_c",
  by = c("Group","ExperimentName", "Regression" )
)
print(slopes_of_range)


