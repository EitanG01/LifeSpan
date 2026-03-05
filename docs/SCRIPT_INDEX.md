# Script Index (R / Rmd)

Purpose: minimal navigation for researchers and reviewers.

| File | Analysis Type | Input Metric | Key Parameters | Output Plot(s) | Output Data (if any) |
|---|---|---|---|---|---|
| GAM analysis/GAM_age_dprime.R | GAM + model comparison | D-prime | Age, ExperimentName, Regression, Subject | output/plots/dprime_gam_lifespan.png | - |
| GAM analysis/GAM_age_slope.R | GAM + model comparison | Slope | Age, ExperimentName, Regression, Subject | output/plots/slope_gam_lifespan.png | - |
| GLMM/GLMM_age_poly.R | GLMM + model comparison + LOSO | ACC | Age, ExperimentName, Regression, Range, Subject | output/plots/age_accuracy_plot_lifespan_final2.png | output/loso_results.csv |
| Histograms/ACC histogram.R | Descriptive + paired tests | ACC | Group, ExperimentName, Regression, Subject | output/plots/ACC_range.png | - |
| Histograms/Dprime Histogram.R | Descriptive + paired tests | D-prime | Group, ExperimentName, Regression, Subject | output/plots/Dprime_range.png | - |
| Histograms/Histogram_R_race_facet.R | Descriptive + paired tests | D-prime | Group, ExperimentName, Regression, Subject | output/plots/Dprime_range_Regression_facet.png | - |
| Histograms/histogram_acc_ORE_compare.R | Descriptive + paired tests | ACC | Group, ExperimentName, Regression, Subject | output/plots/ACC_range_Regression_facet.png | - |
| R/paths.R | Utility helper | - | - | - | - |
| R/theme_pub.R | Utility theme helper | - | - | - | - |
| RangeXACC/Scatter_and_Line.R | Descriptive trajectories | ACC | Group, ExperimentName, Range, Regression, Subject | output/plots/acc_range_lifespan.png | - |
| RangeXACC/Scatter_and_Line_Bias_Figure.R | Descriptive trajectories | ACC | Group, ExperimentName, Range, Regression, Subject | output/plots/acc_range_lifespan_bias_facet.png | - |
| Updating/new_version_updating.R | Descriptive plotting | Updating beta | AgeGroup, Race | output/plots/offir_updating_own_race.png; output/plots/offir_updating_other_race.png | - |
| Weibull Project/Weibull function nlsM.Rmd | Weibull psychometric fitting | ACC (by Range) | Range, ExperimentName, Group, Regression, Subject | output/plots/Weibull_all_groups_lifespan.png (+ additional Weibull plot exports) | data/full_data_with_predictions.csv; data/full_results.csv |
| renv/activate.R | Environment bootstrap | - | - | - | - (excluded from scientific documentation pass) |
