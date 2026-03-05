# Documentation Review Plan (Tracked R / Rmd Files)

Date: 2026-03-05  
Scope: All tracked .R and .Rmd files from git ls-files '*.R' '*.Rmd'.  
Approach: Review plan only (no script edits in this phase).

## Unified Documentation Standard (Minimal, Researcher-Focused)

The same documentation style will be applied across all analysis scripts.

For each analysis script, include:

1. Script header (5-8 lines)
   - Scientific purpose of the analysis
   - Input metric(s)
   - Key parameters: Age, Race/ExperimentName, Range, Regression, Subject (as relevant)
   - Output plot(s)
   - Output data/table files (only when produced)
2. Section flow comments only
   - Load packages/resources
   - Read/filter/prepare data
   - Fit model(s) or compute summaries
   - Build visualization(s)
   - Save outputs
3. Comment style rule
   - Explain analysis flow and intent, not programming syntax
   - No tutorials or extended methodological teaching text

Note: Regression (biasp, biasm) is documented as one regular analysis parameter, alongside the other parameters.

## Per-File Review Plan

| File | Role | Doc Status | Required Improvements | Effort |
|---|---|---|---|---|
| GAM analysis/GAM_age_dprime.R | GAM lifespan trajectory for d-prime by race and regression | Good structure, missing explicit scientific header | Add unified header (input metric, parameters, output plot); normalize section titles to common flow style | Small |
| GAM analysis/GAM_age_slope.R | GAM lifespan trajectory for slope by race and regression | Good structure, missing explicit scientific header | Add unified header; clarify output plot and model-comparison section labels | Small |
| GLMM/GLMM_age_poly.R | Trial-level ACC mixed modeling with model comparisons and LOSO | Partially documented, dense script | Add unified top header; add clear flow block comments for model fitting, predictions, LOSO, comparisons, post-hoc summaries | Medium |
| Histograms/ACC histogram.R | ACC histogram by group/race/regression with paired comparisons | Basic section comments present | Add unified header; align section labels to common flow sequence | Small |
| Histograms/Dprime Histogram.R | D-prime histogram by group/race/regression with paired comparisons | Basic section comments present | Add unified header; align section labels to common flow sequence | Small |
| Histograms/Histogram_R_race_facet.R | D-prime histogram faceted by regression with race comparisons | Comments include change-log style text | Replace change-log comments with stable intent comments; add unified header | Small |
| Histograms/histogram_acc_ORE_compare.R | ACC histogram faceted by regression with race/group comparisons | Good existing documentation | Add unified short header; keep flow comments minimal and consistent | Small |
| R/paths.R | Centralized path helper utilities | Clear and adequate | Keep as is; optional one-line context note only | Small |
| R/theme_pub.R | Shared publication plotting theme helper | Function clear, no context header | Add short file header + one function-purpose comment | Small |
| RangeXACC/Scatter_and_Line.R | ACC vs Range trajectories across groups/race/regression | Good comments, formatting separators are noisy | Add unified top header; simplify decorative separators; preserve minimal flow comments | Small |
| RangeXACC/Scatter_and_Line_Bias_Figure.R | ACC vs Range trajectories with regression facets and race colors | Good comments plus change-log notes | Convert change-log notes to stable intent comments; add unified header | Small |
| Updating/new_version_updating.R | Updating parameter plots by race across age groups | Minimal and readable | Add unified header with input metric and output plots; add minimal flow labels | Small |
| Weibull Project/Weibull function nlsM.Rmd | Weibull psychometric fitting, parameter extraction, plots, and exported data | Functional comments but fragmented | Add structured Rmd narrative flow headers; clearly document both output plots and output data files | Medium |
| renv/activate.R | Environment bootstrap script (generated) | Not analysis documentation target | Exclude from manual scientific documentation pass | None |

## Execution Sequence for Phase 2 (Actual Edits)

Apply the same style to all analysis scripts in repository path order.

1. GAM analysis/
2. GLMM/
3. Histograms/
4. R/ utilities (light touch)
5. RangeXACC/
6. Updating/
7. Weibull Project/

Exclude renv/activate.R.

## Navigation Document Rule

Keep docs/SCRIPT_INDEX.md minimal and practical, focused on:

- file
- analysis type
- input metric
- key parameters
- output plot(s)
- output data file(s) only when present (notably Weibull)

## Ready for Next Step

After approval, Phase 2 will apply minimal in-file documentation edits only (no modeling or logic changes).
