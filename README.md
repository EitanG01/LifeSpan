# Shaping of Face Representations Throughout the Life Span

## Overview

This repository contains the complete analytical pipeline for examining how face representations and perceptual judgments change across the lifespan, with focus on the effects of stimulus race (own-race vs. other-race bias) and response regression to the mean (RTM bias). The analysis integrates psychometric function fitting, generalized linear mixed models, and generalized additive models applied to behavioral accuracy and signal detection metrics.

**Status:** Preprint  
**Analysis Lead:** Eitan Gelfand - eitan.gelfand@gmail.com
**Contact:** Prof. Bat Sheva Hadad - bhadad@edu.haifa.ac.il

---

## Quick Start

### 1. Set Up Environment

This project uses `renv` for reproducible dependency management. Upon cloning:

```R
renv::restore()
```

This restores all required R packages to the exact versions specified in `renv.lock`.

### 2. Font Setup

All publication-quality figures use **Times New Roman** for consistent formatting. The `showtext` package handles font rendering.

**Required:** Ensure Times New Roman is installed on your system.

**Optional:** If Times New Roman is unavailable, modify the font definition in [R/theme_pub.R](R/theme_pub.R):
```R
# Modify the font family within theme_pub() function
```

### 3. Execution Order

**Start with Weibull fitting first**, as it generates the `slope` parameter used downstream:

---

## File Organization

A compact summary of all analysis scripts is available in [docs/SCRIPT_INDEX.md](docs/SCRIPT_INDEX.md), which maps:
- **Input metric** → behavioral measure being analyzed  
- **Analysis type** → statistical framework (GLMM, GAM, Weibull, descriptive)  
- **Key parameters** → experimental factors (Age, ExperimentName, Regression, Range, Subject)  
- **Output plots** → publication-ready figures  
- **Output data/console** → generated tables or diagnostic summaries

### Main Directories

- **Weibull Project/** — Psychometric function fitting; generates `slope` parameter and downstream prediction data
- **GLMM/** — Trial-level mixed-effects modeling of accuracy with polynomial age terms and cross-validation
- **GAM analysis/** — Nonlinear lifespan trajectories for d-prime and slope using smooth age terms
- **Histograms/** — Grouped comparisons of metrics by age, race, and response bias condition
- **RangeXACC/** — Scatter and trend visualization of accuracy across stimulus difficulty (Range)
- **Updating/** — Parameter estimation plots from updating experiments
- **R/** — Utility functions:
  - `paths.R` — centralized path helpers for data and plot output
  - `theme_pub.R` — shared `ggplot2` theme for publication formatting
- **data/** — processed/aggregated CSV files (raw data not included; see below)
- **output/plots/** — generated figure destination
- **docs/** — documentation and planning files

---

## Execution Workflow

### Stage 1: Psychometric Parameters  
**Primary:** [Weibull Project/Weibull function nlsM.Rmd]
- **Input:** Trial-level response data (`life_span_updating.csv`)
- **Method:** Nonlinear least squares fitting of Weibull psychometric functions per subject
- **Outputs:**
  - Generated data files: `full_data_with_predictions.csv`, `full_results.csv`
  - Generated plot: `Weibull_all_groups_lifespan.png` (plus variants)
  - Console: Model convergence status, parameter estimates, R² values

### Stage 2: Lifespan Trajectories  
**No mandatory order**; choose based on metric of interest:

#### d-Prime (Signal Detection Index)
- [GAM analysis/GAM_age_dprime.R](GAM%20analysis/GAM_age_dprime.R) — Smooth age trends  
- [Histograms/Dprime Histogram.R](Histograms/Dprime%20Histogram.R) — Age group comparisons
- [Histograms/Histogram_R_race_facet.R](Histograms/Histogram_R_race_facet.R) — d-prime by Regression condition

#### Accuracy (ACC)
- [GLMM/GLMM_age_poly.R](GLMM/GLMM_age_poly.R) — Trial-level polynomial GLMM with LOSO validation  
- [Histograms/ACC histogram.R](Histograms/ACC%20histogram.R) — Age group comparisons  
- [Histograms/histogram_acc_ORE_compare.R](Histograms/histogram_acc_ORE_compare.R) — ACC by Regression

#### Slope (from Weibull)  
- [GAM analysis/GAM_age_slope.R](GAM%20analysis/GAM_age_slope.R) — Smooth age trends

#### Other  
- [RangeXACC/Scatter_and_Line.R](RangeXACC/Scatter_and_Line.R) — ACC vs. stimulus difficulty across lifespan  
- [RangeXACC/Scatter_and_Line_Bias_Figure.R](RangeXACC/Scatter_and_Line_Bias_Figure.R) — Faceted by Regression  
- [Updating/new_version_updating.R](Updating/new_version_updating.R) — Parameter estimation plots

---

## Data Structure

### Raw Data (Not Included)

Raw behavioral data is **not distributed** in this repository. Access upon request.

The raw dataset structure:
- **Format:** CSV (trial-level records)
- **Scope:**  participants across lifespan (children to elderly); multiple trials per participant
- **Core columns:**
  - `Subject` — participant ID
  - `Age` — chronological age (years)
  - `ExperimentName` — stimulus race condition (Caucasian own-race, Asian other-race)
  - `Range` — stimulus presentation level (numeric, filtered Range ≠ 0)
  - `Regression` — response bias condition (`biasp` = bias plus, `biasm` = bias minus)
  - `ACC` — trial accuracy (1 = correct, 0 = error)
  - Additional fields: Session, ProcedureBlock, Condition, Distribution, Trial order

### Processed Data (Included)

Generated during analysis:
- `data/full_data_with_predictions.csv` — trial-level data with Weibull predictions  
- `data/full_results.csv` — subject-level aggregated parameters (slope, lambda, k, PSE, JND, R²)  
- `data/dprime_results.csv` — signal detection metrics (d-prime, criterion) by subject and condition  
- `data/final_D-prime_Criterion.csv` — summary d-prime and criterion estimates  
- `data/full_data_life_span.csv` — processed trial-level accuracy data  
- `data/offir_updating_results.csv` — updating experiment parameter estimates

**Data Exclusions:**
- **Subjects 229, 241** — removed due to model convergence failures 
- **Range = 0 trials** — excluded from analysis (no stimulus variation)

---

## Key Experimental Parameters

All analysis scripts operate on combinations of:

| Parameter | Values | Role |
|-----------|--------|------|
| **Age** | Continuous (years) or grouped (Children, Young Adults, Middle-Aged, Elderly) | Primary lifespan predictor |
| **ExperimentName** | Caucasian (own-race), Asian (other-race) | Race bias manipulation |
| **Regression** | biasp (bias plus), biasm (bias minus) | RTM response bias condition |
| **Range** | Numeric (stimulus difficulty levels) | Stimulus perceptual distance |
| **Subject** | Participant ID | Random intercept / grouping unit |

---

## Reproducibility

### Environment Management

All packages and versions are locked via `renv`:
```R
renv::snapshot()  # Capture current state
renv::restore()   # Restore to locked state
```

See [renv.lock](renv.lock) for full package inventory.

### Font Rendering

Figures use Times New Roman for consistent publication-style formatting. Plots rely on the `showtext` package to render the font.

**Users should ensure Times New Roman is installed on their system, or modify the font in [R/theme_pub.R](R/theme_pub.R) if needed.**

### Computational Requirements

- **R version:** 4.5+
- **Key packages:** `dplyr`, `ggplot2`, `mgcv`, `glmmTMB`, `emmeans`, `minpack.lm`, `tidyverse`, `rstatix`, `DHARMa`, `showtext`
- **Estimated runtime:** ~5–10 minutes for full pipeline on modern hardware
- **Output space:** ~100 MB (figures + generated data)

---

## Known Issues & Data Quality Notes

### Subject Exclusion (229, 241)

Two participants were removed from the analysis following discovery of **model non-convergence** in psychometric function fitting.


**Reason:** Weibull fitting algorithm failed to converge to valid parameter estimates for these participants under the nonlinear least squares optimization, likely due to atypical response patterns or insufficient trial variability.

**Impact:** All downstream analyses inherit this exclusion. Reported sample sizes and statistics reflect *N* with these two subjects removed.

For additional diagnostic details and a full discussion of data quality, see the associated manuscript.

---

## Visualization Output

All figures are generated in publication-ready format:

- **Font:** Times New Roman, 16pt (via `showtext` + `theme_pub()`)
- **Dimensions:** Optimized for print media (dpi = 300)
- **Theme:** Black & white base with publication conventions (no grids, centered legends)
- **Color conventions:**
  - Regression condition: biasp (blue), biasm (coral)
  - Race condition: Own-Race (teal), Other-Race (salmon)

Figures are saved to `output/plots/` and referenced in manuscript.

---

## For More Context

The analytical methods, theoretical background, hypotheses, and full discussion of results are presented in the associated manuscript. This repository is intended to support **reproducibility and transparency** of the quantitative analyses.

**To understand the research questions and scientific context, please consult the manuscript.**

---

## License

This repository is licensed under the **MIT License**.

---

**Last Updated:** March 2026