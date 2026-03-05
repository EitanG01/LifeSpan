# R/paths.R
# Centralized path definitions for the project
# Assumes scripts run from project root so relative data/output paths resolve consistently.

# Folders inside the project
DATA_DIR   <- "data"
OUTPUT_DIR <- "output"
PLOTS_DIR  <- file.path(OUTPUT_DIR, "plots")

# Helper functions to build full paths
data_path <- function(...) file.path(DATA_DIR, ...)
plot_path <- function(...) file.path(PLOTS_DIR, ...)

# Ensure output folders exist
if (!dir.exists(OUTPUT_DIR)) dir.create(OUTPUT_DIR)
if (!dir.exists(PLOTS_DIR)) dir.create(PLOTS_DIR, recursive = TRUE)