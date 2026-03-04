# R/00_setup.R
# ============================================
# Load core packages and set up project folders
# ============================================

# Ensure dplyr::select is available as select()
select <- dplyr::select

# Load required packages
library(dplyr)
library(lubridate)
library(readr)
library(readxl)
library(stringr)
library(tidyr)
library(ggplot2)
library(scales)

# Create required folder structure (if not already present)
dir.create("data/clean",          showWarnings = FALSE, recursive = TRUE)
dir.create("data/raw",            showWarnings = FALSE, recursive = TRUE)
dir.create("figures/raw",         showWarnings = FALSE, recursive = TRUE)
dir.create("output/clean_data",   showWarnings = FALSE, recursive = TRUE)
dir.create("output/model_objects", showWarnings = FALSE, recursive = TRUE)
