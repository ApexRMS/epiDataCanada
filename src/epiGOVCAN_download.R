## DownloadGovCan Transformer Script

library(rsyncrosim)

# Environment -------------------------------------------------------------

# Load libraries
library(dplyr)
library(tidyr)
library(readr)

# Load environment

E <- ssimEnvironment()
LIB <- ssimLibrary()
SCE <- scenario()

# TRANSFER_DIR <- e$TransferDirectory

TRANSFORMER_NAME <- "Canadian COVID-19 Data: Download from Canadian Government"
VARS <- c("Cases", "Tested", 
          "Cases - Active", "Cases - Recovered", 
          "Deaths - Daily", "Deaths - Cumulative")

# Source helpers ----------------------------------------------------------

source(file.path(E$PackageDirectory, "epiGOVCAN_helpers.R"))

# 1. Load data

filtered_data <- load_inputs_govcan(SCE)
