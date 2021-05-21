## DownloadGovCan Transformer Script

library(rsyncrosim)

# Environment -------------------------------------------------------------

# Load libraries
library(dplyr)
library(tidyr)
library(readr)
library(purrr)

# Load environment

E <- ssimEnvironment()
LIB <- ssimLibrary()
SCE <- scenario()

# TRANSFER_DIR <- e$TransferDirectory

TRANSFORMER_NAME <- "Canadian COVID-19 Data: Download from Covid Tracker"

# Source helpers ----------------------------------------------------------

source(file.path(E$PackageDirectory, "epiTRACKER_helpers.R"))

# 1. Load data

filtered_data <- load_data_tracker(SCE)

# 2. Save Jurisdictions to EPI

save_to_epi_tracker(SCE, filtered_data$data, VARS)

# 3. Transform data

processed_data <- process_data_tracker(filtered_data$data, LOOKUP) %>%
    mutate(TransformerID=TRANSFORMER_NAME)
processed_data$Value[is.na(processed_data$Value)] <- 0
saveDatasheet(SCE, processed_data, "epi_DataSummary", append = TRUE)

# 4. Write out data

# fileName <- basename(GOVCAN_URL)
# filePath <- file.path(E$TransferDirectory, fileName)

# write.csv(filtered_data$raw, filePath, row.names = FALSE)

# 5. Save outpout

save_output_tracker(mySce = SCE, inputs = filtered_data$inputs, filePath = filePath)
