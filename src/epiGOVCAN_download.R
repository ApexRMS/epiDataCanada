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

TRANSFORMER_NAME <- "Canadian COVID-19 Data: Download from Canadian Government"

VARS <- c("Cases - Cumulative", "Cases - Daily", 
          "Active - Daily", # Active cases are daily by default
          "Recovered - Cumulative", "Recovered - Daily", 
          "Tested - Cumulative", "Tested - Daily",
          "Deaths - Cumulative", "Deaths - Daily")
RAWVARS <- c("numconf", "dailynumconf", 
             "numactive", # Active cases are daily by default
             "numrecover", "dailynumrecover",
             "numtested", "dailynumtested", 
             "numdeaths", "dailynumdeaths")
LOOKUP <- data.frame(VARS = VARS, 
                     RAWVARS = RAWVARS)

# Source helpers ----------------------------------------------------------

source(file.path(E$PackageDirectory, "epiGOVCAN_helpers.R"))

# 1. Load data

filtered_data <- load_inputs_govcan(SCE)

# 2. Save Jurisdictions to EPI

save_to_epi_govcan(SCE, filtered_data$data, VARS)

# 3. Transform data

processed_data <- process_data_govcan(filtered_data$data, LOOKUP) %>%
    mutate(TransformerID=TRANSFORMER_NAME) %>%
    dplyr::filter(!is.na(Value))
saveDatasheet(SCE, processed_data, "epi_DataSummary", append = TRUE)

# 4. Write out data

fileName <- basename(GOVCAN_URL)
filePath <- file.path(E$TransferDirectory, fileName)

write.csv(filtered_data$raw, filePath, row.names = FALSE)

# 5. Save outpout

save_output_govcan(mySce = SCE, inputs = filtered_data$inputs, filePath = filePath)
