## DownloadGovCan Transformer Script

library(rsyncrosim)

# Environment -------------------------------------------------------------

# Load environment

E <- ssimEnvironment()
LIB <- ssimLibrary()
SCE <- scenario()

# TRANSFER_DIR <- e$TransferDirectory

TRANSFORMER_NAME <- "Canadian COVID-19 Data: Download from Covid Tracker"

# Source helpers ----------------------------------------------------------

source(file.path(E$PackageDirectory, "epiTRACKER_helpers.R"))

# 1. Load data

inputs <- load_inputs_tracker(SCE)

data_list <- load_data_tracker(SCE, inputs)

# 2. Save Jurisdictions to EPI

save_to_epi_tracker(SCE, data_list$data_clean, LOOKUP$Variable)

# 3. Fill data and save

processed_data <- data_list$data_clean %>% 
  dplyr::mutate(TransformerID=TRANSFORMER_NAME)
processed_data$Value[is.na(processed_data$Value)] <- 0

saveDatasheet(SCE, processed_data, "epi_DataSummary", append = TRUE)

# 4. Write out data

fileName <- make_file_name_tracker(inputs)
filePath <- file.path(E$TransferDirectory, fileName)

write.csv(data_list$data_raw, filePath, row.names = FALSE)

# 5. Save outpout

save_output_tracker(mySce = SCE, inputs = inputs, 
                    filePath = filePath)
