# Helpers -----------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(tidyr)
library(magrittr)
library(stringr)

# Variables ---------------------------------------------------------------

Variable <- c("Cases - Cumulative", "Cases - Daily",
              "Tests - Cumulative", "Tests - Daily",
              "Deaths - Cumulative", "Deaths - Daily",
              "In ICU - Daily", 
              "Recovered - Cumulative", "Recovered - Daily",
              "Vaccinations - Cumulative", "Vaccinations - Daily",
              "In Hospital - Daily")
VAR <- c("total_cases", "change_cases",
         "total_tests", "change_tests",
         "total_fatalities", "change_fatalities", 
         "total_criticals", 
         "total_recoveries", "change_recoveries", 
         "total_vaccinations", "change_vaccinations",
         "total_hospitalizations")
LOOKUP <- data.frame(VAR = VAR, 
                     Variable = Variable)

TRACKER_URL <- "https://api.covid19tracker.ca/provinces"

# Query functions ---------------------------------------------------------

get_provinces_lookup <- function(base_url = TRACKER_URL){
  
  df <- jsonlite::fromJSON(base_url) %>% 
    dplyr::filter(geographic == 1) %>% 
    dplyr::select(code, name)
  
  return(df) 
  
}

get_health_regions_lookup <- function(){
  
  base_url <- "https://api.covid19tracker.ca/regions"
  
  df <- jsonlite::fromJSON(base_url)$data %>% 
    dplyr::select(hr_uid, province, engname) %>% 
    left_join(get_provinces_lookup(), by = c("province"="code"))
  
  return(df) 
  
}

get_province_code <- function(province){
  
  lookup <- get_provinces_lookup()
  code <- lookup[lookup$name == province,]$code
  
  return(code)
  
}

build_url_query <- function(base = "https://api.covid19tracker.ca/reports/", 
                            province_code, HR = FALSE){
  
  # Note: could use fill_dates query parameter to supply complete combinations
  
  if(!HR){
    
    if (province_code == "All"){
      loc_filter = ""
    } else {
      loc_filter <- paste0("province/", province_code)
    }
    
    url <- paste0(base, loc_filter, "?per_page=1000")
    
  } else {
    
    stop(paste0("Only provinces are supported at the moment ",
                "(but see function get_health_regions_lookup)"))
    
  }
  
  return(url)
  
}

get_dataset <- function(url){
  
  ret <- jsonlite::fromJSON(url)$data
  
}

get_data <- function(province, province_code){
  
  the_url <- build_url_query(province_code = province_code)
  
  data_raw <- tryCatch({
    get_dataset(the_url) },
    error = function(cond) {
      warning(paste0("The URL ", the_url, " failed."))
      return(NULL)
    }, 
    warning = function(cond){
      warning(paste0("The URL ", the_url, " failed."))
      return(NULL)
    }
  )
  
  if (is.null(data_raw)){
    
    return(NULL)
    
  } else {
    
    data_cleaned <- data_raw %>% 
      tidyr::pivot_longer(cols = where(is.integer), 
                          names_to = "VAR", values_to = "Value") %>% 
      dplyr::left_join(LOOKUP, by = "VAR") %>%
      dplyr::select(-VAR) %>% 
      dplyr::filter(!is.na(Variable)) %>% 
      dplyr::rename(Timestep=date)
    
    if(province == "Canada") {
      data_cleaned <- data_cleaned %>% 
        dplyr::mutate(Jurisdiction = "Canada")
    } else {
      data_cleaned <- data_cleaned %>% 
        dplyr::mutate(Jurisdiction = paste0("Canada - ", province))
    }
    
  }
  
  return(list(data_cleaned = data_cleaned, 
              data_raw = data_raw %>% 
                dplyr::mutate(code = province_code)))
  
}

# Package functions -------------------------------------------------------

load_inputs_tracker <- function(mySce){
  
  # Process inputs (simple, no need for special functiom)
  inputs <- datasheet(mySce, "epiDataCanada_TrackerInputs", lookupsAsFactors = FALSE)
  
  if(is.na(inputs$ProvinceTerritory)){
    inputs$ProvinceTerritory <- "All"
  }
  
  return(inputs)
  
}

load_data_tracker <- function(mySce, inputs){
  
  juris_vector <- c()
  code_vector <- c()
  
  if(inputs$ProvinceTerritory == "All"){
    
    all_provinces <- get_provinces_lookup()
    
    code_vector <- c(code_vector, all_provinces$code)
    juris_vector <- c(juris_vector, all_provinces$name)
    
  } else {
    
    prov_code <- get_province_code(inputs$ProvinceTerritory)
    
    code_vector <- c(code_vector, prov_code)
    juris_vector <- c(inputs$ProvinceTerritory)
    
  }
  
  if(inputs$IncludeCanada == "Yes"){
    code_vector <- c(code_vector, "All")
    juris_vector <- c(juris_vector, "Canada")
  }
  
  all_datasets <- mapply(FUN = get_data, juris_vector, code_vector, 
                         SIMPLIFY = FALSE)
  
  data_cleaned <- lapply(all_datasets, `[[`, "data_cleaned") %>% 
    bind_rows()
  data_raw <- lapply(all_datasets, `[[`, "data_raw") %>% 
    bind_rows()
  
  return(list(data_cleaned = data_cleaned, 
              data_raw = data_raw))
  
}

save_to_epi_tracker <- function(mySce, df, vars){
  
  # Get the vector of jurisdictions
  allJuris <- unique(df$Jurisdiction)
  
  # Add the required variables and jurisdictions to the SyncroSim project
  saveDatasheet(mySce, 
                data.frame(Name = allJuris), "epi_Jurisdiction")
  saveDatasheet(mySce, 
                data.frame(Name = vars), "epi_Variable")
  
}

make_file_name_tracker <- function(inputs){
  
  file_name <- paste0(
    "epiDataCanada_CovidTracker_", 
    if((inputs$ProvinceTerritory)=="All") "All_Provinces" else 
      stringr::str_replace_all(inputs$ProvinceTerritory, " ", "_"), 
    if (inputs$IncludeCanada == "Yes") "_including_Canada",
    "_reports.csv"
  )
  
}

save_output_tracker <- function(mySce, inputs, filePath){
  
  download_time <- as.character(Sys.time())
  
  output <- datasheet(mySce, "epiDataCanada_GovcanOutputs") %>% 
    dplyr::add_row()
  
  output$Jurisdiction = inputs$ProvinceTerritory
  output$DataSourceID = "Covid Tracker"
  output$DownloadFile = filePath
  output$DownloadURL = TRACKER_URL
  output$DownloadDateTime = download_time
  
  saveDatasheet(mySce, output, "epiDataCanada_TrackerOutputs")
}
