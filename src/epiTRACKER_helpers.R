# Helpers -----------------------------------------------------------------

library(jsonlite)
library(dplyr)
library(tidyr)
library(magrittr)


# Variables ---------------------------------------------------------------

Variable <- c("Cases - Cumulative", "Cases - Daily",
              "Tests - Cumulative", "Tests - Daily",
              "Deaths - Cumulative", "Deaths - Daily")
VAR <- c("total_cases", "change_cases",
         "total_tests", "change_tests",
         "total_fatalities", "change_fatalities")
LOOKUP <- data.frame(VAR = VAR, 
                     Variable = Variable)

# Query functions ---------------------------------------------------------

get_provinces_lookup <- function(){
  
  df <- jsonlite::fromJSON("https://api.covid19tracker.ca/provinces") %>% 
    dplyr::filter(geographic == 1) %>% 
    dplyr::select(code, name)
  
  return(df) 
  
}

get_health_regions_lookup <- function(){
  
  df <- jsonlite::fromJSON("https://api.covid19tracker.ca/regions")$data %>% 
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
    
    browser()
    
    data_cleaned <- data_raw %>% 
      tidyr::pivot_longer(cols = where(is.integer), 
                          names_to = "VAR", values_to = "Value") %>% 
      dplyr::left_join(LOOKUP, by = "VAR") %>%
      dplyr::select(-VAR) %>% 
      dplyr::filter(!is.na(Variable))
    
    if(province == "All") {
      data_cleaned <- data_cleaned %>% 
        dplyr::mutate(Jurisdisction = "Canada")
    } else {
      data_cleaned <- data_cleaned %>% 
        dplyr::mutate(Jurisdisction = province)
    }
      
    
  }
  
  return(list(data_cleaned = data_cleaned, 
              data_raw = data_raw))
  
}

# //////

make_file_name <- function(stat, choices){
  
  caseName <- paste(
    env$TransferDirectory,
    paste(
      "epiDataCanada", 
      if((choices$Province)=="All") "All_Provinces" else str_replace_all(choices$Province, " ", "_"), 
      # if(choices$Regions) "health_regions", 
      if (choices$IncludeCanada) "including_Canada",
      paste0(stat, ".csv"), 
      sep = "_"
    ), sep = "\\"
  )
  
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

load_data_tracker <- function(mySce){
  
  inputs <- load_inputs_tracker(mySce)

  juris_vector <- c()
  code_vector <- c()
  
  if(inputs$Province == "All"){
    
    all_provinces <- get_provinces_lookup()
    
    code_vector <- c(code_vector, all_provinces$code)
    juris_vector <- c(juris_vector, all_provinces$name)
    
  } else {
    
    prov_code <- get_province_code(inputs$Province)
    
    code_vector <- c(code_vector, prov_code)
    juris_vector <- c(inputs$Province)
    
  }
  
  if(inputs$IncludeCanada){
    code_vector <- c(code_vector, "All")
    juris_vector <- c(juris_vector, "Canada")
  }
  
  all_datasets <- mapply(FUN = get_data, juris_vector, code_vector)
  
  
}

