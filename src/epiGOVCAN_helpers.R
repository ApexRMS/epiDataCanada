
# Helpers -----------------------------------------------------------------

load_inputs_govcan <- function(mySce){
  
  # Process inputs (simple, no need for special functiom)
  inputs <- datasheet(mySce, "epiDataCanada_GovcanInputs", lookupsAsFactors = FALSE)
  
  if(is.na(inputs$ProvinceTerritory)){
    inputs$ProvinceTerritory <- "All"
  }
  
  # Download and pre-filter data based on the inputs parameters
  raw_data <- 
    read_csv("https://health-infobase.canada.ca/src/data/covidLive/covid19-download.csv") %>% 
    filter(prname != "Repatriated travellers") %>% 
    rename(Jurisdiction = prname, Timestep = date) %>% 
    select(-c(pruid, prnameFR, update))

  # Edit jurisdiction
  raw_data$Jurisdiction[raw_data$Jurisdiction != "Canada"]  <- 
    paste0("Canada - ", raw_data$Jurisdiction)
  
  if(inputs$IncludeCanada == "No"){
    
    raw_data <- raw_data %>% 
      filter(Jurisdiction != "Canada")
    
    if (inputs$ProvinceTerritory == "None"){
      
      stop("No province requested and Canada not included, no data to return.")
      
    } else if(inputs$ProvinceTerritory == "All"){
      
      return(list(data = raw_data, 
                  inputs = inputs))
      
    } else {
      
      raw_data <- raw_data %>% 
        filter(Jurisdiction == paste0("Canada - ", inputs$ProvinceTerritory))
      
      return(list(data = raw_data, 
                  inputs = inputs))
      
    }
    
  } else {

    if (inputs$ProvinceTerritory == "None"){
      
       raw_data <- raw_data %>% 
      filter(Jurisdiction == "Canada")

      return(list(data = raw_data, 
                  inputs = inputs))
      
    } else if(inputs$ProvinceTerritory == "All"){
      
      return(list(data = raw_data, 
                  inputs = inputs))
      
    } else {
      
      raw_data <- raw_data %>% 
        filter(Jurisdiction %in% c("Canada", paste0("Canada - ", inputs$ProvinceTerritory)))
      
      return(list(data = raw_data, 
                  inputs = inputs))
      
    }

  }
  
}

save_to_epi_govcan <- function(mySce, df, vars){
  
  # Get the vector of jurisdictions
  allJuris <- unique(df$Jurisdiction)
  
  # Add the required variables and jurisdictions to the SyncroSim project
  saveDatasheet(mySce, 
                data.frame(Name = allJuris), "epi_Jurisdiction")
  saveDatasheet(mySce, 
                data.frame(Name = vars), "epi_Variable")
  
}

process_data_govcan <- function(df, lookup){
  
  df %>% 
    select(!contains(c("last", "percent", "rate", "today", 
                       "tests", "prob", "total"))) %>% 
    arrange(Timestep) %>%
    group_by(Jurisdiction) %>% 
    nest() %>% 
    mutate(data = list(purrr::map_df(data, ~rollback_govcan(.x)))) %>% 
    unnest(cols = c(data)) %>%  
    pivot_longer(cols = starts_with(c("num", "daily")), 
                 names_to = "Variable", values_to = "Value") %>% 
    left_join(lookup, by = c("Variable" = "RAWVARS")) %>% 
    select(-Variable) %>% rename(Variable = VARS) %>% 
    filter(!is.na(Variable))
  
}

rollback_govcan <- function(df){
  
  if(is.data.frame(df)){
    
    df <- df %>% 
      mutate(across(starts_with("num"), 
                    ~(.x - lag(.x, n = 1L, default = 0)), .names = "daily{.col}"))
    
  } else {
    
    stop("df should be of type data.frame")
    
  }
  
  return(df)
  
}

make_filename_govcan <- function(inputs){
  
  fileName <- paste0("COVID19_Data_GovCan_", inputs$ProvinceTerritory, ".csv")
  
}

save_output_govcan <- function(mySce, inputs, filePath){
  
  download_time <- as.character(Sys.time())
  
  output <- datasheet(mySce, "epiDataCanada_GovcanOutputs") %>% 
    add_row()
  
  output$Jurisdiction = inputs$ProvinceTerritory
  output$DataSourceID = "Government Of Canada"
  output$DownloadFile = filePath
  output$DownloadDateTime = download_time
  
  saveDatasheet(mySce, output, "epiDataCanada_GovcanOutputs")
}