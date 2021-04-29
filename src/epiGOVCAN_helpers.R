
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
    filter(prname != "Repatriated travellers")
  
  if(inputs$IncludeCanada == "No"){
    
    raw_data <- raw_data %>% 
      filter(prname != "Canada")
    
    if (inputs$ProvinceTerritory == "None"){
      
      stop("No province requested and Canada not included, no data to return.")
      
    } else if(inputs$ProvinceTerritory == "All"){
      
      return(raw_data)
      
    }  else {
      
      raw_data <- raw_data %>% 
        filter(prname == inputs$ProvinceTerritory)
      
    }
    
  }
  
}
