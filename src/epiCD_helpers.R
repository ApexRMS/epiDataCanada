
# Helpers -----------------------------------------------------------------

toTitle <- stringr::str_to_title

uniQuify <- function(x){ 
  
  paste(unique(unlist(strsplit(x, " - "))), collapse=" - ")
  
}

# remove the word Zone, extraneous, parentheses, digits
cleanName <- function(x){ 
  
  x %>% 
    gsub("\\(|\\)|Zone|[0-9]", "", .) %>% 
    gsub("-", " ", .)
  
}

replaceAccents <- function(string){
  
  accentedCharacters <- list("�"="e", "�"="e", "�"="o", "�"="I")
  
  for(index in 1:length(accentedCharacters))
  {
    string <- gsub(
      names(accentedCharacters)[index], 
      accentedCharacters[[1]],
      string
    )
  }
  
  return(string)
  
}

# Query functions ---------------------------------------------------------

timeseries_url <- function(base = "https://api.opencovid.ca/timeseries?", 
                           stat, loc){
  
  url <- paste0(base, "stat=", stat, "&loc=", loc)
  
}

get_dataset <- function(url, stat){
  
  ret <- jsonlite::fromJSON(url)[[stat]]
  
}

get_data <- function(stat, loc, clean){

  the_url <- timeseries_url(stat = stat, loc = loc)
  
  data_raw <- tryCatch({
    get_dataset(the_url, stat = stat) },
    error = function(cond) {
      warning(paste0("The URL ", the_url, " failed."))
      return(NULL)
    }, 
    warning = function(cond){
      warning(paste0("The URL ", the_url, " failed."))
      return(NULL)
    }
  )
  
  data_raw <- data_raw %>% 
    filter(province != "Repatriated")
  
  if (is.null(data_raw)){
    
    return(NULL)
    
  }
  
  if(clean != "canada"){
    
    data_raw <- data_raw %>% 
      left_join(PROVINCE_LOOKUP, by = "province") %>% 
      mutate(full_name = ifelse(is.na(full_name), province, full_name)) %>% 
      select(-province) %>% 
      rename(province = full_name) 
    
  }
  
  # Clean jurisdiction based on location
  if(clean == "hr"){
    
    if ("health_region" %in% colnames(data_raw)) {
    
    data_raw <- data_raw %>% 
      mutate(health_region = 
               health_region %>% 
               cleanName() %>% 
               replaceAccents()) %>%
      mutate(Jurisdiction = sprintf("Canada - %s - %s", province, health_region)) %>%
      select(-c(province, health_region))
    
    } else {
      
      warning(paste0("The stat ", stat, " is not available at the HR level"))
      return(NULL)
      
    }
    
  } else if (clean == "prov") {
    
    data_raw <- data_raw %>% 
      mutate(Jurisdiction = sprintf("Canada - %s", province)) %>%
      select(-c(province))
    
  } else if(clean == "canada") {
    
    data_raw <- data_raw %>% 
      rename(Jurisdiction = province)
    
  }
  
  # Clean column names appropriately based on the stat required
  data_cleaned <- rename_columns(data_raw, stat)
  
  data_cleaned <- data_cleaned %>%
    data.table() %>%
    melt.data.table(., id.vars=c("Timestep", "Jurisdiction")) %>% 
    rename("Variable"="variable", "Value"="value") %>%
    mutate(Timestep=as.IDate(Timestep, format="%d-%m-%Y"))
  
  return(data_cleaned)
  
}

rename_columns <- function(data_raw, stat){
  
  if(stat == "cases"){
    
    data_cleaned <- data_raw %>%
      rename(
        "Timestep" = "date_report",
        "Cases - Daily" = "cases",
        "Cases - Cumulative" = "cumulative_cases")
    
  } else if (stat == "mortality"){
    
    data_cleaned <- data_raw %>% 
      rename(
        "Timestep" = "date_death_report",
        "Deaths - Daily" = "deaths",
        "Deaths - Cumulative" = "cumulative_deaths") 
    
  } else if (stat == "recovered"){
    
    data_cleaned <- data_raw %>% 
      rename(
        "Timestep" = "date_recovered",
        "Recovered - Daily" = "recovered",
        "Recovered - Cumulative" = "cumulative_recovered") 
    
  } else if (stat == "testing"){
    
    data_cleaned <- data_raw %>% 
      select(-testing_info) %>% 
      rename(
        "Timestep" = "date_testing",
        "Tested - Daily" = "testing",
        "Tested - Cumulative" = "cumulative_testing") 
    
  } else if (stat == "active"){
    
    data_cleaned <- data_raw %>% 
      select(!contains(c("cumulative", "change"))) %>% 
      rename(
        "Timestep" = "date_active",
        "Active - Daily" = "active_cases") 
    
  } else if (stat == "avaccine"){
    
    data_cleaned <- data_raw %>% 
      rename(
        "Timestep" = "date_vaccine_administered",
        "Vaccines (Administered) - Daily" = "avaccine",
        "Vaccines (Administered) - Cumulative" = "cumulative_avaccine") 
    
  } else if (stat == "dvaccine"){
    
    data_cleaned <- data_raw %>% 
      rename(
        "Timestep" = "date_vaccine_distributed",
        "Vaccines (Distributed) - Daily" = "dvaccine",
        "Vaccines (Distributed) - Cumulative" = "cumulative_dvaccine") 
    
  } else if (stat == "cvaccine"){
    
    data_cleaned <- data_raw %>% 
      rename(
        "Timestep" = "date_vaccine_completed",
        "Vaccines (Completed) - Daily" = "cvaccine",
        "Vaccines (Completed) - Cumulative" = "cumulative_cvaccine") 
    
  }
  
  return(data_cleaned)
  
}

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

# Key Variables -----------------------------------------------------------

jurisDictionary <- list(
  
  "Alberta" = list("code" = "AB", 
                   "regions" =  c(4832, 4833, 4834, 4835, 4831)),
  
  "British Columbia" = list("code" = "BC", 
                            "regions" = c(591, 592, 593, 594, 595)),
  
  "Manitoba" = list("code" = "MB", 
                    "regions" = c(4603, 4604, 4602, 4605, 4601)),
  
  "New Brunswick" = list("code" = "NB", 
                         regions = c(1301, 1302, 1303, 1304, 1305, 1306, 1307)),
  
  "Newfoundland and Labrador" = list("code" = "NL", 
                                     "regions" = c(1012, 1011, 1014,	1013)),
  
  "Northwest Territories" = list("code" = "NT", 
                                 "regions" = c(6101)),
  
  "Nova Scotia" = list("code" = "NS", 
                       "regions" = c(1201, 1202, 1203, 1204)),
  
  "Nunavut" = list("code" = "NU", 
                   "regions" = c(6201)),
  
  "Ontario" = list("code" = "ON", 
                   "regions" = c(3526, 3527, 3540, 3530, 3558, 3533, 3534, 3535, 3536, 
                                 3537, 3538, 3539, 3541, 3542, 3543, 3544, 3546, 3547,
                                 3549, 3551, 3553, 3555, 3556, 3557, 3560, 3575, 3561, 
                                 3562, 3563, 3595, 3565, 3566, 3568, 3570
                   )),
  
  "Prince Edward Island" = list("code" = "PE", 
                                "regions" = c(1100)),
  
  "Quebec" = list("code" = "QC", 
                  "regions" = c(2408, 2401, 2403, 2412, 2409, 2405, 2411, 2414, 2415, 2413, 2404, 2416, 2406, 2410, 2417, 2407, 2402, 2418)),
  
  "Saskatchewan" = list("code" = "SK", 
                        "regions" = c(473, 471, 472, 475, 474, 476)),
  
  "Yukon" = list("code" = "YT", 
                 "regions" = c(6001))
)

code_list <- lapply(jurisDictionary, `[[`, 1)
PROVINCE_LOOKUP <- data.frame(province = unlist(code_list), 
                              full_name = names(code_list))