rm(list=ls())

library(rsyncrosim)
library(data.table)
library(jsonlite)
library(stringr)
library(dplyr)

env <- ssimEnvironment()
myScenario <- scenario()

desiredSeries = c("timeseries")
desiredStats <- c("cases", "mortailty")

toTitle <- stringr::str_to_title
uniQuify <- function(x){ return( paste(unique(unlist(strsplit(x, " - "))), collapse=" - ") ) }
# remove the word Zone, extraneous, parentheses, digits
cleanName <- function(x){ return(x %>% gsub("\\(|\\)|Zone|[0-9]", "", .) %>% gsub("-", " ", .)) }

# accentedCharacters <- list("é"="&#xE9;", "è"="&#xE8;", "ô"="&#xF4;", "Î"="&#xCE;")
accentedCharacters <- list("é"="e", "è"="e", "ô"="o", "Î"="I")
replaceAccents <- function(string)
{
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

jurisDictionary <- list(
    "Alberta" = list("code" = "AB", "regions" =  c(4832, 4833, 4834, 4835, 4831)),
    "British Columbia" = list("code" = "BC", "regions" = c(591, 592, 593, 594, 595)),
    "Manitoba" = list("code" = "MB", "regions" = c(4603, 4604, 4602, 4605, 4601)),
    "New Brunswick" = list("code" = "NB", regions = c(1301, 130, 1303, 1304, 1305, 1306, 1307)),
    "Newfoundland and Labrador" = list("code" = "NL", "regions" = c(1012, 1011, 1014,	1013)),
    "Northwest Territories" = list("code" = "NT", "regions" = c(6101)),
    "Nova Scotia" = list("code" = "NS", "regions" = c(1201, 1202, 1203, 1204)),
    "Nunavut" = list("code" = "NU", "regions" = c(6201)),
    "Ontario" = list("code" = "ON", "regions" = c(3526, 3527, 3540, 3530, 3558, 3533, 3534, 3535, 3536, 
        3537, 3538, 3539, 3541, 3542, 3543, 3544, 3546, 3547,
        3549, 3551, 3553, 3555, 3556, 3557, 3560, 3575, 3561, 
        3562, 3563, 3595, 3565, 3566, 3568, 3570
    )),
    "Prince Edward Island" = list("code" = "PE", "regions" = c(1100)),
    "Quebec" = list("code" = "QC", "regions" = c(2408, 2401, 2403, 2412, 2409, 2405, 2411, 2414, 2415, 2413, 2404, 2416, 2406, 2410, 2417, 2407, 2402, 2418)),
    "Saskatchewan" = list("code" = "SK", "regions" = c(473, 471, 472, 475, 474, 476)),
    "Yukon" = list("code" = "YT", "regions" = c(6001))
)



choices <- datasheet(myScenario, "epiDataCanada_Inputs")

downTable <- data.table()

if(is.na(choices$Province)) # all of Canada
{
    if(choices$Regions) # all the health regions in Canada
    {
        downTable <- rbind(
            jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=hr") %>%
                .$cases %>%                
                mutate(health_region = health_region %>% cleanName() %>% replaceAccents()) %>%
                mutate(Jurisdiction = sprintf("Canada - %s - %s", province, health_region)) %>%
                select(-c(province, health_region)) %>%
                rename(
                    "Timestep" = "date_report",
                    "Cases - Daily" = "cases",
                    "Cases - Cumulative" = "cumulative_cases"
                ) %>%
                data.table() %>%
                melt.data.table(., id.vars=c("Timestep", "Jurisdiction")),
            
            jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=mortality&loc=hr") %>%
                .$mortality %>%
                mutate(health_region = health_region %>% cleanName() %>% replaceAccents()) %>%
                mutate(Jurisdiction = sprintf("Canada - %s - %s", province, health_region)) %>%
                select(-c(province, health_region)) %>%
                rename(
                    "Timestep" = "date_death_report",
                    "Mortality - Daily" = "deaths",
                    "Mortality - Cumulative" = "cumulative_deaths"
                ) %>%
                data.table() %>%
                melt.data.table(., id.vars=c("Timestep", "Jurisdiction"))
        )
        
    } else { # just an overview of the country
        
        downTable <- rbind(
            jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=cases&loc=canada") %>%
                .$cases %>%
                rename(
                    "Timestep" = "date_report",
                    "Jurisdiction" = "province",
                    "Cases - Daily" = "cases",
                    "Cases - Cumulative" = "cumulative_cases"
                ) %>%
                data.table() %>%
                melt.data.table(., id.vars=c("Timestep", "Jurisdiction")),
            
            jsonlite::fromJSON("https://api.opencovid.ca/timeseries?stat=mortality&loc=canada") %>%
                .$mortality %>%
                rename(
                    "Timestep" = "date_death_report",
                    "Jurisdiction" = "province",
                    "Mortality - Daily" = "deaths",
                    "Mortality - Cumulative" = "cumulative_deaths"
                ) %>%
                data.table() %>%
                melt.data.table(., id.vars=c("Timestep", "Jurisdiction"))
        )
    }
    
} else { # the user requested a specific province
    
    if(choices$Regions) # give information for all the health regions
    {
        for(code in jurisDictionary[[choices$Province]]$regions)
        {
            downTable <- rbind(
                downTable,
                jsonlite::fromJSON(paste0("https://api.opencovid.ca/timeseries?stat=cases&loc=", code)) %>%
                    .$cases %>%
                    rename(
                        "Timestep" = "date_report",
                        "Cases - Daily" = "cases",
                        "Cases - Cumulative" = "cumulative_cases"
                    ) %>%
                    data.table() %>%
                    mutate(health_region = health_region %>% cleanName() %>% replaceAccents()) %>%
                    mutate(Jurisdiction = sprintf("Canada - %s - %s", choices$Province, health_region)) %>%
                    select(-c(province, health_region)) %>%
                    melt.data.table(., id.vars=c("Timestep", "Jurisdiction")),
                
                jsonlite::fromJSON(paste0("https://api.opencovid.ca/timeseries?stat=mortality&loc=", code)) %>%
                    .$mortality %>%
                    rename(
                        "Timestep" = "date_death_report",
                        "Mortality - Daily" = "deaths",
                        "Mortality - Cumulative" = "cumulative_deaths"
                    ) %>%
                    data.table() %>%
                    mutate(health_region = health_region %>% cleanName() %>% replaceAccents()) %>%
                    mutate(Jurisdiction = sprintf("Canada - %s - %s", choices$Province, health_region)) %>%
                    select(-c(province, health_region)) %>%
                    melt.data.table(., id.vars=c("Timestep", "Jurisdiction"))
            )
        }
       
    } else { # just the province
        
        downTable <- rbind(
            jsonlite::fromJSON(paste0("https://api.opencovid.ca/timeseries?stat=cases&loc=", jurisDictionary[[choices$Province]]$code)) %>%
                .$cases %>%
                rename(
                    "Timestep" = "date_report",
                    "Cases - Daily" = "cases",
                    "Cases - Cumulative" = "cumulative_cases"
                ) %>%
                data.table() %>%
                mutate(Jurisdiction = sprintf("Canada - %s", choices$Province)) %>%
                select(-c(province)) %>%
                melt.data.table(., id.vars=c("Timestep", "Jurisdiction")),
            
            jsonlite::fromJSON(paste0("https://api.opencovid.ca/timeseries?stat=mortality&loc=", jurisDictionary[[choices$Province]]$code)) %>%
                .$mortality %>%
                rename(
                    "Timestep" = "date_death_report",
                    "Mortality - Daily" = "deaths",
                    "Mortality - Cumulative" = "cumulative_deaths"
                ) %>%
                data.table() %>%
                mutate(Jurisdiction = sprintf("Canada - %s", choices$Province)) %>%
                select(-c(province)) %>%
                melt.data.table(., id.vars=c("Timestep", "Jurisdiction"))
        )
    }
}

downTable <- downTable %>%
    rename("Variable"="variable", "Value"="value") %>%
    mutate(Timestep=as.IDate(Timestep, format="%d-%m-%Y"))

caseName <- paste(
    env$TransferDirectory,
    paste(
        "epiDataCanada", 
        if(is.na(choices$Province)) "Canada" else choices$Province, 
        if(choices$Regions) "health_regions", 
        "cases.csv", 
        sep = "_"
    ), sep = "\\"
)

fwrite(
    downTable %>% subset(grepl("Cases", Variable)),
    file = caseName
)

mortalityName <- paste(
    env$TransferDirectory,
    paste(
        "epiDataCanada", 
        if(is.na(choices$Province)) "Canada" else choices$Province, 
        if(choices$Regions) "health_regions", 
        "deaths.csv", 
        sep = "_"
    ), sep = "\\"
)

fwrite(
    downTable %>% subset(grepl("Mortality", Variable)),
    file = mortalityName
)

outputTable <- datasheet(myScenario, "epiDataCanada_Outputs")
outputTable[1,] <- NA
outputTable$DownloadDate <- Sys.time()
if(is.na(choices$Province))
{
    outputTable$CanadaCaseCSV <- caseName
    outputTable$CanadaMortalityCSV <- mortalityName

} else {

    outputTable$ProvinceCaseCSV <- caseName
    outputTable$ProvinceMortalityCSV <- mortalityName
}
if(choices$Regions)
{
    outputTable$HealthCaseCSV <- caseName
    outputTable$HealthMortalityCSV <- mortalityName
}
saveDatasheet(myScenario, outputTable, "epiDataCanada_Outputs")

epiVariable <- datasheet(myScenario, "epi_Variable")
tempVar <- datasheet(myScenario, "epi_Variable", empty=TRUE)
for(var in unique(downTable$Variable)){ if(!(var %in% epiVariable$Name))
{
    tempVar <- rbind(
        tempVar,
        data.table("Name"=var, "Description"="")
    )
}}
if(nrow(tempVar) != 0) saveDatasheet(myScenario, tempVar, "epi_Variable")

epiJurisdiction <- datasheet(myScenario, "epi_Jurisdiction")
tempJuris <- datasheet(myScenario, "epi_Jurisdiction", empty=TRUE)
for(juris in unique(downTable$Jurisdiction)){ if(!(juris %in% epiJurisdiction$Name))
{
    tempJuris <- rbind(
        tempJuris,
        data.table("Name"=juris, "Description"="")
    )
}}
if(nrow(tempJuris) != 0) saveDatasheet(myScenario, tempJuris, "epi_Jurisdiction")

epiDataSummary <- datasheet(myScenario, "epi_DataSummary")
epiDataSummary[nrow(downTable), ] <- NA
epiDataSummary$TransformerID <- "Canadian COVID-19 Data: Download from University of Toronto"
epiDataSummary$Timestep <- downTable$Timestep
epiDataSummary$Variable <- downTable$Variable
epiDataSummary$Jurisdiction <- downTable$Jurisdiction
epiDataSummary$AgeMin <- NULL
epiDataSummary$AgeMax <- NULL
epiDataSummary$Value <- downTable$Value
saveDatasheet(myScenario, epiDataSummary, "epi_DataSummary")
