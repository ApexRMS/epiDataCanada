## DownloadCanada transformer R script

rm(list=ls())

# Load libraries ----------------------------------------------------------

library(rsyncrosim)
library(data.table)
library(jsonlite)
library(stringr)
library(dplyr)

# Variables - environment -------------------------------------------------

env <- ssimEnvironment()
myScenario <- scenario()

vars_query <- c("cases", "mortality", "recovered", "testing", 
                "active", "avaccine", "dvaccine", "cvaccine")

# Source helpers ----------------------------------------------------------

source(file.path(env$PackageDirectory, "epiCD_helpers.R"))

# Load inputs -------------------------------------------------------------

choices <- datasheet(myScenario, "epiDataCanada_Inputs")

downTable <- data.table()

if(choices$Province == "All"){ # all provinces
    
    if(choices$Regions){ # all the health regions in all provinces
        
        downTable <- mapply(FUN = get_data, 
                            stat = vars_query,
                            loc = "hr", 
                            clean = "hr", 
                            SIMPLIFY = FALSE) %>% bind_rows()
        
    } else { # all provinces
        
        downTable <-  mapply(FUN = get_data, 
                             stat = vars_query,
                             loc = "prov", 
                             clean = "prov", 
                             SIMPLIFY = FALSE) %>% bind_rows()
        
    }
    
} else { # the user requested a specific province
    
    if(choices$Regions) { # All health region in a given province
        
        if (choices$Province != "None"){
            
            codes <- jurisDictionary[[as.character(choices$Province)]]$regions
            
            downTable <-  mapply(FUN = get_data, 
                                 stat = vars_query, 
                                 loc = codes, 
                                 clean = "hr", 
                                 SIMPLIFY = FALSE) %>% bind_rows()
            
        }
        
    } else { # Just the province
        
        if (choices$Province != "None"){
            
            code <- jurisDictionary[[as.character(choices$Province)]]$code
            
            downTable <- mapply(FUN = get_data, 
                                stat = vars_query,
                                loc = code, 
                                clean = "prov", 
                                SIMPLIFY = FALSE) %>% bind_rows()
            
        }
        
    }
}

if (choices$IncludeCanada){
    
    canadaData <- mapply(FUN = get_data, 
                         stat = vars_query,
                         loc = "canada", 
                         clean = "canada", 
                         SIMPLIFY = FALSE) %>% bind_rows()
    
    if(nrow(downTable) > 0){
        downTable <- bind_rows(downTable, canadaData)
    } else {
        downTable <- canadaData
    }
    
}


# Write out files ---------------------------------------------------------

caseName <- make_file_name(stat="cases", choices)

fwrite(
    downTable %>% subset(grepl("Cases", Variable)),
    file = caseName
)

mortalityName <- make_file_name(stat="mortality", choices)

fwrite(
    downTable %>% subset(grepl("Mortality", Variable)),
    file = mortalityName
)

# Set up output tables ----------------------------------------------------

outputTable <- datasheet(myScenario, "epiDataCanada_Outputs")
outputTable[1,] <- NA
outputTable$DownloadDate <- Sys.time()

if(is.na(choices$Province)) {
    
    outputTable$CanadaCaseCSV <- caseName
    outputTable$CanadaMortalityCSV <- mortalityName
    
} else {
    
    outputTable$ProvinceCaseCSV <- caseName
    outputTable$ProvinceMortalityCSV <- mortalityName
}

if(choices$Regions) {
    
    outputTable$HealthCaseCSV <- caseName
    outputTable$HealthMortalityCSV <- mortalityName
    
}

saveDatasheet(myScenario, outputTable, "epiDataCanada_Outputs")

# Variables ---------------------------------------------------------------

epiVariable <- datasheet(myScenario, "epi_Variable")
tempVar <- datasheet(myScenario, "epi_Variable", empty=TRUE)

for(var in unique(downTable$Variable)){ 
    
    if(!(var %in% epiVariable$Name)) {
        
        tempVar <- rbind(
            tempVar,
            data.table("Name"=var, "Description"=""))
        
    }
}

if(nrow(tempVar) != 0) saveDatasheet(myScenario, tempVar, "epi_Variable")

# Jurisdictions -----------------------------------------------------------

epiJurisdiction <- datasheet(myScenario, "epi_Jurisdiction")
tempJuris <- datasheet(myScenario, "epi_Jurisdiction", empty=TRUE)

for(juris in unique(downTable$Jurisdiction)){ 
    
    if(!(juris %in% epiJurisdiction$Name)){
        tempJuris <- rbind(
            tempJuris,
            data.table("Name"=juris, "Description"=""))
    }
}

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
