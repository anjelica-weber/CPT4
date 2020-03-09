# Loading Libraries -------------------------------------------------------
library(xlsx)

# Importing Master Data File ----------------------------------------------
if (exists('master_data_RAW')==F) {
  path_master_data_RAW <- choose.files(caption = "Select Master RAW Database CSV File", multi = F)
  master_data_RAW <- read.csv(path_master_data_RAW)
  #Processing Master Database
  master_data_RAW$PostingDate <- as.Date(master_data_RAW$PostingDate)
  master_data_RAW$ServiceDate <- as.Date(master_data_RAW$ServiceDate)
}#if the master data isn't already loaded in the environment import data

# Creating Premier Upload File --------------------------------------------
generate_Premier_file <- function(df,start_date,end_date){
  start_date <- as.Date(start_date, tryFormats= c("%m/%d/%Y", '%m-%d-%Y', '%m/%d/%y', '%m-%d-%y'))
  end_date <- as.Date(end_date, tryFormats= c("%m/%d/%Y", '%m-%d-%Y', '%m/%d/%y', '%m-%d-%y'))
  #Subsetting Data
  premier_data <- df[((df$ServiceDate >= start_date) & (df$ServiceDate <= end_date)),] #subset of data by date range of service
  premier_data <- subset(premier_data, select = c("FacilityId", "RevenueCenter" ,"ChargeCode", "ServiceDate" ,"NumberOfUnits"))
  #Importing Dictionaries
    #Charge Code to CPT Code "Charge Description Master"
    cat("Select the most recent Charge Description Master", fill = T)
    path_dict_CDM <- file.choose(new = T)
    dict_CDM <- read.csv(file = path_dict_CDM, na.strings = c("", "Unavailable"))
    dict_CDM <- subset(dict_CDM, select = c("CHARGE_CODE", "IPTB_cpt4"))
    dict_CDM <- na.omit(dict_CDM)
    colnames(dict_CDM)<- c("ChargeCode", "CPTCode")
    #Revenue to Cost Center 
    cat("Select the Revenue to Cost Center Mapping file", fill = T)
    path_dict_CC<- file.choose(new = T)
    dict_CC<- read.xlsx(path_dict_CC, sheetIndex = 1)
  #Merging dictionaries and data
    premier_data <- merge.data.frame(premier_data, dict_CC, all.x = T, all.y = F)
    premier_data <- merge.data.frame(premier_data, dict_CDM, all.x = T, all.y = F)
  return(premier_data)
} 

