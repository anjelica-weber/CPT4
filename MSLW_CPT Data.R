
# Libraries ---------------------------------------------------------------
library(tidyverse)
library(splitstackshape)
library(stringi)
library(xlsx)

# Constants ---------------------------------------------------------------
dir_files <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSLW Data/SLW/Charge Detail/Source Data'
dir_CDM <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/CDMs'
dir_dictionary <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSLW Data/SLW/Charge Detail/Dictionaries/MSLW_Revenue to Cost Center Map.xlsx'
dir_export <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data/MSLW Data/SLW/Charge Detail'

# Import Data -------------------------------------------------------------
list_filenames_RAWdata <- list.files(path = choose.dir(caption = "Select most recent folder", default = dir_files), full.names = T, pattern = "csv$") # only pulls in .csv files
if(length(list_filenames_RAWdata) != 3){stop("Unexpected number of files in selected folder")}#QC
CPT_data <- lapply(list_filenames_RAWdata, read.csv)

# Preprocess Data ---------------------------------------------------------
CPT_data <- lapply(CPT_data, function(x) cSplit(x, colnames(x), sep = '|', type.convert = T)) # separating csvs into tables
col_names_split <- function(df){
  new_col_names<- unlist(stri_split_fixed(str = colnames(df)[1], '.'))
  new_col_names<- sapply(new_col_names, function(x) stri_replace_all(str = x, regex = "_01", replacement = ""))
  colnames(df)<- new_col_names
  return(df)
}
CPT_data <- lapply(CPT_data, function(x) col_names_split(x))
CPT_data <- do.call(plyr::rbind.fill, CPT_data)
CPT_data <- CPT_data %>%
  mutate(PostingDate = as.Date(PostingDate),
         ServiceDate = as.Date(ServiceDate),
         FacilityId = as.character(FacilityId),
         RevenueCenter = as.character(RevenueCenter),
         ChargeCode = as.character(ChargeCode))

# Import Dictionaries -----------------------------------------------------
dictionary_CC <- read.xlsx2(file = dir_dictionary , sheetIndex = 1)
dictionary_CC <- dictionary_CC %>%
  mutate(Premier.Facility.ID = as.character(Premier.Facility.ID),
         FacilityId = as.character(FacilityId),
         RevenueCenter = as.character(RevenueCenter),
         Cost.Center = as.character(Cost.Center))
import_recent_CDM <- function(site.cdm) {
  #Compiling Data on Files
  Name <- list.files(path = dir_CDM, full.names = F, pattern ="csv$")
  Path <- list.files(path = dir_CDM, full.names = T, pattern ="csv$")
  Site <- sapply(Name, function(x) unlist(str_split(x, pattern = "_"))[1])
  Date <- sapply(Name, function(x) unlist(str_split(x, pattern = "_"))[4])
  Type <- sapply(Name, function(x) unlist(str_split(x, pattern = "_"))[4])
  #Formatting Data
  Date <- sapply(Date, function(x) substr(x,1, 9))
  Date <- as.Date(Date,"%d%B%Y")
  Type <- sapply(Type, function(x) substr(x,11, nchar(x)))
  #Creating Table of Data
  files <- data.table::data.table(Name, Path, Site, Date, Type)
  files <- files %>% arrange(desc(Date)) %>% filter(Site == site.cdm)
  #Selecting Most Recent File
  file_import <- files[1,]
  #Importing Data
  data_import <- read.table(file_import$Path, sep = ',',header = T, na.strings = c("", "Unavailable", "VOIDV"),fill = T)
  #Processing Data
  data_export <- data_import %>% 
    select(CHARGE_CODE,IPTB_cpt4,CHARGE_DESC) %>%
    rename(ChargeCode = CHARGE_CODE,
           CPTCode = IPTB_cpt4) %>%
    drop_na()
  return(data_export)
}
dictionary_CDM <- import_recent_CDM("SLR")
dictionary_CDM <- dictionary_CDM %>% 
  mutate(ChargeCode = as.character(ChargeCode),
         CPTCode = as.character(CPTCode))
#dictionary_CDM <- read.csv('SLR_OP_CDM_24AUG2020.csv', na.strings = c("", "Unavailable"))
#dictionary_CDM$CHARGE_DESC <- NULL
#colnames(dictionary_CDM)<- c("ChargeCode", "CPTCode")

# Formatting Data ---------------------------------------------------------
CPT_data_upload <- left_join(CPT_data, dictionary_CC)
CPT_data_upload <- left_join(CPT_data_upload, dictionary_CDM)

# Creating Upload File ----------------------------------------------------
file_upload <- function(cpt.data, site, start.date, end.date) {
  data_upload <- CPT_data_upload %>%
    filter(ServiceDate >= start.date,
           ServiceDate <= end.date,
           Premier.Facility.ID == site) %>%
    mutate(Corp = "729805",
           EndDate = ServiceDate,
           ServiceDate = format(ServiceDate, "%m/%d/%Y"),
           EndDate = format(EndDate, "%m/%d/%Y")) %>%
    select(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate, CPTCode, NumberOfUnits) %>%
    group_by(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate, CPTCode) %>%
    summarise(Volume = sum(NumberOfUnits, na.rm=T)) %>%
    mutate(Budget = 0) %>%
    drop_na()
}
MSW_upload <- file_upload(CPT_data_upload, 'NY2162', as.Date('2020-05-01'), as.Date('2020-07-31'))
MSM_upload <- file_upload(CPT_data_upload, 'NY2163', as.Date('2020-05-01'), as.Date('2020-07-31'))

# Exporting Files ---------------------------------------------------------
write.table(MSW_upload, file = paste0(dir_export, "/", 'MSW_CPT_1MAY20 to 31JUL20', '.csv'), sep = ',', row.names = F, col.names = F)
write.table(MSM_upload, file = paste0(dir_export, "/", 'MSM_CPT_1MAY20 to 31JUL20', '.csv'), sep = ',', row.names = F, col.names = F)
