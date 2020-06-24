# Load Libraries ----------------------------------------------------------
library(xlsx)
library(tidyverse)
library(splitstackshape)
library(stringi)

# Importing RAW Data ------------------------------------------------------
#Need to create new folder structure in share drive, CPT Data folder with sub folders by month (with all site data in folder)
#dir_data <- 'J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity/Volume - Data'
dir_data <- 'J:/deans/Presidents/SixSigma/Individual Folders/Current Employees/Engineers/Anjelica Weber/Projects/2020/Charge Detail R Coding/Mock Data Folder Structure'
list_filenames_RAWdata <- list.files(path = dir_data, full.names = T, pattern = "csv$")
#QC
if ((length(list_filenames_RAWdata)/3)%%1 != 0){stop("Files missing")} #if number of files not a multiple of 3 files are missing
#
data_raw <- lapply(list_filenames_RAWdata, read.csv)



# Preprocessing Data ------------------------------------------------------
data_CPT <- lapply(data_raw, function(x) cSplit(x, colnames(x), sep = '|', type.convert = T)) # separating csv into table
col_names_split <- function(df){
  new_col_names<- unlist(stri_split_fixed(str = colnames(df)[1], '.'))
  new_col_names<- sapply(new_col_names, function(x) stri_replace_all(str = x, regex = "_01", replacement = ""))
  colnames(df)<- new_col_names
  return(df)
}
data_CPT <- lapply(data_CPT, function(x) col_names_split(x)) # defining column names
#data_CPT <- do.call(full_join, data_CPT)
