# Loading Libraries -------------------------------------------------------
library(splitstackshape)
library(stringi)
library(dplyr)

# Importing RAW Data --------------------------------------------
folder_RAWdata <- choose.dir(caption = "Select folder with RAW CPT Data")
list_filenames_RAWdata <- list.files(path = folder_RAWdata, full.names = T, pattern = "csv$") # only pulls in .csv files
  # Quality Check - correct number of files loaded
  if(length(CPT_data_list) != 3){stop("Unexpected number of files in selected folder")} #test under different conditions, if wrong folder selected give option to select again
CPT_data_list <- lapply(list_filenames_RAWdata, read.csv)
  
# Pre Processing Data -----------------------------------------------------
# Converting csv files into 1 table
CPT_data_list <- lapply(CPT_data_list, function(x) cSplit(x, colnames(x), sep = '|', type.convert = T)) # separating csv into table
  col_names_split <- function(df){
    new_col_names<- unlist(stri_split_fixed(str = colnames(df)[1], '.'))
    new_col_names<- sapply(new_col_names, function(x) stri_replace_all(str = x, regex = "_01", replacement = ""))
    colnames(df)<- new_col_names
    return(df)
  }
CPT_data_list <- lapply(CPT_data_list, function(x) col_names_split(x)) # defining column names
  merge_multiple_dataframes<- function(list.dfs){
  output<- list.dfs[1]
  for (i in 2:length(list.dfs)){
    output <- merge.data.frame(output, list.dfs[i], all.x = T, all.y = T)
  }
  return(output)
}
CPT_data_RAW <- merge_multiple_dataframes(CPT_data_list)
#Formatting Date Columns
CPT_data_RAW$PostingDate<- as.Date(CPT_data_RAW$PostingDate)
CPT_data_RAW$ServiceDate<- as.Date(CPT_data_RAW$ServiceDate)
#Sorting Data
CPT_data_RAW<- arrange(CPT_data_RAW,`PostingDate`,`FacilityId` ,`RevenueCenter` , `ChargeCode`)

# Importing Master Database --------------------------------------------
path_master_data_RAW <- choose.files(caption = "Select Master RAW Database File", multi = F)
master_data_RAW <- read.csv(path_master_data_RAW)
#Processing Master Database
master_data_RAW$PostingDate <- as.Date(master_data_RAW$PostingDate)
master_data_RAW$ServiceDate <- as.Date(master_data_RAW$ServiceDate)

# Quality Check -----------------------------------------------------------
range_new_data <- range(CPT_data_RAW$PostingDate)
range_master_data <- range(master_data_RAW$PostingDate)
if(range_new_data[1]< range_master_data[2]){stop("Date range of the new data has already been added to the Master RAW Database")} # test under different conditions
#generate_log_uploaded<- function(){
 # user <- Sys.getenv(if(.Platform$OS.type == "windows")"USERNAME" else "USER")
  #uploaded <- Sys.Date()
  #start_date <- range_new_data[1]
  #end_date <- range_new_data[2]
  #file_names <- list.files(path = folder_RAWdata, full.names = T, pattern = "csv$")[1]
#}
# Appending New Data to Master --------------------------------------------


