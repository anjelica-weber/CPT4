
# Loading Libraries -------------------------------------------------------
library(splitstackshape)
library(stringi)
library(dplyr)

# Importing RAW Data --------------------------------------------
folder_RAWdata <- choose.dir(caption = "Select folder with RAW CPT Data")
list_filenames_RAWdata <- list.files(path = folder_RAWdata, full.names = T, pattern = "csv$") # only pulls in .csv files
CPT_data_list <- lapply(list_filenames_RAWdata, read.csv)
  # Quality Check - correct number of files loaded
  stopifnot(length(CPT_data_list) == 3) # run code again, make sure the correct folder is selected
  

# Pre Processing Data -----------------------------------------------------
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
#levels(CPT_data_RAW$PostingDate)<- as.POSIXct(levels(CPT_data_RAW$PostingDate)) # formats the levels to remove the seconds
#levels(CPT_data_RAW$ServiceDate)<- as.POSIXct(levels(CPT_data_RAW$ServiceDate))


# Importing Master Data Base --------------------------------------------
path_master_data_RAW <- choose.files(caption = "Select Master RAW Data Base File", multi = F)
master_data_RAW<- read.csv(path_master_data_RAW)
