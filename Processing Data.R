
# Loading Libraries -------------------------------------------------------
library(stringi)


# Importing RAW Data --------------------------------------------
folder_RAWdata <- choose.dir(caption = "Select folder with RAW CPT Data")
list_filenames_RAWdata<- list.files(path = folder_RAWdata, full.names = T, pattern = "csv$")
CPT_data_list <- lapply(list_filenames_RAWdata, read.csv)
  # Quality Check - correct number of files loaded
  stopifnot(length(CPT_data_list) == 3) # run code again, make sure the correct folder is selected
  
