# Loading Libraries -------------------------------------------------------


# Importing Master Data File ----------------------------------------------
if (exists('master_data_RAW')==F) {
  path_master_data_RAW <- choose.files(caption = "Select Master RAW Database CSV File", multi = F)
  master_data_RAW <- read.csv(path_master_data_RAW)
  #Processing Master Database
  master_data_RAW$PostingDate <- as.Date(master_data_RAW$PostingDate)
  master_data_RAW$ServiceDate <- as.Date(master_data_RAW$ServiceDate)
}#if the master data isn't already loaded in the environment import data

