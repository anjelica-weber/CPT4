# Loading Libraries -------------------------------------------------------
library(splitstackshape)
library(stringi)
library(dplyr)

# Importing RAW Data --------------------------------------------
folder_RAWdata <- choose.dir(caption = "Select folder with RAW CPT Data")
list_filenames_RAWdata <- list.files(path = folder_RAWdata, full.names = T, pattern = "csv$") # only pulls in .csv files
  # Quality Check - correct number of files loaded
  if(length(list_filenames_RAWdata) != 3){stop("Unexpected number of files in selected folder")} #test under different conditions, if wrong folder selected give option to select again
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
path_master_data_RAW <- choose.files(caption = "Select Master RAW Database CSV File", multi = F)
master_data_RAW <- read.csv(path_master_data_RAW)
#Processing Master Database
master_data_RAW$PostingDate <- as.Date(master_data_RAW$PostingDate)
master_data_RAW$ServiceDate <- as.Date(master_data_RAW$ServiceDate)

# Quality Check -----------------------------------------------------------
range_new_data <- range(CPT_data_RAW$PostingDate)
range_master_data <- range(master_data_RAW$PostingDate)
if(range_new_data[1]< range_master_data[2]){stop("The Posting Date range of the new data has already been added to the Master RAW Database")} # test under different conditions
#generate_log_uploaded<- function(){
 # user <- Sys.getenv(if(.Platform$OS.type == "windows")"USERNAME" else "USER")
  #uploaded <- Sys.Date()
  #start_date_posting <- range_new_data[1]
  #end_date_posting <- range_new_data[2]
  #file_names <- list.files(path = folder_RAWdata, full.names = T, pattern = "csv$")[1]
#}

# Quality Check - Volume Validation ---------------------------------------
  #Importing Dictionaries
    #Charge Description Master
    cat("Select the most recent Charge Description Master", fill = T)
    path_dict_CDM <- file.choose(new = T)
    dict_CDM <- read.csv(file = path_dict_CDM, na.strings = c("", "Unavailable"))
    dict_CDM <- subset(dict_CDM, select = c("CHARGE_CODE", "IPTB_cpt4"))
    colnames(dict_CDM)<- c("ChargeCode", "CPTCode")
    #Revenue to Cost Center
    cat("Select the Revenue to Cost Center Mapping file", fill = T)
    path_dict_CC <- file.choose(new = T)
    dict_CC <- read.xlsx(path_dict_CC, sheetIndex = 1)
  #Preprocessing New Data
    #Adding CC and CPT codes
      CPT_data_analysis <- merge.data.frame(CPT_data_RAW, dict_CC, all.x = T, all.y = F)
      CPT_data_analysis <- merge.data.frame(CPT_data_analysis, dict_CDM, all.x = T, all.y = F)
    #Getting sum of charges per day per cost center
      CPT_data_analysis_sum <- aggregate(CPT_data_analysis$NumberOfUnits, by= list(CPT_data_analysis$Premier.Facility.ID, CPT_data_analysis$Cost.Center, CPT_data_analysis$PostingDate), FUN= 'sum')
      colnames(CPT_data_analysis_sum)  <- c('Facility ID', 'Cost Center', 'ServiceDate', 'Sum of Charges') 
  #Preprocessing Master Data
      master_data_analysis <- merge.data.frame(master_data_RAW, dict_CC, all.x = T, all.y = F)
      master_data_analysis <- merge.data.frame(master_data_analysis, dict_CDM, all.x = T, all.y = F)
      #Getting sum of charges per day per cost center
      master_data_analysis_sum <- aggregate(master_data_analysis$NumberOfUnits, by= list(master_data_analysis$Premier.Facility.ID, master_data_analysis$Cost.Center, master_data_analysis$PostingDate), FUN= 'sum')
      colnames(master_data_analysis_sum)  <- c('Facility ID', 'Cost Center', 'ServiceDate', 'Sum of Charges') 
  #Analysis of Data
      #mean_master <- c(unique(master_data_analysis_sum$`Cost Center`), mean(master_data_analysis_sum[master_data_analysis_sum$`Cost Center`=='1109019368', 'Sum of Charges']))
      #mean_new
# Appending New Data to Master --------------------------------------------
master_data_RAW<- merge.data.frame(master_data_RAW, CPT_data_RAW, all=T)

# Exporting Master Data File ---------------------------------------------------
filename_master <- paste0("MasterData_", format(range_master_data[1], '%d%b%y'), ' to ', format(range_new_data[2], '%d%b%y'), '.csv')
write.table(master_data_RAW, file = filename_master, row.names = F, col.names = T, sep = ',')
