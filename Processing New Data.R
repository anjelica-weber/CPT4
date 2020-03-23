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
    path_dict_CDM <- choose.files(caption= "Select the most recent Charge Description Master",multi = F)
    dict_CDM <- read.csv(file = path_dict_CDM, na.strings = c("", "Unavailable"))
    dict_CDM <- subset(dict_CDM, select = c("CHARGE_CODE", "IPTB_cpt4"))
    colnames(dict_CDM)<- c("ChargeCode", "CPTCode")
    #Revenue to Cost Center
    path_dict_CC <- choose.files(caption = 'Select the Revenue to Cost Center Mapping file', multi = F)
    dict_CC <- read.xlsx(path_dict_CC, sheetIndex = 1)
  #Preprocessing New Data
    #Adding CC and CPT codes
      CPT_data_analysis <- merge.data.frame(CPT_data_RAW, dict_CC, all.x = T, all.y = F)
      CPT_data_analysis <- merge.data.frame(CPT_data_analysis, dict_CDM, all.x = T, all.y = F)
    #Getting sum of charges per month per cost center
      CPT_data_analysis_sum <- CPT_data_analysis[((CPT_data_analysis$ServiceDate >= range_new_data[1]) & (CPT_data_analysis$ServiceDate <= range_new_data[2])),]
      CPT_data_analysis_sum <- aggregate(CPT_data_analysis_sum$NumberOfUnits, by= list(CPT_data_analysis_sum$Premier.Facility.ID, CPT_data_analysis_sum$Cost.Center, format(CPT_data_analysis_sum$ServiceDate, "%m")), FUN= 'sum')
      colnames(CPT_data_analysis_sum)  <- c('Facility ID', 'Cost Center', 'Service Month', 'Sum of Charges') 
  #Preprocessing Master Data
      master_data_analysis <- merge.data.frame(master_data_RAW, dict_CC, all.x = T, all.y = F)
      master_data_analysis <- merge.data.frame(master_data_analysis, dict_CDM, all.x = T, all.y = F)
      #Getting sum of charges per month per cost center
      master_data_analysis_sum <- master_data_analysis[((master_data_analysis$ServiceDate >= range_master_data[1]) & (master_data_analysis$ServiceDate <= range_master_data[2])),] 
      master_data_analysis_sum <- aggregate(master_data_analysis_sum$NumberOfUnits, by= list(master_data_analysis_sum$Premier.Facility.ID, master_data_analysis_sum$Cost.Center, format(master_data_analysis_sum$ServiceDate,"%m-%Y")), FUN= 'sum')
      colnames(master_data_analysis_sum)  <- c('Facility ID', 'Cost Center', 'Service  Month', 'Sum of Charges') 
  #Analysis of Data
#analyze_charges <- function(master_df, new_df){
  analyze_master <- as.data.frame(unique(master_data_analysis_sum$`Cost Center`))#replace variable to function variable
  colnames(analyze_master) <- "Cost Center"
  for (i in 1:length(analyze_master$`Cost Center`)){
          analyze_master$Mean[i] <- mean(master_data_analysis_sum[master_data_analysis_sum$`Cost Center`== analyze_master$`Cost Center`[i], 'Sum of Charges'])#replace variable to function variable
          analyze_master$SD[i] <- sd(master_data_analysis_sum[master_data_analysis_sum$`Cost Center`== analyze_master$`Cost Center`[i], 'Sum of Charges']) #replace variable to function variable
  }
  analyze_master$`Lower Limit` <- round((analyze_master$Mean - analyze_master$SD))
  analyze_master$`Upper Limit` <- round((analyze_master$Mean +  analyze_master$SD))
  if(length(analyze_master$`Cost Center`) != length(unique(CPT_data_analysis_sum$`Cost Center`))){
        missing_cc <- analyze_master[!(analyze_master$`Cost Center`  %in% analyze_new$`Cost Center`), 'Cost Center']
        cat(paste0("File uploaded is missing data for ", length(missing_cc), ' cost centers: '), fill = T)
        cat(paste(missing_cc), fill = T)
        stop("File is not accurate")
      }else { #still working on this, what are criteria of charge volume to flag?
        for (i in 1:length(CPT_data_analysis_sum$`Cost Center`)){
          
        }
      }
  for(i in 1:length(CPT_data_analysis_sum$`Cost Center`)){
    missing_charges <- as.data.frame(NA)
    colnames(missing_charges) <- 'Cost Center'
    increased_charges <- as.data.frame(NA)
    colnames(increased_charges) <- 'Cost Center'
    if(CPT_data_analysis_sum$`Sum of Charges`[i] < analyze_master[analyze_master$`Cost Center`== CPT_data_analysis_sum$`Cost Center`[i], 'Lower Limit']){
      missing_charges <- rbind.data.frame(CPT_data_analysis_sum$`Cost Center`[i], missing_charges)
    }else if (CPT_data_analysis_sum$`Sum of Charges`[i] > analyze_master[analyze_master$`Cost Center`== CPT_data_analysis_sum$`Cost Center`[i], 'Upper Limit']){
      increased_charges <- rbind.data.frame(CPT_data_analysis_sum$`Cost Center`[i],increased_charges)
    }
  } #generating list of cc that are outside limits
  if(length(na.omit(increased_charges)) > 0){
    cat("The following Cost Center(s) showed a large increase in charges:", fill = T)
    cat(paste(increased_charges), fill=T)
  }else if (length(na.omit(missing_charges)) > 0){
    cat("The following Cost Centers(s) are missing a large amount of charges:", fill = T)
    cat(paste(missing_charges), fill=T)
    stop("File is missing data")
  }
}

# Appending New Data to Master --------------------------------------------
master_data_RAW<- merge.data.frame(master_data_RAW, CPT_data_RAW, all=T)

# Exporting Master Data File ---------------------------------------------------
filename_master <- paste0("MasterData_", format(range_master_data[1], '%d%b%y'), ' to ', format(range_new_data[2], '%d%b%y'), '.csv')
write.table(master_data_RAW, file = filename_master, row.names = F, col.names = T, sep = ',')
