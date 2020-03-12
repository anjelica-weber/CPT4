# Loading Libraries -------------------------------------------------------
library(xlsx)
library(dplyr)

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
  #Subsetting Data by Date
  premier_data <<- df[((df$ServiceDate >= start_date) & (df$ServiceDate <= end_date)),] #subset of data by date range of service
  premier_data <- subset(premier_data, select = c("FacilityId", "RevenueCenter" ,"ChargeCode", "ServiceDate" ,"NumberOfUnits"))
  #Importing Dictionaries
    #Charge Code to CPT Code "Charge Description Master"
    cat("Select the most recent Charge Description Master", fill = T)
    path_dict_CDM <- file.choose(new = T)
    dict_CDM <- read.csv(file = path_dict_CDM, na.strings = c("", "Unavailable"))
    dict_CDM <- subset(dict_CDM, select = c("CHARGE_CODE", "IPTB_cpt4"))
    colnames(dict_CDM)<- c("ChargeCode", "CPTCode")
    dict_CDM <<- dict_CDM
    #Revenue to Cost Center 
    cat("Select the Revenue to Cost Center Mapping file", fill = T)
    path_dict_CC <- file.choose(new = T)
    dict_CC <<- read.xlsx(path_dict_CC, sheetIndex = 1)
  #Merging dictionaries and data
    premier_data <- merge.data.frame(premier_data, dict_CC, all.x = T, all.y = F)
    premier_data <- merge.data.frame(premier_data, dict_CDM, all.x = T, all.y = F)
  #Aggregating Data
    premier_data <- aggregate(premier_data$NumberOfUnits, by= list(premier_data$Premier.Facility.ID, premier_data$Cost.Center,premier_data$ServiceDate, premier_data$CPTCode), FUN= 'sum')
    colnames(premier_data) <- c('Facility ID', 'Cost Center', 'Start Date', 'CPT4 Code', 'Volume' )
  # Processing Data
    premier_data <- arrange(premier_data,`Facility ID`, `Start Date`, `Cost Center`, `CPT4 Code` )
    premier_data$`Budget` <- rep(0, length(premier_data$`Facility ID`))
    premier_data$`End Date` <- premier_data$`Start Date`
    premier_data$`Corporate ID` <- rep(729805, length(premier_data$`Facility ID`))
    premier_data <- premier_data[, c(8,1,2,3,7,4,5,6)] #correct column order
  return(premier_data)
} 
data_Premier <- generate_Premier_file(master_data_RAW, readline(prompt = "Start Date of Pay Period needed(mm/dd/yyyy):"),readline(prompt = "End date of Pay Period needed(mm/dd/yyyy):") )

# Generate Omitted Data Report --------------------------------------------
generate_omitt_report <- function(){
  #Ommited due to missing CPT code
  dict_CDM_omit <- dict_CDM
  dict_CDM_omit <- dict_CDM_omit[is.na(dict_CDM_omit$CPTCode),]
  dict_CDM_omit$Reason2 <- rep('Missing CPT code, no equivalent for charge code', length(dict_CDM_omit$ChargeCode))
  dict_CDM_omit <- dict_CDM_omit[, c('ChargeCode', 'Reason2')]
  #Ommitted due to missing cost center
  dict_CC_known <- subset(dict_CC, select = c("FacilityId", "RevenueCenter"))
  dict_CC_known$ID <- paste0(dict_CC_known$FacilityId, '-', dict_CC_known$RevenueCenter)
  rev_missing <- subset(premier_data, select=c("FacilityId", "RevenueCenter"))
  rev_missing$ID <- paste0(rev_missing$FacilityId, '-', rev_missing$RevenueCenter)
  dict_CC_omit <- rev_missing[!rev_missing$ID  %in% dict_CC_known$ID,]
  dict_CC_omit <- dict_CC_omit[match(unique(dict_CC_omit$ID), dict_CC_omit$ID),c("FacilityId", "RevenueCenter")]
  dict_CC_omit$Reason1 <- rep('Missing Cost Center', length(dict_CC_omit$FacilityId))
  # Creating Filter of Omitted Data
  premier_data_test <- premier_data
  premier_data_test$Row <- rep(1:length(premier_data_test$FacilityId))
  data_omitted_2 <- premier_data_test[premier_data_test$ChargeCode %in% dict_CDM_omit$ChargeCode, 'Row']
  data_omitted_1 <- premier_data_test[!(paste0(premier_data_test$FacilityId,'-', premier_data_test$RevenueCenter)) %in%dict_CC_known$ID,'Row']
  data_omitted_both <- c(data_omitted_1,data_omitted_2)
  data_omitted_both <- data_omitted_both[duplicated(c(data_omitted_both))]
  data_omitted_CC <- as.data.frame(data_omitted_1[!data_omitted_1 %in% data_omitted_both])
  colnames(data_omitted_CC) <- 'Row' 
  data_omitted_CC$Reason <- rep('Missing Cost Center',length(data_omitted_CC$Row))
  data_omitted_CDM <- as.data.frame(data_omitted_2[!data_omitted_2 %in% data_omitted_both])
  colnames(data_omitted_CDM) <- 'Row' 
  data_omitted_CDM$Reason <- rep('Missing CPT Code for Charge Code',length(data_omitted_CDM$Row))
  data_omitted_both <- as.data.frame(data_omitted_both)
  colnames(data_omitted_both) <- 'Row'
  data_omitted_both$Reason <- rep('Missing Cost Center and CPT Code',length(data_omitted_both$Row))
  data_omitted <- merge.data.frame(data_omitted_both, data_omitted_CC, all=T)
  data_omitted <- merge.data.frame(data_omitted, data_omitted_CDM, all=T)
  #Subsetting Data
  data_return <- merge.data.frame(premier_data_test, data_omitted, all=F)
  return(data_return)
}


# Exporting Premier Files -------------------------------------------------
#data_west <- data_dates_needed[(data_dates_needed$`Facility ID`=="NY2162"),]
#data_lukes <- data_dates_needed[(data_dates_needed$`Facility ID`=="NY2163"),]
#write.table(data_west, file ="MSW_CPT4_1AUG19 to 31DEC19.csv",row.names = F, col.names = T, sep = ',')
#write.table(data_lukes, file ="MSSL_CPT4_1AUG19 to 31DEC19.csv",row.names = F, col.names = T, sep = ',')
#write.table(data_Premier, file = "test_full master.csv", row.names = F, col.names = T, sep = ',')

