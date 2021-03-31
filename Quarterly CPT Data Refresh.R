
# Load Libraries ----------------------------------------------------------
library(tidyverse)
library(xlsx)
library(lintr)

# User Input --------------------------------------------------------------
#end date of the last pay period of data to refresh
pp_end_date <- as.Date("2020-12-19")

# Constants ---------------------------------------------------------------
num_payperiods_refresh <- 12
dir_rds <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity
            /Volume - Data/MSLW Data/Both Sites Data/Charge Detail"
dir_paycycle <- "J:/deans/Presidents/SixSigma/MSHS Productivity/Productivity
            /Volume - Data/Multisite Volumes/Census Days"

# Load Data ---------------------------------------------------------------
dictionary_pc <- read.xlsx2(paste0(dir_paycycle,
                                   "/Pay Cycle Dictionaries.xlsx"),
                            sheetIndex = 1,
                            colClasses = c("Date", "Date", "Date"))
cpt_data <- readRDS(paste0(dir_rds,
                           "/Source Data/",
                           list.files(paste0(dir_rds, "/Source Data"),
                                      pattern = "RDS$")))

# Preprocessing Data ------------------------------------------------------
pp_refresh <- dictionary_pc %>%
  drop_na() %>%
  arrange(desc(Date)) %>%
  filter(End.Date <= pp_end_date) %>%
  select(End.Date) %>%
  distinct()

dates_refresh <- dictionary_pc %>%
  filter(End.Date %in% pp_refresh$End.Date[1:num_payperiods_refresh])

# Creating File -----------------------------------------------------------
file_upload <- function(all_data, site, start_date, end_date) {
  data_upload <- all_data %>%
    filter(ServiceDate >= start_date,
           ServiceDate <= end_date,
           Premier.Facility.ID == site) %>%
    mutate(Corp = "729805",
           EndDate = ServiceDate,
           ServiceDate = format(ServiceDate, "%m/%d/%Y"),
           EndDate = format(EndDate, "%m/%d/%Y")) %>%
    select(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate,
           CPTCode, NumberOfUnits) %>%
    group_by(Corp, Premier.Facility.ID, Cost.Center, ServiceDate, EndDate,
             CPTCode) %>%
    summarise(Volume = sum(NumberOfUnits, na.rm = T)) %>%
    mutate(Budget = 0) %>%
    drop_na()
}

msw_upload <- file_upload(cpt_data, "NY2162",
                          range(dates_refresh$Date)[1],
                          range(dates_refresh$Date)[2])
msm_upload <- file_upload(cpt_data, "NY2163",
                          range(dates_refresh$Date)[1],
                          range(dates_refresh$Date)[2])

# Export Data -------------------------------------------------------------
write.table(msw_upload,
            file = paste0(dir_rds, "/MSW_CPT_",
                          format(as.Date(range(msw_upload$ServiceDate)[1],
                                         format = "%m/%d/%Y"), "%d%b%y"),
                          " to ",
                          format(as.Date(range(msw_upload$ServiceDate)[2],
                                         format = "%m/%d/%Y"), "%d%b%y"),
                          ".csv"),
            sep = ",", row.names = F, col.names = F)
write.table(msm_upload,
            file = paste0(dir_rds, "/MSM_CPT_",
                          format(as.Date(range(msm_upload$ServiceDate)[1],
                                         format = "%m/%d/%Y"), "%d%b%y"),
                          " to ",
                          format(as.Date(range(msm_upload$ServiceDate)[2],
                                         format = "%m/%d/%Y"), "%d%b%y"),
                          ".csv"),
            sep = ",", row.names = F, col.names = F)
