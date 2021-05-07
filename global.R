library(shiny); library(dataRetrieval); library(tidyverse); library(lubridate); # library(dplyr)
library(magrittr); library(caTools); library(dygraphs); library(xts)
library(rvest); library(xml2)
# library(tibbletime); library(readxl); library(xlsx)

## Todo: Add function to install missing libraries.

# streamStatus <- readRDS('appData//streamStatus.rds')
# streamData <- readRDS('appData//streamData.rds')
# month5thPercentiles <- readRDS('appData//30_Day_5th_Percentiles.rds')
# multiDayPercentiles <- readRDS('appData//Multiday_Mean_Percentiles.rds')

############### Monitoring Site Data #######################
# columns: site_no, type, label, startDate, endDate, lat, lng.
# include a function to fill in missing lat lng for USGS sites (?)
# type %in% c('stream', 'lake', 'duke')
remove_quotes <- function(x) {stringr::str_remove_all(x, '"') %>% stringr::str_remove_all("'")}

sites <- read.csv('www//app_inputs.csv', stringsAsFactors=FALSE) %>% 
  dplyr::mutate(site_no=remove_quotes(site_no),
         startDate=as.character(startDate) %>% remove_quotes(),
         endDate=as.character(endDate) %>% remove_quotes() ) %>% 
  dplyr::mutate(startDate=dplyr::if_else(is.na(startDate), '', startDate),
         endDate=dplyr::if_else(is.na(endDate), '', endDate) )

source('streamflow.R')

source('reservoirs.R')

source('groundwater.R')

counties <- scwateruse::counties


##### Junk code that used to be in a test.R file:
# calculateMultiDayPercentiles2 <- function(Data) {
#   group_by(Data, label) %>%
#     do({
#       x <- arrange(., Date) %>%
#         mutate(NA_flag = is.na(Flow)) %>%
#         mutate(NA_count14 = zoo::rollapply(NA_flag, 14, sum, na.rm=TRUE, fill=NA_integer_, align='right'),
#                NA_count28 = zoo::rollapply(NA_flag, 28, sum, na.rm=TRUE, fill=NA_integer_, align='right'),
#                Flow14 = zoo::rollapply(Flow, 14, mean, na.rm=TRUE, fill=NA_real_, align='right'),
#                Flow28 = zoo::rollapply(Flow, 28, mean, na.rm=TRUE, fill=NA_real_, align='right') ) } ) %>%
#     mutate(Flow14=if_else(NA_count14 > 4, NA_real_, Flow14),
#            Flow28=if_else(NA_count28 > 9, NA_real_, Flow28)) %>% ungroup() %>%
#     mutate(Year = year(Date)) %>%
#     select(label, Year, Day_of_year, Flow14, Flow28) # %>%
#   # saveRDS('Multiday Percentiles.rds')
# }
# 
# calculateMultiDayPercentiles2(approvedData) -> test
# 
# ## yesterday is 210
# filter(test, Day_of_year==210) %>%
#   select(-Flow28, -Day_of_year) %>%
#   spread(label, Flow14) %>%
#   write.csv('test.csv', na='')
# 
# 
# # ######## Test updateFlowData function ##############
# ## Test with an endDate and a startDate...
# # oldStreamData <- filter(streamData, Date < "2018-03-01")
# # updatedStreamData <- updateStreamData(sites, oldStreamData)
# # 
# # incompleteStreamData <- filter(streamData, site_no != "02130900")
# # completedStreamData <- updateStreamData(sites, incompleteStreamData)
# # 
# # streamData %<>% arrange(site_no, Date)
# # updatedStreamData %<>% arrange(site_no, Date)
# # completedStreamData %<>% arrange(site_no, Date)
# # 
# # all.equal(streamData, updatedStreamData) ## TRUE
# # all.equal(streamData, completedStreamData) ## TRUE
# # all.equal(updatedStreamData, completedStreamData) ## TRUE


