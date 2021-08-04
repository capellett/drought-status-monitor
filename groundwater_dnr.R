library(tidyverse)

dnr_wells <- c('AIK-0849', 'ALL-0372', 'CTF-0081',
               'GRV-3342', 'JAS-0425', 'KER-0263',
               'LRN-1705', 'ORG-0431', 'SAL-0069')



grv3342 <- read.csv('documentation/GRV-3342_WLs.txt') %>%
  dplyr::mutate(Day = lubridate::mdy_hms(Day))

unique(grv3342$CountyID)
## There are two values.

unique(grv3342$site_serial)


initialize_dnr_well_data <- function(sites) {}

update_dnr_well_data <- function(sites) {}

calculate_gw_percentiles <- function(gw_data) {}
