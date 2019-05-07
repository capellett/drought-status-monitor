library(shiny); library(dataRetrieval); library(tidyverse); library(lubridate)
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
remove_quotes <- function(x) {str_remove_all(x, '"') %>% str_remove_all("'")}

sites <- read.csv('www//app_inputs.csv', stringsAsFactors=FALSE) %>% 
  mutate(site_no=remove_quotes(site_no),
         startDate=as.character(startDate) %>% remove_quotes(),
         endDate=as.character(endDate) %>% remove_quotes() ) %>% 
  mutate(startDate=if_else(is.na(startDate), '', startDate),
         endDate=if_else(is.na(endDate), '', endDate) )


################## Stream Flow Data #################
removeNWISattributes <- function(x) {
  attr(x, 'url') <- NULL; attr(x, 'siteInfo') <- NULL
  attr(x, 'variableInfo') <- NULL; attr(x, 'url') <- NULL
  attr(x, 'disclaimer') <- NULL; attr(x, 'statisticInfo') <- NULL
  attr(x, 'queryTime') <- NULL; return(x)}

downloadStreamData <- function(siteNumber, startDate='', endDate='') {
  Data <- readNWISdv(
    siteNumber=siteNumber, parameterCd='00060', 
    startDate=startDate, endDate=endDate) %>%
    renameNWISColumns()
  if(!("Flow" %in% names(Data)) &&
     !("Flow_cd" %in% names(Data) ) ) {
    Data %<>% rename(Flow = PUBLISHED_Flow,
                     Flow_cd = PUBLISHED_Flow_cd ) }
  Data %>% select(site_no, Date, Flow, Flow_cd) %>%
    removeNWISattributes() %>%
    mutate(Day_of_year = as.numeric(strftime(Date, format='%j')),
           Day_and_month = strftime(Date, format='%b %d'),
           Flow14 = caTools::runmean(Flow, 14, 'exact', 'NA', 'right'),
           Flow28 = caTools::runmean(Flow, 28, 'exact', 'NA', 'right') ) }

initializeStreamData <- function(sites) {
  sites %<>% filter(type=='stream')
    newData <- list()
    for(i in 1:nrow(sites)){
      site <- sites[i,]
      endingDate <- site$endDate != '' & !is.na(site$endDate) & is.Date(as_date(site$endDate))
      startDate <- site$startDate
      newData[[i]] <- downloadStreamData(
        siteNumber=site$site_no, startDate=startDate, endDate=site$endDate) }
    bind_rows(newData) %>% 
      left_join(select(sites, site_no, label), by='site_no') %>% 
      saveRDS('appData//streamData.rds') }

updateStreamData <- function(sites, streamData) {
  sites %<>% filter(type=='stream')
  withProgress(message='Updating Stream Flow Data', value=0, {
    newData <- list()
    for(i in 1:nrow(sites)){
      site <- sites[i,]
      existingSite <- site$site_no %in% streamData$site_no
      endingDate <- site$endDate != '' & !is.na(site$endDate) & is.Date(as_date(site$endDate))
      startDate <- if(existingSite) {
        streamData[streamData$site_no == site$site_no,]$Date %>%
          max(na.rm=TRUE) - 365} else {site$startDate}
      progressDetail = paste0(
        "Downloading data from ",
        if(!existingSite){"new site "} else {" "},
        site$label, startDate, ' to ',
        if(endingDate){site$endDate} else {'present.'})
      incProgress(.9/nrow(sites), detail=progressDetail)
      newData[[i]] <- downloadStreamData(
        siteNumber=site$site_no, startDate=startDate, endDate=site$endDate) 
      if(existingSite) {newData[[i]] %<>% filter(Date > min(Date) + 28) }
      if(endingDate){streamData %<>% 
          filter(!(site_no == site$site_no & Date > as_date(site$endDate) ) ) } }
    incProgress(.1, detail='Combining and saving data.')
    newData <- bind_rows(newData) %>% 
      left_join(select(sites, site_no, label), by='site_no')
    streamData %>% anti_join(newData, by=c('site_no', 'Date')) %>% 
      bind_rows(newData) %>% 
      filter(site_no %in% sites$site_no) %>% 
      saveRDS('appData//streamData.rds') } ) }

calculate30day5thPercentiles <- function(Data) {
  Data2 <- mutate(Data, Day_of_year = as.numeric(Day_of_year))
  group_by(Data2, label) %>%
    do({site_flow_data <- .
      Data3 <- group_by(site_flow_data, Day_of_year) %>%
        summarise(`30-Day 5th Percentile` = {
          day <- unique(Day_of_year)
          dayRange <- (day-15):(day+15)
          Data4 <- filter(site_flow_data, 
                          (Day_of_year %in% dayRange) | 
                            ((Day_of_year - 365) %in% dayRange) |
                            ((Day_of_year + 365) %in% dayRange))
          quantile(Data4$Flow, probs=c(.05), na.rm=T, names=F) } )
      } ) %>% ungroup() %>% saveRDS('appData//30_Day_5th_Percentiles.rds')}

calculateMultiDayAverageFlows <- function(Data) {
  mutate(Data, Flow=if_else((Flow_cd=='A' | Flow_cd=='A e') & Flow >=0, Flow, NA_real_)) %>%
    group_by(label) %>%
    do({
      x <- arrange(., Date) %>%
        mutate(NA_flag = is.na(Flow)) %>%
        mutate(NA_count14 = zoo::rollapply(NA_flag, 14, sum, na.rm=TRUE, fill=NA_integer_, align='right'),
               NA_count28 = zoo::rollapply(NA_flag, 28, sum, na.rm=TRUE, fill=NA_integer_, align='right'),
               Flow14 = zoo::rollapply(Flow, 14, mean, na.rm=TRUE, fill=NA_real_, align='right'),
               Flow28 = zoo::rollapply(Flow, 28, mean, na.rm=TRUE, fill=NA_real_, align='right') ) } ) %>%
    mutate(Flow14=if_else(NA_count14 > 4, NA_real_, Flow14),
           Flow28=if_else(NA_count28 > 9, NA_real_, Flow28)) %>% ungroup() %>%
    select(label, Day_and_month, Flow14, Flow28)
}

calculateRawFlowPercentiles <- function(x){ ## x is all of the Flow values for a given gage, day, & nday
  y <- sort(x[!is.na(x)])
  tibble(Flow=y, percentile=seq_along(y)*100/(length(y)+1))
}

calculateInterpolatedPercentileFlows <- function(x){ ## x is a tibble with percentile and Flow
  tibble(percentileIntrp=seq(5,95,5)) %>% rowwise() %>%
    mutate(
      Flow = {
        if(min(x$percentile) > percentileIntrp) return(NA_real_)
        if(max(x$percentile) < percentileIntrp) return(NA_real_)
        low = filter(x, percentile == max(x$percentile[x$percentile < percentileIntrp]))
        high = filter(x, percentile == min(x$percentile[x$percentile > percentileIntrp]))
        b = low$Flow
        m = (high$Flow - low$Flow) / (high$percentile - low$percentile)
        flow = m*(percentileIntrp-low$percentile)+b 
        if(length(flow)==0) return(NA_real_) else return(flow)} ) %>%
    ungroup() %>% 
    bind_rows(
      {filter(x, percentile == min(percentile) & percentile < 5) %>%
        rename(percentileIntrp=percentile)},
      {filter(x, percentile == max(percentile) & percentile > 95) %>%
        rename(percentileIntrp=percentile) } ) }

calculateMultiDayPercentiles <- function(streamData) {
    incProgressSteps <- nrow(unique(streamData[,c('label', 'Day_and_month')]))
    incProgress(detail="Calculating 14 and 28-day means")
    streamData %>%
      calculateMultiDayAverageFlows() %>%
      group_by(label, Day_and_month) %>%
      do({
        incProgress(1/incProgressSteps, 
                    detail=paste0(unique(.$label), ' ', unique(.$Day_and_month)))
        fourteen <- calculateRawFlowPercentiles(.$Flow14) %>%
          calculateInterpolatedPercentileFlows()
        twentyeight <- calculateRawFlowPercentiles(.$Flow28) %>%
          calculateInterpolatedPercentileFlows()
        bind_rows(`14`=fourteen, `28`=twentyeight, .id='ndays')
      }) %>% ungroup() %>% saveRDS('appData//Multiday_Mean_Percentiles.rds')
}

calculateInterpolatedFlowPercentile <- function(.label, .Day_and_month, .ndays, 
                                                .Flow, .multiDayPercentiles) {
  z <- filter(.multiDayPercentiles, label==.label & Day_and_month==.Day_and_month & 
                ndays==.ndays & !is.na(Flow))
  if(nrow(z)==0) return(NA_real_)
  low = if(.Flow <= min(z$Flow)) {return(0)} else {
    filter(z, Flow == max(z$Flow[z$Flow < .Flow], na.rm=TRUE)) }
  high = if(.Flow >= max(z$Flow)) {return(nrow(z)/(nrow(z)+1))} else {
    filter(z, Flow == min(z$Flow[z$Flow > .Flow], na.rm=TRUE)) }
  b = low$percentileIntrp
  m = (high$percentileIntrp - low$percentileIntrp) / (high$Flow - low$Flow)
  m * (.Flow - low$Flow) + b
}

updateStreamStatus <- function(streamData, multiDayPercentiles, month5thPercentiles) {
  withProgress(message = "Updating drought status", {
    streamStatus <- streamData[!(is.na(streamData$Flow)),] %>%
      group_by(label) %>% slice(which.max(Date)) %>%
      left_join(month5thPercentiles, by=c('label', 'Day_of_year') ) %>%
      ungroup() %>% rowwise() %>%
      mutate(
        `14-Day Percentile` = {
          a <- calculateInterpolatedFlowPercentile(
            .label=label, .Day_and_month=Day_and_month, .ndays='14', .Flow=Flow14,
            .multiDayPercentiles=multiDayPercentiles)
          if(length(a)==0 | is.na(a) | is.null(a)) {a <- NA_real_}
          a},
        `28-Day Percentile` = {
          a <- calculateInterpolatedFlowPercentile(
            .label=label, .Day_and_month=Day_and_month, .ndays='28', .Flow=Flow28,
            .multiDayPercentiles=multiDayPercentiles)
          if(length(a)==0 | is.na(a) | is.null(a)) {a <- NA_real_}
          a})
    
    # for(i in 1:nrow(streamStatus)){
    #   x <- streamStatus[i,]
    #   incProgress(.5/nrow(streamData), detail= x$label)
    #   y <- semi_join(multiDayPercentiles, x, by=c('label','Day_of_year')) %>%
    #     select(Flow14, Flow28)
    #   streamStatus[i, "14-Day Percentile"] <- {
    #     z <- filter(y, !is.na(Flow14))$Flow14
    #     rank(c(z, now=x$Flow14), 'keep', 'average')[['now']]*100 / (length(z)+2)
    #     }
    #   streamStatus[i, "28-Day Percentile"] <- {
    #     z <- filter(y, !is.na(Flow28))$Flow28
    #     rank(c(z, now=x$Flow28), 'keep', 'average')[['now']]*100 / (length(z)+2)
    #     }  }
    
    incProgress(.5, detail='Labeling drought statuses')
    drought_labels <- c('Extreme','Severe','Moderate', 
                        'Incipient','no drought')
    streamStatus %>% mutate(
      `Regulatory Method` = cut(Flow14/`30-Day 5th Percentile`, 
                                c(Inf, 1.2, 1.1, 1, .9, 0),
                                labels=drought_labels, ordered_result=T),
      `14-Day Method` = cut(`14-Day Percentile`, c(100, 20, 10, 5, 2, 0), 
                            labels=drought_labels, ordered_result=T),
      `28-Day Method` = cut(`28-Day Percentile`, c(100, 20, 10, 5, 2, 0), 
                            labels=drought_labels, ordered_result=T) ) %>%
      select(Date, site_no, label, Flow, Flow14, `30-Day 5th Percentile`, `Regulatory Method`,
             `14-Day Percentile`, `14-Day Method`, Flow28, `28-Day Percentile`, `28-Day Method`) %>% 
      ungroup() %>% mutate(Date=format(Date, '%m-%d-%Y')) %>% saveRDS('appData//streamStatus.rds')
  }) # format(dat$date,'%Y-%m-%d')
  }

# updateStreamStatus(streamData, multiDayPercentiles, month5thPercentiles)

### Function to return Drought Status Table (For all sites)
di_table <- function(siteNumbers=siteNumbers, flowData=flowData) {}

#################### Flow Plotting #############################################
### Function to plot Drought Index History (For a given site)
plot_stream <- 
  function(site_label="Black Crk nr McBee", 
           index_type="14", .streamData=streamData, 
           .month5thPercentiles=month5thPercentiles, 
           .multiDayPercentiles=multiDayPercentiles) {
    .streamData <- filter(.streamData, label==site_label) %>%
      rename(DailyFlow=Flow, Flow=paste0("Flow", index_type))
    .month5thPercentiles <- filter(.month5thPercentiles, label==site_label)
    .multiDayPercentiles <- filter(
      .multiDayPercentiles, label==site_label & ndays==index_type &
        percentileIntrp %in% c(5,25,50,75,95)) %>%
      mutate(percentileIntrp=paste0(percentileIntrp, "th Percentile")) %>%
      spread(percentileIntrp, Flow)
    minMaxes <- group_by(.streamData, Day_of_year) %>%
      summarise(
        min=min(Flow, na.rm=T), max=max(Flow, na.rm=T),
        DailyMin=min(DailyFlow, na.rm=T), DailyMax=max(DailyFlow, na.rm=T))
    futureDays <- tibble(Date=as_date((today()+1):(today()+30))) %>%
      mutate(Day_of_year = as.numeric(strftime(Date, format='%j')),
             Day_and_month = strftime(Date, format='%b %d'))
    .streamData <- filter(.streamData, Date>=today()-364) %>%
      bind_rows(futureDays) %>%
      left_join(.month5thPercentiles, 'Day_of_year') %>%
      left_join(minMaxes, 'Day_of_year') %>%
      left_join(.multiDayPercentiles, 'Day_and_month')
    ggplot(.streamData, aes(x=Date)) +
      geom_ribbon(aes(ymin=`5th Percentile`, ymax=`25th Percentile`),
                  fill="lightsalmon", alpha=.5) +
      geom_ribbon(aes(ymin=`25th Percentile`, ymax=`50th Percentile`),
                  fill="olivedrab", alpha=.5) +
      geom_ribbon(aes(ymin=`50th Percentile`, ymax=`75th Percentile`),
                  fill="greenyellow", alpha=.5) +
      geom_ribbon(aes(ymin=min, ymax=`5th Percentile`), 
                  fill="firebrick", alpha=.5) +
      geom_ribbon(aes(ymin=`75th Percentile`, ymax=`95th Percentile`),
                  fill="turquoise", alpha=.5) +
      geom_ribbon(aes(ymin=`95th Percentile`, ymax=max),
                  fill="blue", alpha=.5)+
      geom_line(aes(y=Flow)) +
      geom_line(aes(y=`30-Day 5th Percentile`), linetype='dashed') +
      geom_line(aes(y=DailyMin), linetype='dotted', alpha=.5) +
      geom_line(aes(y=DailyFlow), size=.1, alpha=.25) +
      scale_y_log10() +
      labs(title=site_label, subtitle=paste0(index_type, " day average"),
           caption="Colored area spans the historical range of the multi-day average flow.
           Colored bands divide the 5th, 25th, 50th, 75th, and 95th percentiles.
           Gray line is the daily average flow.
           Dotted line is the historical minimum daily average flow.
           Dashed line is the historical 5th percentile daily flow, using a 30 day window.") +
      theme_bw() +
      coord_cartesian(ylim=range(
        .streamData[,c("Flow", "95th Percentile", "DailyMin")], na.rm=TRUE)) +
      ylab("Streamflow, cubic feet per second")
  }
# plot_stream(index_type='28')
# plot_stream()
# for(i in filter(sites, type=='stream')$label %>% unique()) {plot_stream(i) %>% print()}


# filter(streamData, label=='Chattooga nr Clayton' & Date > as_date('2018-3-01') &
#          Date < '2018-04-30') %>% View()


# flow='Flow14'; indices=c('Monthly 5th Percentile', '30-Day 5th Percentile')
di_plot <- function(siteNumber, flowData=flowData, flow='Flow14',
                    indices=c('Monthly 5th Percentile', '30-Day 5th Percentile')) {
  select_cols=c('Date', flow, indices)
  filter(flowData, site_no==siteNumber & Date > "2001-10-01" & Date < "2002-10-01") %>%
    select(select_cols) %>%
    xts(x=.[,-1], order.by=.$Date) %>%
    dygraph()
}
# di_plot('02110500', flowData)

di_plot <- function(siteNumber, flowData=flowData, flow='Flow14',
                    indices=c('Monthly 5th Percentile', '30-Day 5th Percentile')) {
  select_cols=c('Date', flow, indices)
  filter(flowData, site_no==siteNumber) %>%
    select(select_cols) %>%
    xts(x=.[,-1], order.by=.$Date) %>%
    dygraph()
}


################################ LAKE DATA ##################################
scrapeDukeLakeLevels <- function(
  lakes=c('Jocassee', 'Keowee', 'Wateree', 'Wylie') ) {
  
  lakeData <- tribble(~site_name, ~site_no, ~`Water Level`, ~dateTime,
                     'Jocassee', '9', NA_real_, NA_character_,
                     'Keowee', '10', NA_real_, NA_character_,
                     'Wateree', '17', NA_real_, NA_character_,
                     'Wylie', '18', NA_real_, NA_character_)  
  withProgress(message='Scraping Duke Website', value=0, {
    for(lake in lakes) {
      incProgress(1/length(lakes), detail=paste('Downloading data for lake', lake))
      lake_number = filter(lakeData, site_name==lake)$site_no
      url <- paste0('https://lakes.duke-energy.com/index.html#/detail/', 
                  lake_number,'/Detail#Detail')
      outfile <- paste0('dukeScrape//', lake, '_level.html')
    
      ### Modify the phantom JS script
      lines <- readLines("dukeScrape//duke_scrape_template.js")
      lines[1] <- paste0("var url ='", url ,"';")
      lines[2] <- paste0("var outfile = '", outfile, "';")
      writeLines(lines, "dukeScrape//duke_scrape.js")
      ### Render and save the page using phantom JS
      system("dukeScrape//phantomjs dukeScrape//duke_scrape.js")
    
      scraped_page <- read_html(outfile) 
      
      file.remove(outfile)
      file.remove("dukeScrape//duke_scrape.js")
      
      scraped_data <- html_nodes(
        scraped_page, xpath='/html/body/div[1]/div[2]/div/div[2]/h2') %>%
        as.character() %>% str_replace_all('[<][^>]*[>]', '') %>%
        str_replace_all(' ft.', '') %>% as.numeric()
      lakeData[lakeData$site_name==lake, "Water Level"] <- scraped_data 
    
      scraped_time <- html_nodes(
        scraped_page, xpath='/html/body/div[1]/div[2]/div/div[2]/p') %>%
        as.character() %>% str_replace_all('[<][^>]*[>]', '') %>%
        str_replace_all('\n', '') %>% str_trim() %>%
        str_replace_all('As of ', '')
      lakeData[lakeData$site_name==lake, 'dateTime'] <- scraped_time # as_datetime(scraped_time)
    } 
    }); 
  filter(lakeData, site_name %in% lakes) }

downloadUSGSLakeData <- function(siteNumbers) {
  lapply(siteNumbers, function(x) {
    Data <- readNWISdata(
      sites=x, service='iv', parameterCd='00062',
      tz='America/New_York') %>%
      renameNWISColumns() %>% 
      rename(`Water Level`='X_00062_Inst')
    Data$site_name = attr(Data, 'siteInfo')$station_nm
    Data %>%
      select(-agency_cd, -`X_00062_Inst_cd`, -tz_cd) %>% # mutate(Date=as_datetime(Date))
      removeNWISattributes() } ) %>% bind_rows() }

## updateLakeData
## add endDate; add manual over-ride for duke levels?
updateLakeData <- function(sites) {
  USGS_lake_data <- downloadUSGSLakeData(filter(sites, type=='lake')$site_no)
  Duke_lake_data <- scrapeDukeLakeLevels() %>%
    mutate(dateTime=mdy_hm(dateTime))
  lakeData <- bind_rows(USGS_lake_data, Duke_lake_data) %>%
    left_join(sites, by='site_no') %>%
    mutate(Month=month(dateTime),
           Day=day(dateTime)) %>%
    select(site_no, label, Month, Day, dateTime, `Water Level`)
  
  guideCurves <- readxl::read_xlsx('www//Guide_Curves.xlsx', na="NA") %>%
    semi_join(lakeData, by=c('Month', 'Day')) %>%
    gather(key='label', value='Target')
  
  fullPool <- tribble(~label, ~`Full Pool`,
                     'Greenwood', 439,
                     'Murray', 360, 
                     'Moultrie', 75.6,
                     'Hartwell', 660,
                     'Thurmond', 330,
                     'Blalock', 710,
                     'Jocassee', 100,
                     'Keowee', 100,
                     'Wateree', 100,
                     'Wylie', 100)
  
  lakeData %<>% left_join(guideCurves, 'label') %>% 
    left_join(fullPool, 'label') %>%
    mutate(`Deviation from Guide Curve` = `Water Level`-Target,
           `Deviation from Full Pool` = `Water Level` - `Full Pool`) %>%
    select(site_no, label, Date=dateTime, `Water Level`, Target, `Full Pool`, 
           `Deviation from Guide Curve`, `Deviation from Full Pool`) %>%
    saveRDS('appData//lakeData.rds')
}

########
# USGS_lake_data <- readRDS('USGS_lake_data.rds')
# Duke_lake_data <- readRDS('Duke_lake_data.rds')
# 

### Flag missing data


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
