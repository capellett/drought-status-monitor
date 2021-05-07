#####################################################
################## Stream Flow Data #################
#####################################################

removeNWISattributes <- function(x) {
  attr(x, 'url') <- NULL; attr(x, 'siteInfo') <- NULL
  attr(x, 'variableInfo') <- NULL; attr(x, 'url') <- NULL
  attr(x, 'disclaimer') <- NULL; attr(x, 'statisticInfo') <- NULL
  attr(x, 'queryTime') <- NULL; return(x)}

downloadStreamData <- function(siteNumber, startDate='', endDate='') {
  Data <- dataRetrieval::readNWISdv(
    siteNumbers=siteNumber, parameterCd='00060', 
    startDate=startDate, endDate=endDate) %>%
    dataRetrieval::renameNWISColumns()
  if(!("Flow" %in% names(Data)) &&
     !("Flow_cd" %in% names(Data) ) ) {
    Data %<>% rename(Flow = PUBLISHED_Flow,
                     Flow_cd = PUBLISHED_Flow_cd ) }
  Data %>% select(site_no, Date, Flow, Flow_cd) %>%
    removeNWISattributes()}

addStreamDataColumns <- function(data) {
  dplyr::mutate(data, Day_of_year = as.numeric(strftime(Date, format='%j')),
                Day_and_month = strftime(Date, format='%b %d'),
                Flow14 = caTools::runmean(Flow, 14, 'exact', 'NA', 'right'),
                Flow28 = caTools::runmean(Flow, 28, 'exact', 'NA', 'right') ) %>%
    dplyr::mutate(Day_and_month = dplyr::if_else(Day_and_month == "Feb 29",
                                                 "Feb 28", Day_and_month))
}

initializeStreamData <- function(sites) {
  sites %<>% dplyr::filter(type=='stream')
  newData <- list()
  for(i in 1:nrow(sites)){
    site <- sites[i,]
    # endingDate <- site$endDate != '' & !is.na(site$endDate) & is.Date(as_date(site$endDate))
    startDate <- site$startDate
    newData[[i]] <- downloadStreamData(
      siteNumber=site$site_no, startDate=startDate, endDate=site$endDate) }
  dplyr::bind_rows(newData) %>% 
    dplyr::left_join(dplyr::select(sites, site_no, label), by='site_no') %>% 
    addStreamDataColumns() %>%
    saveRDS('appData//streamData.rds') }

updateStreamData <- function(sites, streamData) {
  sites %<>% dplyr::filter(type=='stream')
  shiny::withProgress(message='Updating Stream Flow Data', value=0, {
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
      shiny::incProgress(.9/nrow(sites), detail=progressDetail)
      newData[[i]] <- downloadStreamData(
        siteNumber=site$site_no, startDate=startDate, endDate=site$endDate) 
      if(existingSite) {newData[[i]] %<>% filter(Date > min(Date) + 28) }
      if(endingDate){streamData %<>% 
          filter(!(site_no == site$site_no & Date > as_date(site$endDate) ) ) } }
    shiny::incProgress(.1, detail='Combining and saving data.')
    newData <- bind_rows(newData) %>% 
      tidyr::complete(site_no, Date) %>%
      addStreamDataColumns() %>%
      dplyr::left_join(dplyr::select(sites, site_no, label), by='site_no')
    streamData %>% dplyr::anti_join(newData, by=c('site_no', 'Date')) %>% 
      dplyr::bind_rows(newData) %>% 
      dplyr::filter(site_no %in% sites$site_no) %>% 
      saveRDS('appData//streamData.rds') } ) }

calculate30day5thPercentiles <- function(Data) {
  Data2 <- dplyr::mutate(Data, Day_of_year = as.numeric(Day_of_year))
  dplyr::group_by(Data2, label) %>%
    dplyr::do({site_flow_data <- .
    Data3 <- dplyr::group_by(site_flow_data, Day_of_year) %>%
      dplyr::summarise(`30-Day 5th Percentile` = {
        day <- unique(Day_of_year)
        dayRange <- (day-21):(day+6)
        Data4 <- dplyr::filter(site_flow_data, 
                               (Day_of_year %in% dayRange) | 
                                 ((Day_of_year - 365) %in% dayRange) |
                                 ((Day_of_year + 365) %in% dayRange))
        quantile(Data4$Flow, probs=c(.05), na.rm=T, names=F) } )
    } ) %>% dplyr::ungroup() %>% saveRDS('appData//30_Day_5th_Percentiles.rds')}

calculateMultiDayAverageFlows <- function(Data) {
  dplyr::mutate(Data, Flow=if_else((Flow_cd=='A' | Flow_cd=='A e') & Flow >=0, Flow, NA_real_)) %>%
    dplyr::group_by(label) %>%
    dplyr::do({
      x <- dplyr::arrange(., Date) %>%
        dplyr::mutate(NA_flag = is.na(Flow)) %>%
        dplyr::mutate(NA_count14 = zoo::rollapply(NA_flag, 14, sum, na.rm=TRUE, fill=NA_integer_, align='right'),
                      NA_count28 = zoo::rollapply(NA_flag, 28, sum, na.rm=TRUE, fill=NA_integer_, align='right'),
                      Flow14 = zoo::rollapply(Flow, 14, mean, na.rm=TRUE, fill=NA_real_, align='right'),
                      Flow28 = zoo::rollapply(Flow, 28, mean, na.rm=TRUE, fill=NA_real_, align='right') ) } ) %>%
    dplyr::mutate(Flow14=if_else(NA_count14 > 4, NA_real_, Flow14),
                  Flow28=if_else(NA_count28 > 9, NA_real_, Flow28)) %>% dplyr::ungroup() %>%
    dplyr::select(label, Day_and_month, Flow14, Flow28)
}

calculateRawFlowPercentiles <- function(x){ ## x is all of the Flow values for a given gage, day, & nday
  y <- sort(x[!is.na(x)])
  tibble::tibble(Flow=y, percentile=seq_along(y)*100/(length(y)+1))
}

calculateInterpolatedPercentileFlows <- function(x){ ## x is a tibble with percentile and Flow
  minPercentile <- min(x$percentile, na.rm=TRUE)
  maxPercentile <- max(x$percentile, na.rm=TRUE)
  
  stupidInterpolation <- function(Interpercentile) {
    low <- dplyr::filter(x, percentile == max(x$percentile[x$percentile < Interpercentile]))[1,]
    high <- dplyr::filter(x, percentile == min(x$percentile[x$percentile > Interpercentile]))[1,]
    b <- low$Flow
    m <- (high$Flow - low$Flow) / (high$percentile - low$percentile)
    flow <- m*(Interpercentile-low$percentile)+b 
    if(
      (minPercentile > Interpercentile) |
      (maxPercentile < Interpercentile) |
      (length(flow)==0) ) NA_real_ else flow}
  
  y <- tibble::tibble(percentileIntrp=seq(5,95,5)) %>% dplyr::rowwise() %>%
    dplyr::mutate(Flow = stupidInterpolation(percentileIntrp) ) %>%
    dplyr::ungroup()
  z <- dplyr::filter(x, (percentile == min(percentile) & percentile < 5) |
                       (percentile == max(percentile) & percentile > 95) ) %>%
    dplyr::rename(percentileIntrp=percentile)
  dplyr::bind_rows(y, z)
}

calculateMultiDayPercentiles <- function(streamData) {
  incProgressSteps <- nrow(unique(streamData[,c('label', 'Day_and_month')]))
  shiny::incProgress(detail="Calculating 14 and 28-day means")
  calculateMultiDayAverageFlows(streamData) %>%
    dplyr::group_by(label, Day_and_month) %>%
    dplyr::do({
      shiny::incProgress(1/incProgressSteps, 
                         detail=paste0(unique(.$label), ' ', unique(.$Day_and_month)))
      dplyr::bind_rows(
        `14`= calculateRawFlowPercentiles(.$Flow14) %>% calculateInterpolatedPercentileFlows(), 
        `28`= calculateRawFlowPercentiles(.$Flow28) %>% calculateInterpolatedPercentileFlows(), 
        .id='ndays')
    }) %>% dplyr::ungroup() %>% saveRDS('appData//Multiday_Mean_Percentiles.rds')
}

calculateInterpolatedFlowPercentile <- function(.label, .Day_and_month, .ndays, 
                                                .Flow, .multiDayPercentiles) {
  z <- dplyr::filter(.multiDayPercentiles, label==.label & Day_and_month==.Day_and_month & 
                       ndays==.ndays & !is.na(Flow))
  if(nrow(z)==0) return(NA_real_)
  low = if(.Flow <= min(z$Flow)) {return(0)} else {
    filter(z, Flow == max(z$Flow[z$Flow < .Flow], na.rm=TRUE)) }
  high = if(.Flow >= max(z$Flow)) {return(99)} else {
    filter(z, Flow == min(z$Flow[z$Flow > .Flow], na.rm=TRUE)) }
  b = low$percentileIntrp
  m = (high$percentileIntrp - low$percentileIntrp) / (high$Flow - low$Flow)
  m * (.Flow - low$Flow) + b
}

updateStreamStatus <- function(streamData, multiDayPercentiles, month5thPercentiles) {
  shiny::withProgress(message = "Updating drought status", {
    streamStatus <- streamData[!(is.na(streamData$Flow)),] %>%
      dplyr::group_by(label) %>% slice(which.max(Date)) %>%
      dplyr::left_join(month5thPercentiles, by=c('label', 'Day_of_year') ) %>%
      dplyr::ungroup() %>% dplyr::rowwise() %>%
      dplyr::mutate(
        `14-Day Percentile` = {
          a <- calculateInterpolatedFlowPercentile(
            .label=label, .Day_and_month=Day_and_month, .ndays='14', .Flow=Flow14,
            .multiDayPercentiles=multiDayPercentiles)[[1]]
          if(length(a)==0 | is.na(a) | is.null(a)) {a <- NA_real_}
          a},
        `28-Day Percentile` = {
          a <- calculateInterpolatedFlowPercentile(
            .label=label, .Day_and_month=Day_and_month, .ndays='28', .Flow=Flow28,
            .multiDayPercentiles=multiDayPercentiles)[[1]]
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
    
    shiny::incProgress(.5, detail='Labeling drought statuses')
    drought_labels <- c('Extreme','Severe','Moderate', 
                        'Incipient','No Drought')
    streamStatus %>% dplyr::mutate(
      `Regulatory Method` = cut(Flow14/`30-Day 5th Percentile`, 
                                c(Inf, 1.2, 1.1, 1, .9, 0),
                                labels=drought_labels, ordered_result=T),
      `14-Day Method` = cut(`14-Day Percentile`, c(100, 20, 10, 5, 2, 0), 
                            labels=drought_labels, ordered_result=T),
      `28-Day Method` = cut(`28-Day Percentile`, c(100, 20, 10, 5, 2, 0), 
                            labels=drought_labels, ordered_result=T) ) %>%
      dplyr::select(Date, site_no, label, Flow, Flow14, `30-Day 5th Percentile`, `Regulatory Method`,
                    `14-Day Percentile`, `14-Day Method`, Flow28, `28-Day Percentile`, `28-Day Method`) %>% 
      dplyr::ungroup() %>% dplyr::mutate(Date=format(Date, '%m-%d-%Y')) %>% saveRDS('appData//streamStatus.rds')
  }) # format(dat$date,'%Y-%m-%d')
}

# updateStreamStatus(streamData, multiDayPercentiles, month5thPercentiles)

### Function to return Drought Status Table (For all sites)
di_table <- function(siteNumbers=siteNumbers, flowData=flowData) {}

#################### Stream Flow Plot #############################################
### Function to plot Drought Index History (For a given site)
plot_stream <- 
  function(site_label="Black Crk nr McBee", 
           index_type="14", .streamData=streamData, 
           .month5thPercentiles=month5thPercentiles, 
           .multiDayPercentiles=multiDayPercentiles) {
    .streamData <- dplyr::filter(.streamData, label==site_label) %>%
      dplyr::rename(DailyFlow=Flow, Flow=paste0("Flow", index_type))
    .month5thPercentiles <- dplyr::filter(.month5thPercentiles, label==site_label)
    .multiDayPercentiles <- dplyr::filter(
      .multiDayPercentiles, label==site_label & ndays==index_type &
        percentileIntrp %in% c(5,25,50,75,95)) %>%
      dplyr::mutate(percentileIntrp=paste0(percentileIntrp, "th Percentile")) %>%
      tidyr::spread(., percentileIntrp, Flow)
    minMaxes <- dplyr::group_by(.streamData, Day_of_year) %>%
      dplyr::summarise(
        min=min(Flow, na.rm=T), max=max(Flow, na.rm=T),
        DailyMin=min(DailyFlow, na.rm=T), DailyMax=max(DailyFlow, na.rm=T))
    futureDays <- tibble::tibble(Date=as_date((today()+1):(today()+30))) %>%
      dplyr::mutate(Day_of_year = as.numeric(strftime(Date, format='%j')),
                    Day_and_month = strftime(Date, format='%b %d'))
    .streamData <- dplyr::filter(.streamData, Date>=today()-364) %>%
      dplyr::bind_rows(futureDays) %>%
      dplyr::left_join(.month5thPercentiles, 'Day_of_year') %>%
      dplyr::left_join(minMaxes, 'Day_of_year') %>%
      dplyr::left_join(.multiDayPercentiles, 'Day_and_month') %>%
      dplyr::mutate(Flow=dplyr::if_else(Flow<=0, NA_real_, Flow),
                    DailyMin=dplyr::if_else(DailyMin<=0, NA_real_, DailyMin))
    y_vals <- .streamData[,c('Flow', '95th Percentile', 'DailyMin')] %>% unlist()
    y_vals[!is.finite(y_vals)] <- NA_real_
    y_range <- range(y_vals, na.rm=TRUE)
    ggplot2::ggplot(.streamData, ggplot2::aes(x=Date)) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=`5th Percentile`, ymax=`25th Percentile`),
                           fill="lightsalmon", alpha=.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=`25th Percentile`, ymax=`50th Percentile`),
                           fill="olivedrab", alpha=.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=`50th Percentile`, ymax=`75th Percentile`),
                           fill="greenyellow", alpha=.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=min, ymax=`5th Percentile`), 
                           fill="firebrick", alpha=.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=`75th Percentile`, ymax=`95th Percentile`),
                           fill="turquoise", alpha=.5) +
      ggplot2::geom_ribbon(ggplot2::aes(ymin=`95th Percentile`, ymax=max),
                           fill="blue", alpha=.5)+
      ggplot2::geom_line(ggplot2::aes(y=Flow)) +
      ggplot2::geom_line(ggplot2::aes(y=`30-Day 5th Percentile`), linetype='dashed') +
      ggplot2::geom_line(ggplot2::aes(y=DailyMin), linetype='dotted', alpha=.5) +
      ggplot2::geom_line(ggplot2::aes(y=DailyFlow), size=.1, alpha=.25) +
      ggplot2::scale_y_log10() +
      ggplot2::labs(title=site_label, subtitle=paste0(index_type, " day average"),
                    caption="Colored area spans the historical range of the multi-day average flow.
           Colored bands divide the 5th, 25th, 50th, 75th, and 95th percentiles.
           Gray line is the daily average flow.
           Dotted line is the historical minimum daily average flow.
           Dashed line is the historical 5th percentile daily flow, using a 30 day window.") +
      ggplot2::theme_bw() +
      ggplot2::coord_cartesian(ylim=y_range) +
      ggplot2::ylab("Streamflow, cubic feet per second")
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
  dplyr::filter(flowData, site_no==siteNumber & Date > "2001-10-01" & Date < "2002-10-01") %>%
    dplyr::select(select_cols) %>%
    xts(x=.[,-1], order.by=.$Date) %>%
    dygraph()
}
# di_plot('02110500', flowData)

di_plot <- function(siteNumber, flowData=flowData, flow='Flow14',
                    indices=c('Monthly 5th Percentile', '30-Day 5th Percentile')) {
  select_cols=c('Date', flow, indices)
  dplyr::filter(flowData, site_no==siteNumber) %>%
    dplyr::select(select_cols) %>%
    xts(x=.[,-1], order.by=.$Date) %>%
    dygraph()
}
