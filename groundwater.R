#####################################
###### Groundwater Data #############
#####################################

download_USGS_Well_Data <- function(siteNumber, startDate='', endDate='') {
  Data <- dataRetrieval::readNWISgwl(siteNumbers=siteNumber,
                      startDate=startDate, 
                      endDate=endDate) %>%
    dplyr::select(
      -agency_cd, ## all "USGS"
      -site_tp_cd, ## all "GW"
      -lev_tm, ## time, not necessary
      -lev_tz_cd_reported, ## time zone, not necessary
      -sl_lev_va, ## all NA
      -sl_datum_cd, ## all NA
      -lev_status_cd, ## all NULL
      -lev_agency_cd, ## not necessary? mostly USGS, some SC008 and NC###
      -lev_dt_acy_cd, ## date accuracy?
      -lev_acy_cd, ## all NA
      -lev_src_cd, ## S (3,839) and A (87) and NA (~1,000)
      -lev_meth_cd, ## mostly S and V, with some other letters too. ??
      ## -lev_age_cd, ## 4,852 A, 179 P... approved and provisional?
      ## -lev_dt, ## some dates aren't right, use lev_dateTime column
      -lev_tz_cd)  %>% ## all "UTC"
    dplyr::mutate(lev_dateTime = as.Date(lev_dateTime), ## some are NA/failed to parse
                  Value=lev_va*-1) %>% ## Value is (negative) height above land surface.
    dplyr::mutate(lev_dateTime = dplyr::if_else(
      is.na(lev_dateTime), lubridate::as_date(lev_dt), lev_dateTime)) %>%
    dplyr::rename(Date=lev_dateTime) %>%
    dplyr::select(-lev_dt, -lev_va)
    
  attr(Data, "header") <- NULL
  attr(Data, "queryTime") <- NULL
  attr(Data, "url") <- NULL
  attr(Data, "siteInfo") <- NULL
  
  return(Data)}


initialize_USGS_Well_Data <- function(sites) {
  sites <- dplyr::filter(sites, type=='USGS well')
  newData <- list()
  for(i in 1:nrow(sites)){
    site <- sites[i,]
    # endingDate <- site$endDate != '' & !is.na(site$endDate) & is.Date(as_date(site$endDate))
    startDate <- site$startDate
    newData[[i]] <- download_USGS_Well_Data(
      siteNumber=site$site_no, startDate=startDate, endDate=site$endDate) }
  
  wellData <- dplyr::bind_rows(newData) %>% 
    dplyr::left_join(dplyr::select(sites, site_no, label), by='site_no')
  
  saveRDS(wellData, 'appData//usgsWellData.rds')
  invisible(wellData)}

# initialize_USGS_Well_Data(sites)


update_USGS_Well_Data <- function(sites, usgsWellData) {
  sites %<>% dplyr::filter(type=='USGS well')
  shiny::withProgress(message='Updating USGS Well Data', value=0, {
    newData <- list()
    for(i in 1:nrow(sites)){
      site <- sites[i,]
      existingSite <- site$site_no %in% usgsWellData$site_no
      endingDate <- site$endDate != '' & !is.na(site$endDate) & is.Date(as_date(site$endDate))
      startDate <- if(existingSite) {
        usgsWellData[usgsWellData$site_no == site$site_no,]$Date %>%
          max(na.rm=TRUE) - 365} else {site$startDate}
      shiny::progressDetail = paste0(
        "Downloading data from ",
        if(!existingSite){"new site "} else {" "},
        site$label, startDate, ' to ',
        if(endingDate){site$endDate} else {'present.'})
      shiny::incProgress(.9/nrow(sites), detail=progressDetail)
      newData[[i]] <- download_USGS_Well_Data(
        siteNumber=site$site_no, startDate=startDate, endDate=site$endDate) 
      if(existingSite) {newData[[i]] %<>% dplyr::filter(Date > min(Date) + 28) }
      if(endingDate){usgsWellData %<>% 
          dplyr::filter(!(site_no == site$site_no & Date > lubridate::as_date(site$endDate) ) ) } }
    incProgress(.1, detail='Combining and saving data.')
    newData <- dplyr::bind_rows(newData) %>% 
      tidyr::complete(site_no, Date) %>%
      dplyr::left_join(dplyr::select(sites, site_no, label), by='site_no')
    
    usgsWellData %<>% dplyr::anti_join(newData, by=c('site_no', 'Date')) %>% 
      dplyr::bind_rows(newData) %>% 
      dplyr::filter(site_no %in% sites$site_no)
    
    saveRDS(usgsWellData, 'appData//usgsWellData.rds') } )
  
  invisible(usgsWellData)}


##### Calculate Baseflow from Streamflow
## Lyne and Hollick Algorithm
## Q_f(i) = 0.925*Q_f(i-1) + (Q(i) - Q(i-1))*1.925/2
## Baseflow: Q_b = Q - Q_f
calculateBaseflow <- function(streamData) {
  baseflow <- streamData %>%
    dplyr::select(site_no, label, Date, Flow) %>%
    dplyr::filter(!is.na(Flow)) %>% ## How should we handle NAs?
    dplyr::group_by(site_no, label) %>%
    dplyr::arrange(site_no, label, Date, by_group=TRUE) %>%
    dplyr::do(
      dplyr::mutate(
        ., 
        Value = EcoHydRology::BaseflowSeparation(
          Flow, filter_parameter = 0.925, passes = 3)[[1]]
      )) %>%
    dplyr::ungroup()
  
  # saveRDS(baseflow, 'appData//baseflow.rds')
  # invisible(baseflow)
  return(baseflow)}
 

## Calculate percentile breaks of monthly median values
## D4 Exceptional : <2
## D3 Extreme : 2-5
## D2 Severe : 5-10
## D1 Moderate : 10-20
## D0 Abnormally Dry : 20-30
## Normal : 30-70
## Wet : >70

# gw_percentiles <- gw_data %>%
#   dplyr::mutate(Month = lubridate::month(Date),
#                 Year = lubridate::year(Date)) %>%
#   dplyr::group_by(Type, site_no, label, Year, Month) %>%
#   dplyr::summarise(Value = median(Value, na.rm=T)) %>%
#   dplyr::ungroup() %>%
#   dplyr::group_by(Type, site_no, label, Month) %>%
#   dplyr::summarise(
#     `2` = quantile(Value, probs=0.02, na.rm=T, names=F),
#     `5` = quantile(Value, probs=0.05, na.rm=T, names=F),
#     `10`= quantile(Value, probs=0.10, na.rm=T, names=F),
#     `20`= quantile(Value, probs=0.20, na.rm=T, names=F),
#     `30`= quantile(Value, probs=0.30, na.rm=T, names=F),
#     `70`= quantile(Value, probs=0.70, na.rm=T, names=F)) %>%
#   dplyr::ungroup()


updateGWStatus <- function(gw_data, .date=lubridate::today()) {
  gwStatus <- gw_data[!(is.na(gw_data$Value)|is.na(gw_data$Date)),] %>%
    dplyr::filter(Date <= .date) %>%
    dplyr::mutate(Month=lubridate::month(Date)) %>%
    dplyr::group_by(Type, site_no, label, Month) %>% 
    dplyr::mutate(Percentile = rank(Value)/(dplyr::n()+1)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(Type, site_no, label) %>%
    dplyr::slice(which.max(Date)) %>%
    dplyr::ungroup()
    
  drought_labels <- c(
    'D4 Exceptional', 'D3 Extreme', 'D2 Severe', 'D1 Moderate',
    'D0 Abnormally Dry', 'Normal', 'Wet')
  
  gwStatus <- gwStatus %>% 
    dplyr::mutate(
      Status = cut(Percentile, c(1.00, 0.70, 0.30, 0.20, 0.10, 0.05, 0.02, 0), 
                   labels=drought_labels, ordered_result=T),
      Date=format(Date, '%m-%d-%Y'))
  
  saveRDS(gwStatus, 'appData//gwStatus.rds') 
  invisible(gwStatus)}

# updateGWStatus(gw_data)

## Make map
mapGroundwater <- function(gwStatus, sites, counties) {
  
  gwStatusPts <- gwStatus %>%
    dplyr::left_join(
      dplyr::select(sites, site_no, lat, lng), by="site_no")
  
  ## interpolate a grid before creating contours
  pts.grid <- akima::interp(gwStatusPts$lng, gwStatusPts$lat, gwStatusPts$Percentile)
  pts.grid2 <- expand.grid(x=pts.grid$x, y=pts.grid$y)
  pts.grid2$z <- as.vector(pts.grid$z)
  
  gwStatusPts %>%
    ggplot2::ggplot() +
    ggplot2::stat_contour_filled(
      data=na.omit(pts.grid2), 
      mapping=ggplot2::aes(x=x, y=y, z=z),
      breaks = c(1, 0.7, 0.3, 0.2, 0.1, .05, .02, 0),
      alpha=0.5) +
    ggplot2::scale_fill_manual(
      limits = c(
        '(0.70, 1.00]', '(0.30, 0.70]', '(0.20, 0.30]', '(0.10, 0.20]', 
        '(0.05, 0.10]', '(0.02, 0.05]', '[0, 0.02]'),
      labels = c(
        '>70   Wet', '30-70 Normal', '20-30 D0 Abnormally Dry', '10-20 D1 Moderate',
        '5-10  D2 Severe', '2-5   D3 Extreme', '<2    D4 Exceptional'),
      values = c(
        'blue3', 'darkgreen', 'yellow', 'sandybrown',
        'darkorange', 'firebrick1', 'darkred') ) +
    ggplot2::geom_sf(data=counties, fill=NA, color='black') +
    ggplot2::geom_point(mapping=ggplot2::aes(x=lng, y=lat, shape=Type), fill='black') + #, color=Status),
                        # show.legend = c(shape=T, color=F)) +
    # ggplot2::scale_color_manual(
    #   values = c(
    #     Wet = 'blue3', Normal = 'darkgreen', `D0 Abnormally Dry` = 'yellow',
    #     `D1 Moderate` = 'sandybrown', `D2 Severe` = 'darkorange', `D3 Extreme` = 'firebrick1',
    #     `D4 Exceptional` = 'darkred')) +
    ggplot2::scale_shape_manual(values=c(baseflow = 21, USGS_well = 24, DNR_well = 25)) +

    ggplot2::theme_bw()}



# usgsWellData_raw <- lapply(
#   1:nrow(USGSwells),
#   function(i) {
#     readNWISgwl(USGSwells[i,]$site_no) %>%
#       dplyr::mutate(lev_dt = as.character(lev_dt))})
# 
# usgsWellSiteInfo <- lapply(
#   usgsWellData_raw, 
#   function(i) {
#     attr(i, "siteInfo") %>%
#       dplyr::mutate(project_no = as.character(project_no))}) %>%
#   bind_rows()

## This doesn't work, gonna have to calculate monthly median percentiles myself.
# gw_stats <- readNWISstat(
#   siteNumbers=c("34505183041800"),
#   parameterCd=c("72019"), ## 	##Depth to water level, feet below land surface
#   # parameterCd=c("30210"), ## Depth to water level, below land surface datum, meters
#   statReportType="monthly",
#   statType="median")





## total river miles in region 1 and total in SC
## 8,000 river miles in the state ?
## Navigable ?
## Light filter and heavy filter
## Presentation to Friends of Lake Keowee
## Lengths of Chatooga (include Tugaloo) and Chauga Rivers within SC
## for Amy, LEO.



