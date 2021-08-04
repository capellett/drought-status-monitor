## TODO
## Lake Moultrie actually doesn't have a guide curve or target elevation
## Lake Marion does, so maybe we should be using that.


################################ LAKE DATA ##################################
scrapeDukeLakeLevels <- function(
  lakes=c('Jocassee', 'Keowee', 'Wateree', 'Wylie') ) {
  
  lakeData <- tibble::tribble(~site_name, ~site_no, ~`Water Level`, ~dateTime,
                              'Jocassee', '9', NA_real_, NA_character_,
                              'Keowee', '10', NA_real_, NA_character_,
                              'Wateree', '17', NA_real_, NA_character_,
                              'Wylie', '18', NA_real_, NA_character_)  
  shiny::withProgress(message='Scraping Duke Website', value=0, {
    for(lake in lakes) {
      incProgress(1/length(lakes), detail=paste('Downloading data for lake', lake))
      lake_number = dplyr::filter(lakeData, site_name==lake)$site_no
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
      
      scraped_page <- xml2::read_html(outfile) 
      
      file.remove(outfile)
      file.remove("dukeScrape//duke_scrape.js")
      
      scraped_data <- rvest::html_nodes(
        x = scraped_page, 
        xpath = '/html/body/div[1]/div[2]/div/div[2]/h2') %>%
        as.character() %>% 
        stringr::str_replace_all('[<][^>]*[>]', '') %>%
        stringr::str_replace_all(' ft.', '') %>% 
        as.numeric()
      
      lakeData[lakeData$site_name==lake, "Water Level"] <- scraped_data 
      
      scraped_time <- rvest::html_nodes(
        x = scraped_page, 
        xpath='/html/body/div[1]/div[2]/div/div[2]/p') %>%
        as.character() %>% str_replace_all('[<][^>]*[>]', '') %>%
        stringr::str_replace_all('\n', '') %>% stringr::str_trim() %>%
        stringr::str_replace_all('As of ', '')
      
      lakeData[lakeData$site_name==lake, 'dateTime'] <- scraped_time # as_datetime(scraped_time)
    } 
  }); 
  dplyr::filter(lakeData, site_name %in% lakes) }

download_USGS_Lake_Levels <- function(siteNumbers) {
  lapply(siteNumbers, function(x) {
    Data <- dataRetrieval::readNWISdata(
      sites=x, service='iv', parameterCd='00062',
      tz='America/New_York') %>%
      dataRetrieval::renameNWISColumns() %>% 
      dplyr::rename(`Water Level`='X_00062_Inst')
    Data$site_name = attr(Data, 'siteInfo')$station_nm
    Data %>%
      dplyr::select(-agency_cd, -`X_00062_Inst_cd`, -tz_cd) %>% # mutate(Date=as_datetime(Date))
      removeNWISattributes() } ) %>% dplyr::bind_rows() }

## updateLakeData
## add endDate; add manual over-ride for duke levels?
update_Lake_Levels <- function(sites) {
  sites <- dplyr::filter(sites, type %in% c('lake', 'duke'))
  USGS_lake_data <- download_USGS_Lake_Levels(dplyr::filter(sites, type=='lake')$site_no)
  Duke_lake_data <- scrapeDukeLakeLevels() %>%
    dplyr::mutate(dateTime=lubridate::mdy_hm(dateTime))
  lakeData <- dplyr::bind_rows(USGS_lake_data, Duke_lake_data) %>%
    dplyr::left_join(sites, by='site_no') %>%
    dplyr::mutate(Month=lubridate::month(dateTime),
                  Day=lubridate::day(dateTime)) %>%
    dplyr::select(site_no, label, Month, Day, dateTime, `Water Level`)
  
  guideCurves <- readxl::read_xlsx('www//Guide_Curves.xlsx', na="NA") %>%
    dplyr::semi_join(lakeData, by=c('Month', 'Day')) %>%
    tidyr::gather(key='label', value='Target')
  
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
  
  lakeData %<>% dplyr::left_join(guideCurves, 'label') %>% 
    dplyr::left_join(fullPool, 'label') %>%
    dplyr::mutate(`Deviation from Guide Curve` = `Water Level`-Target,
                  `Deviation from Full Pool` = `Water Level` - `Full Pool`,
                  Date=as.character(dateTime)) %>%
    dplyr::select(site_no, label, Date, `Water Level`, Target, `Full Pool`, 
                  `Deviation from Guide Curve`, `Deviation from Full Pool`)
  
  saveRDS(lakeData, 'appData//lakeData.rds')
  invisible(lakeData)
}

### Flag missing data

# lakes <- sf::st_read('mapData/lakes.shp')
# lakes2 <- sf::st_read('mapData/lakes2.shp')
# lakes <- dplyr::bind_rows(lakes, lakes2)
# rm(lakes2)
# lakes$label <- c(
#   'Thurmond', 'Murray', 'Wateree', 'Blalock', 'Hartwell', 'Wylie',
#   'Greenwood', 'Moultrie', 'Hartwell', 'Jocassee', 'Keowee', 'Hartwell')
# 
# lakes <- lakes %>%
#   dplyr::group_by(label) %>%
#   dplyr::summarise(
#     geometry = sf::st_union(geometry))
# 
# saveRDS(lakes, 'mapData/lakes.rds')



make_lake_map <- function(lakeData, lakes=lakes, counties=counties) {
  lakeData %>%
    dplyr::mutate(
      Deviation = 
        dplyr::if_else(is.na(Target), 
                       `Deviation from Full Pool`, 
                       `Deviation from Guide Curve`) %>%
        round(2)) %>%
    dplyr::mutate(
      Deviation = dplyr::if_else(
        Deviation >= 0, paste0("+", Deviation), as.character(Deviation))) %>%
    dplyr::mutate(
      mapText = paste0(label, ": ", Deviation)) %>%
    dplyr::right_join(lakes, by='label') %>%
    ggplot2::ggplot(ggplot2::aes(geometry=geometry)) +
    ggplot2::geom_sf(data=counties, alpha=0.5,
                     ggplot2::aes(fill=DMA)) +
    ggplot2::geom_sf(fill='blue', color='blue') +
    ggplot2::geom_sf_label(ggplot2::aes(label=mapText),
                           nudge_y = 0.1) +
    # ggplot2::scale_fill_manual() +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position="none",
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank()) +
    ggplot2::ggtitle(
      paste0("Lake Level Deficit/Surplus on ", 
             format.Date(max(lakeData$Date), "%b %d, %Y")),
      "(Deficit/Surplus values are referenced to guide curves, 
      except Jocassee and Keowee which are referenced to full pool)")
}


