shinyServer(function(input, output) { 

  reactiveFile <- function(file, sec=3, readFunc=readRDS) {
    reactiveFileReader(intervalMillis=sec*1000, session=NULL, filePath=file, readFunc=readFunc)}
  
  if(!file.exists("appData//streamData.rds")) {
    withProgress(message="appData/streamData.rds not found.",
                 detail='Initializing stream flow data.',
                 expr=initializeStreamData(sites) ) }
  streamData <- reactiveFile('appData//streamData.rds')
  
  if(!file.exists("appData//30_Day_5th_Percentiles.rds")) {
    withProgress(message="appData/30_Day_5th_Percentiles.rds not found.",
                 detail='Initializing 30 day 5th percentiles.',
                 expr=calculate30day5thPercentiles(
                   readRDS("appData//streamData.rds")) ) }
  month5thPercentiles <- reactiveFile('appData//30_Day_5th_Percentiles.rds')
  
  if(!file.exists("appData//Multiday_Mean_Percentiles.rds")) {
    withProgress(message="appData/Multiday_Mean_Percentiles.rds not found.",
                 detail="Initializing Multi-day mean percentiles (this takes 5-10 minutes)",
                 expr=calculateMultiDayPercentiles(
                   readRDS("appData//streamData.rds")) ) }
  multiDayPercentiles <- reactiveFile('appData//Multiday_Mean_Percentiles.rds')
  
  if(!file.exists("appData//streamStatus.rds")) {
    updateStreamStatus(
      readRDS("appData//streamData.rds"), 
      readRDS("appData//Multiday_Mean_Percentiles.rds"),  
      readRDS("appData//30_Day_5th_Percentiles.rds") ) }
  streamStatus <- reactiveFile('appData//streamStatus.rds', 1)
  
  observeEvent(input$updateStreamData, ignoreInit=TRUE, {
    updateStreamData(sites, streamData()) } )
  
  observeEvent(input$updateStreamStatus, ignoreInit=TRUE, {
     updateStreamStatus(streamData(), multiDayPercentiles(), month5thPercentiles() ) })
  
  observeEvent(input$updateStreamStats, ignoreInit=TRUE, {
    withProgress(message='Calculating statistics', {
      incProgress(.5, detail='30 day 5th percentiles')
      calculate30day5thPercentiles(streamData() )
      incProgress(.5, detail='multi-day mean percentiles (this can take 5-10 minutes)')
      calculateMultiDayPercentiles(streamData())
    } )
 } )
  
  output$streamTable <- renderTable(streamStatus())
  
  output$downloadStreamStatus <- downloadHandler(
    filename= function() {paste0('Stream Flow Drought Status', Sys.Date(), '.csv')},
    content= function(file) {write.csv(streamStatus(), file) },
    contentType="text/csv")

  month5thPercentiles2 <- reactive({
      spread(month5thPercentiles(), label, `30-Day 5th Percentile`) } ) # label
  
  output$`30_day_5th_percentiles` <- renderTable(month5thPercentiles2())
  
  output$download30day5th <- downloadHandler(
    filename= function() {paste0('30 Day 5th Percentile Stream Flow ', Sys.Date(), '.csv')},
    content= function(file) write.csv(month5thPercentiles2(), file) )
  
  multiDayPercentiles2 <- reactive({
    filter(multiDayPercentiles(), Day_and_month==input$dayAndMonth & ndays==input$multiDays &
             !is.na(Flow)) %>%
      select(label, Flow, `Interpolated_Percentile`=percentileIntrp) %>%
      spread(label, Flow) })
  
  output$multiDayPercentiles <- renderTable(multiDayPercentiles2())
  
  output$downloadMultiDayPercentiles <- downloadHandler(
    filename= function() {paste0('MultiDay Mean Percentile Stream Flow ', Sys.Date(), '.csv')},
    content= function(file) {write.csv(multiDayPercentiles2(), file) },
    contentType="text/csv")
    
  output$streamPlot <- renderPlot({
    plot_stream(site_label=input$streamPlotGageLabel,
                index_type=input$nDays,
                .streamData=streamData(), .month5thPercentiles=month5thPercentiles(),
                .multiDayPercentiles=multiDayPercentiles() ) })
  
  
####### Reservoirs
  ## if () this file doesn't exist, create it.
  lakeData <- reactiveFile('appData//lakeData.rds')
  
  observeEvent(input$updateLakeData, ignoreInit=TRUE, {
    filter(sites, type %in% c('lake', 'duke')) %>% updateLakeData() } ) 
  ## add endDate option to this...
  ## add manual override for duke levels?
  
  output$lakeTable <- renderTable({lakeData() } )
  
  output$downloadLakeStatus <- downloadHandler(
    filename= paste0('Reservoir Drought Status', Sys.Date(), '.csv'),
    content= function(file) {lakeData() %>%
        write.csv(file) } )
  
})
