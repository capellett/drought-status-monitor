shinyUI(
  fluidPage(
    verticalLayout(
      titlePanel("Hydrologic Indicators of Drought in South Carolina"),
      tabsetPanel(
        tabPanel(
          title='Stream Flow',
          tabsetPanel(
            tabPanel(
              title='Stream Flow Drought Status Table',
              actionButton('updateStreamData', 'Update Stream Flow Data'),
              actionButton('updateStreamStatus', 'Update Stream Flow Drought Status'),
              downloadButton('downloadStreamStatus', 'Export Stream Flow Drought Status Table'),
              tableOutput("streamTable")
              ),
            # tabPanel(
            #   title='Stream Flow Map'
            # ),
            tabPanel(
              title='Stream Flow Statistics',
              # textOutput('flowStatsLastUpdatedText'),
              actionButton('updateStreamStats', 'Update Stream Flow Statistics'),
              tabsetPanel(
                tabPanel('30-day 5th percentiles',
                         downloadButton('download30day5th', 'Export 30-day 5th percentiles'),
                         tableOutput('30_day_5th_percentiles') ),
                tabPanel('Multi-day mean percentiles',
                         selectInput('multiDays', 'Number of days', c('14', '28'), '14'),
                         selectInput('dayAndMonth', 'Day and Month', 
                                     strftime(as_date('2000-01-01')+0:365, '%b %d'), 
                                     strftime(today()-1, '%b %d')),
                         downloadButton('downloadMultiDayPercentiles', 'Export Multi-day mean percentiles'),
                         tableOutput('multiDayPercentiles') ) )
              ),
            tabPanel(
              title='Stream Flow Data',
              actionButton('updateStreamData', 'Update Stream Flow Data'),
              dateRangeInput('streamDateRange', 'Date range', start=today()-28, end=today()),
              # downloadButton('downloadStreamData', 'Export Stream Flow Data'),
              tableOutput('streamData')
              ),
            tabPanel(
              title='Stream Flow Map',
              plotOutput('streamMap')
            ),
            tabPanel(
              title='Stream Flow Plot',
              selectInput('streamPlotGageLabel', 'Stream Gage', filter(sites, type=='stream')$label),
              selectInput('nDays', 'Number of days', c('14', '28')),
              plotOutput('streamPlot') )
            )
          ),
        tabPanel(
          title='Lakes',
          tabsetPanel(
            tabPanel(
              title='Lake Level Table',
              actionButton('update_Lake_Levels', 'Update Lake Levels'),
              downloadButton('downloadLakeStatus', 'Export Lake Level Table'),
              tableOutput('lakeTable')
              ),
            tabPanel(
              title='Lake Map',
              plotOutput('lakeMap') ),
            tabPanel(
              title='Lake Level Guide Curves',
              p("Coming soon")
              )
            )
          ),
        tabPanel(
          title='Groundwater',
          # p("Coming soon")
          tabsetPanel(
            tabPanel(
              title='Groundwater Condition Table',
              actionButton('update_USGS_Well_Data', 'Update USGS Well Data'),
              actionButton('update_GW_status', 'Update Groundwater Conditions'),
              downloadButton('downloadGWStatus', 'Export Groundwater Conditions'),
              tableOutput('gwTable') ),
            tabPanel(
              title='Groundwater Condition Maps',
              plotOutput('gwMap_all'),
              plotOutput('gwMap_usgs_wells'),
              plotOutput('gwMap_baseflow') )
            )
          )
        )
      )
    )
  )

       ## download map shapefile
       ## download output table
       ## download flow table
       ## download monthly percentiles
       ## download 14 and 28 day flow percentiles
