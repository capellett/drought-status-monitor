shinyUI(
  fluidPage(
    verticalLayout(
      titlePanel("Hydrologic Indicators of Drought in South Carolina"),
      tabsetPanel(
        tabPanel(
          title='Stream Flow',
          tabsetPanel(
            tabPanel(
              title='Stream Status Table',
              actionButton('updateStreamData', 'Update Stream Data'),
              actionButton('updateStreamStatus', 'Update Stream Status'),
              downloadButton('downloadStreamStatus', 'Download Stream Status Table'),
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
                         downloadButton('download30day5th', 'Download 30-day 5th percentiles'),
                         tableOutput('30_day_5th_percentiles') ),
                tabPanel('Multi-day mean percentiles',
                         selectInput('multiDays', 'Number of days', c('14', '28'), '14'),
                         selectInput('dayAndMonth', 'Day and Month', 
                                     strftime(as_date('2000-01-01')+0:365, '%b %d'), 
                                     strftime(today()-1, '%b %d')),
                         downloadButton('downloadMultiDayPercentiles', 'Download Multi-day mean percentiles'),
                         tableOutput('multiDayPercentiles') ) )
              ),
            tabPanel(
              title='Stream Data',
              actionButton('updateStreamData', 'Update Stream Data'),
              dateRangeInput('streamDateRange', 'Date range', start=today()-28, end=today()),
              # downloadButton('downloadStreamData', 'Download Stream Flow Data'),
              tableOutput('streamData')
              ),
            tabPanel(
              title='Stream Plot',
              selectInput('streamPlotGageLabel', 'Stream Gage', filter(sites, type=='stream')$label),
              selectInput('nDays', 'Number of days', c('14', '28')),
              plotOutput('streamPlot') )
            )
          ),
        tabPanel(
          title='Reservoirs',
          tabsetPanel(
            tabPanel(
              title='Reservoir Status Table',
              actionButton('updateLakeData', 'Update Lake Levels'),
              downloadButton('downloadLakeStatus', 'Download Reservoir Status Tables'),
              tableOutput('lakeTable')
              ),
            # tabPanel(
            #   title='Reservoir Map'
            # ),
            tabPanel(
              title='Guide Curves'
              )
            )
          ),
        tabPanel(
          title='Groundwater',
          p("Coming soon")
          # tabsetPanel(
          #   tabPanel(
          #     title='Groundwater Status Table'
          #   ),
          #   tabPanel(
          #     title='Groundwater Map'
          #   )
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
