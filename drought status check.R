streamData <- readRDS("~/RshinyApps/drought-status-monitor/appData/streamData.rds")
regmetric <- readRDS("~/RshinyApps/drought-status-monitor/appData/30_Day_5th_Percentiles.rds")

x <- left_join(streamData, regmetric)
y <- select(x, label, Date, Flow14, regmetric=`30-Day 5th Percentile`)

drought_labels <- c('Extreme','Severe','Moderate', 
                    'Incipient','no drought')

z <- mutate(y, 
            Status = cut(Flow14/regmetric, 
            c(Inf, 1.2, 1.1, 1, .9, 0),
            labels=drought_labels, ordered_result=T))

select(z, Status, label) %>% table()





#########################
do_it_all <- function(dataset) {
  PeeDee2 <- transform(dataset,
                       `Discharge (cfs)` = as.numeric(as.character(`Discharge`)), 
                       Month = as.numeric(format(Date, "%m"))) # Converting the columns into numerics
  
  Rank <- function(x) {rank(x, ties.method = "first") } # Ranking the discharge values in ascending order, lowest to highest, duplicate discharges are getting seperate ranks
  PeeDee3 <- transform(PeeDee2, 
                       Rank = ave(`Discharge`, Month, FUN = Rank))
  
  # o <- with(PeeDee3, order(Month, Date))
  # PeeDee3[o, ]
  
  S <- split(PeeDee3, PeeDee3$Month) #splits the data into different months

  total_rows <- sapply(S, nrow) # Calculates total rows of data for each month
  n <- function(total_rows) 0.05*(total_rows+1)
  N <- sapply(total_rows, n)
  k <- sapply(N, floor)# Gives the whole number before decimal for k
  d <- N-k # Gives the integer value after decimal for d
  
  discharge_k <- map2(S, k, ~ .x %>% filter(Rank == .y, !duplicated(Rank))  %>% 
                      select(Discharge)) #creates a list of rows for respective ranks
  p <- k+1
  discharge_p <- map2(S, p, ~ .x %>% filter(Rank == .y, !duplicated(Rank))  %>% 
                      select(Discharge))
  dischar_k <- unlist(discharge_k, use.names = FALSE) #unlist
  dischar_p <- unlist (discharge_p, use.names = FALSE) #unlist
  
  z <- dischar_k + d*(dischar_p - dischar_k) #monthly percentile formula
  return(z) }



