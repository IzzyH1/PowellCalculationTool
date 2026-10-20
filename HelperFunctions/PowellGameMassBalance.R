library(lubridate)
library(readxl)
library(tidyr)


source("HelperFunctions/AutoReadUSBRData/AutoReadUSBRData.R")
source("HelperFunctions/AutoReadCRMMS24MS.R")
source("HelperFunctions/ProjectPowell.R")

# Loads in Hydrodata for storage data
hydrodata <- fReadReclamationHydroData(FALSE)


fPowellGameDataWrangle <- function(Inflow, Inflow_Time, Release, Release_Time, Add_Release, Add_Time, Storage_Data = hydrodata, start_date)
{
 
  # Defines time_grid
  duration <- min(sum(Inflow_Time), sum(Release_Time), sum(Add_Time))
  time_grid<- seq.Date(from = start_date, by = "month", length.out = duration)
  
  #Defines Inflow
  if(any(Inflow == "CRMMS")){
    inflow <- fAutoReadCRMMS24MS("Lake Powell", "Inflow Volume")
    inflow$datetime <- as.Date(inflow$date)
    inflow_fun <- approxfun(as.numeric(inflow$datetime),inflow$'24MS MIN PROB')
    if (length(time_grid) > length(inflow$datetime)){
      time_grid <- inflow$datetime[-(1:2)]   #Indexing makes time_grid start at current month
    }
    inflow_i <- data.frame(datetime = time_grid)
    inflow_i$inflow <- inflow_fun(as.numeric(time_grid))
    
  }else{
    Inflow_Time[is.na(Inflow_Time)] <- 0
    stage_end <- start_date %m+% months(cumsum(Inflow_Time))
    
    # Defines inflow distribution
    inflow_prop <- fMonthlyProportion("Inflow Volume", "Lake Powell", Storage_Data, 2021)
    
    for (i in seq_along(Inflow)){
      inflow_i <- data.frame(datetime = time_grid)
      inflow_i$inflow <- sapply(
        inflow_i$datetime,
        function(d) {
          stage <- which(d <= stage_end)[1]
          if(is.na(stage)){
            0
          }
          else{
            inflow_prop$prop[month(d) == inflow_prop$month] * Inflow[stage]*1000000
          }
        }
      )
    }
  }
 
  
  # Defines Release
  stage_date_ranges <- fCalculateDateRangeGame(start_date, Release_Time)
  release_prop <- fMonthlyProportionGame("Release volume", "Lake Powell", hydrodata, stage_date_ranges)
  
  release_i <- data.frame(datetime = time_grid)
  release_i$release <- sapply(
    release_i$datetime,
    function(d) {
      stage <- which(d < stage_date_ranges$end_date)[1]
      release_prop$prop[d == release_prop$datetime] * Release[stage]*1000000
    }
  ) 
    
    
    # Defines Additional Release data frame
    # Values can be positive for upstream releases (Flaming Gorge)
    
    # Defines when additional release rule changes
    #req(Add_Time)
    #req((length(Add_Time) > 0),
    Add_Time[is.na(Add_Time)] <- 0
    stage_end <- start_date %m+% months(cumsum(Add_Time))
    
    #  Matches releases to dates and places in data frame 
    #if no match add_release = 0
    # add_release is distributed evenly across stage
    add_release_i <- data.frame(datetime = time_grid)
    add_release_i$add_release <- sapply(
      add_release_i$datetime,
      function(d) {
        stage <- which(d <= stage_end)[1]
        if(is.na(stage)){
          return(0)
        }
        Add_Release[stage]/Add_Time[stage] * 1000000
      }
    )
    
    # Merges inflow and add_release
    inflow_i$with_release <- inflow_i$inflow + add_release_i$add_release
    
    
    #Projects Storage and merges into single data frame
    no_add_release <- project_storage(inflow_i$inflow, release_i$release, Storage_Data, time_grid)
    print(no_add_release)
    with_release <- project_storage(inflow_i$with_release, release_i$release, Storage_Data, time_grid)
    if("crashed" %in% c(no_add_release, with_release)){
      return("crashed")
    }
    
    #Adds labels to different operations
    no_add_release$label <- "No Additional Release"
    if (length(Add_Release) > 1){
      with_release$label <- "With Additional Release"
    }
    else{
      with_release$label <- paste0("Additional Release: ", Add_Release[1], " MAF in ", Add_Time[1], " months")
    }
    
    #Places projection into single data frame
    no_add_release$inflow <- inflow_i$inflow
    with_release$inflow <- inflow_i$with_release
    projection<- rbind(no_add_release, with_release)
    return(projection)
}



#Function: Takes the start_date of the simulation and the length of each of the stages and calculates the start and end date of each stage
#Inputs: start_date (Date), stage_durations (numeric vector) value in months
# Outpus: dataframe with columns: start_date (Date) and end_date (Date)
fCalculateDateRangeGame <- function(start_date, stage_durations){
  end_date <- as.Date(rep(NA, length(stage_durations)))
  stage_number <- numeric(length(stage_durations))
  
  for (i in seq_along(stage_durations)){
    end_date[i] <- start_date[i] %m+% months(stage_durations[i])
    start_date[i + 1] <- end_date[i]
    stage_number[i] <- i
  }
  
  #Gets rid of last extra start date
  start_date <- start_date[-length(start_date)]
  
  stage_date_ranges <- data.frame(start_date = start_date, end_date = end_date)
  return(stage_date_ranges)
}
#Test
#parameter <- "Release volume"
#reservoir <- "Lake Powell"
#date_ranges <- stage_date_ranges

#Function: Sums the total of a parameter over a list of date_ranges and calculates the proportion of the total that occurred over the applicable date range that occurs every month
# Inputs: parameter (character) like "Release volume" or "Inflow Volume", reservoir (character), hydrodata (dataframe), date_ranges (dataframe) output of fCalculateDateRange
# Output: dataframe with columns: stage_number (character), datetime (Date), prop (number)
fMonthlyProportionGame <- function(parameter, reservoir, hydrodata, date_ranges){
  
  # Changes date ranges to be for the 2021 water year (6 years ago from simulation)
  date_ranges[] <- lapply(date_ranges, \(x) x %m-% years(6))
  
  # Initial filter of monthly hydrodata
  monthly <- filter(hydrodata$dfResMonthly, ResName == reservoir & FieldName == parameter)
  
  results <- list()
  
  for (i in seq_along(date_ranges$start_date)){
    range_values <- filter(monthly, as.Date(Date) >= date_ranges$start_date[i] & as.Date(Date) < date_ranges$end_date[i])
    range_total <- sum(range_values$MonthlyValue)
    monthly_prop <- data.frame(datetime = as.Date(range_values$Date), prop = range_values$MonthlyValue / range_total)
    
    
    # Changes date range back to 2026 water year
    monthly_prop$datetime <- monthly_prop$datetime %m+% years(6)
    results[[i]] <- monthly_prop #Stores each iteration along loop
  }
  results <-dplyr::bind_rows(results, .id = "stage_number")
  
  #results_init <- dplyr::bind_rows(results, .id = "stage_number")
  #blank_date<- data.frame(stage_number = max(results_init$stage_number), datetime = max(results_init$datetime) %m+% months(1), prop = 0 )
  #results<- rbind(results_init, blank_date)
  
  return(results)
}
