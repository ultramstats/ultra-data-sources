  ### Big's Backyard Ultra ####

  # This script generates the data set(s) used in the analysis of the 2019
  # Big's Backyard Ultra Marathon

  # https://ultramstats.com

  # The data is stored  here: https://my6.raceresult.com/139372/results?lang=en#0_2C3B48
  # It is not in a suitable form for direct analysis. As such, it needs to be manipulated into a useable form
  # once it is scraped into R. 

  # Load in packages
  library(rvest)
  library(tidyverse)
  library(jsonlite)
  library(lubridate)
  library(survival)
  library(survminer)
  library(stringr)
  library(DT)

  # url where data is stored --> This is the JSON file containing all of the data on the webpage
  url <- "https://my6.raceresult.com/RRPublish/data/list.php?callback=jQuery171019224300360763946_1572549135033&eventid=139372&key=3a52cf488ad3dfe5c994f6b203e7c2e8&listname=Result+Lists%7CLap+Details&page=results&contest=0&r=all&l=0&_=1572549135407"

  ## read data into r by processing json
  
  # establish a link to the JSON file
  page <- html_session(url)
  
  # read the JSON file
  json <- readBin(page$response$content, what = "json")
  json1 <- substring(json, 3461, 77037)

  # This is what the data looks like, we want to keep columns 2, 4, and 5.
  # column1 = BibNumber --> discard
  # column2 = lap
  # column3 = actual time (military) --> discard
  # column4 = lap time
  # column5 = rest time

  # create a list of each runner's results
  bigs_list <- lapply(fromJSON(json1), as.data.frame)

  # merge the lists into one data frame
  bigs_df <- bind_rows(bigs_list, .id = "column_label")

  # This long sequence manipulates the data into a suitable form for analysis
  bigs_df <- bigs_df %>% mutate(Name = as.factor(str_match(column_label, "///(.*?)///")[,2]),
                                Lap = as.numeric(V2), Lap_Time = V4, 
                                Rest_Time = V5) %>%
                         select(Name, Lap, Lap_Time, Rest_Time) %>%
                         mutate(Lap_Time = minute(as.POSIXct(Lap_Time, format = "%M:%S")) + 
                                           second(as.POSIXct(Lap_Time, format = "%M:%S"))/60,
                                Rest_Time = minute(as.POSIXct(Rest_Time, format = "%M:%S")) +
                                            second(as.POSIXct(Rest_Time, format = "%M:%S"))/60) %>%
                        group_by(Name) %>% 
                        mutate(Total_Time_Course = cumsum(Lap_Time),
                               Total_Time_Rest = cumsum(Rest_Time),
                               Distance = Lap * 4.1667,
                               Drop = ifelse(Name != "Maggie Guterl" & row_number() == n(),
                                             "YES", "NO"),
                               Status = ifelse(Drop == "YES", 2,1))
  
  # Convert Drop ("YES", "NO") from character to factor
  bigs_df$Drop <- as.factor(bigs_df$Drop)
  
  # bigs_df is our first data set. It contains all lap data for each runner. Each row gives the lap information 
  # associated with a specific lap and a specific runner.
  
  # to save out bigs_df as an .csv we can run the code below
  write_csv(bigs_df, path = "data sources/Bigs Backyard Ultra 2019/BBU19 Lap Level Data.csv")
  

  # Of interest is a summary of each runner's efforts.
  # bigs_df_last_lap provides this. Each row gives the final performance data for each runner
  bigs_df_last_lap <- bigs_df %>% filter(row_number() == n())
  
  # to save out bigs_df_last_lap as an c.sv we can run the code below
  write_csv(bigs_df_last_lap, path = "data sources/Bigs Backyard Ultra 2019/BBU19 Final Level Data.csv")
  
