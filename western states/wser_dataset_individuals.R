### Western States Data Source ####

#data scraped from www.wser.org

# For those that complete the course

# Load in the data of interest: (age, gender, ranking, precipitation,
# weather, previous results)

library(tidyverse)
library(rvest)
library(lubridate)


# Let's Load in the last 30 years of Western States Results
# There was no race in 2008

years <- c(2003,2004,2005,2006,2007,2009,2010,
           2011,2012,2013,2014,2015,2016,2017,2018,2019)

wser_results <- c()
for (k in 1:length(years)) {
  website <- paste0('https://www.wser.org/results/',years[k],'-results/')
  wser_results <- read_html(website) %>% 
    html_nodes('#content') %>%
    map_df(~list(Place = html_nodes(.x,'.column-1') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Year = rep(years[k],length(html_nodes(.x,'.column-1') %>%
                                              html_text() %>%
                                              {if(length(.) == 0){NA} else {.}})),
                 Time = html_nodes(.x,'.column-2') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 First_Name = html_nodes(.x, '.column-4') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Last_Name = html_nodes(.x, '.column-5') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Gender = html_nodes(.x, '.column-6') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Age = html_nodes(.x, '.column-7') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 State = html_nodes(.x, '.column-9') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}})) %>%
    rbind(wser_results,.)
}

# We want to get rid of the header rows and convert to factors and numeric 
# where needed

wser_results_sb <- wser_results %>% filter(Age != "Age") %>%
  mutate(Place = as.numeric(Place), 
         Age = as.numeric(Age),
         Time = hms(as.character(as.factor(Time))),
         Hours = as.numeric(hour(Time) + (minute(Time)/60)),
         Athlete = paste0(First_Name,Last_Name),
         State = as.factor(State),
         Gender = as.factor(Gender),
         Year = as.factor(Year),
         Silver_Buckle = as.factor(ifelse(Hours > 24, "No", "Yes"))) %>%
  dplyr::select(Silver_Buckle, Athlete, First_Name, Last_Name, Year, 
                Time, Age, State, Gender)


# Get temperature data

website <- paste0('https://www.wser.org/weather')
temp_data <- read_html(website) %>%
  html_nodes('#content') %>%
  map_df(~list(Date = html_nodes(.x, '.column-1') %>%
                 html_text() %>%
                 {if(length(.) == 0){NA} else {.}},
               High_Temp = html_nodes(.x, '.column-2') %>%
                 html_text() %>% 
                 {if(length(.) == 0){NA} else {.}},
               Low_Temp = html_nodes(.x, '.column-3') %>%
                 html_text() %>% 
                 {if(length(.) == 0){NA} else {.}}))

temp_data <- temp_data[-1,] %>% 
  mutate(Year = as.factor(seq(2019,1974,-1)),
         High_Temp = as.numeric(High_Temp),
         Low_Temp = as.numeric(Low_Temp)) %>%
  dplyr::select(Year, High_Temp, Low_Temp)

# Get snowpack data
website <- paste0('https://www.wser.org/snowpack')
snowpack_data <- read_html(website) %>%
  html_nodes('#content') %>%
  map_df(~list(Year = html_nodes(.x, '.column-1') %>%
                 html_text() %>%
                 {if(length(.) == 0){NA} else {.}},
               April_1 = html_nodes(.x, '.column-2') %>%
                 html_text() %>%
                 {if(length(.) == 0){NA} else {.}},
               May_1 = html_nodes(.x, '.column-3') %>%
                 html_text() %>%
                 {if(length(.) == 0){NA} else {.}},
               June_1 = html_nodes(.x, '.column-3') %>%
                 html_text() %>%
                 {if(length(.) == 0){NA} else {.}}))

snowpack_data <- snowpack_data[-1,] %>%
  mutate(Year = as.factor(Year),
         April_1 = as.numeric(April_1),
         May_1 = as.numeric(May_1),
         June_1 = as.numeric(June_1))

# Combine Datasets
wser_dataset <- wser_results_sb %>% 
  left_join(temp_data, by = "Year") %>%
  left_join(snowpack_data, by = "Year") 