###### Western States Finish Rate Data


### taken from www.wser.org

# libraries
library(tidyverse)
library(rvest)
library(lubridate)


# use rvest to read in data
  website <- paste0('https://www.wser.org/results/')
  wser_finish <- read_html(website) %>% 
    html_nodes('#content') %>%
    map_df(~list(Year = html_nodes(.x,'.column-1') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Starters = html_nodes(.x,'.column-2') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Finishers = html_nodes(.x, '.column-3') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Silver_Buckle = html_nodes(.x, '.column-4') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}},
                 Finish_Rate = html_nodes(.x, '.column-5') %>%
                   html_text() %>%
                   {if(length(.) == 0){NA} else {.}}))

  # Set numeric features to numeric  
  wser_finish <- wser_finish[-1,] %>%
                 mutate(Year = as.factor(Year),
                        Starters = as.numeric(Starters),
                        Finishers = as.numeric(Finishers),
                        Silver_Buckle = as.numeric(Silver_Buckle),
                        Bronze_Buckle = Finishers - Silver_Buckle,
                        Finish_Rate = as.numeric(Finish_Rate))
  
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
  
  # Combine finishing results, temp, and snowpack data
  
  # Combine Datasets
  wser_finish <- wser_finish %>% 
    left_join(temp_data, by = "Year") %>%
    left_join(snowpack_data, by = "Year") %>%
    mutate(Year = as.numeric(Year)) %>%
    filter(Year != 2008)
  