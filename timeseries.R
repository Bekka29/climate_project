#Installing package required for installing libraries
install.packages("pacman")
install.packages("kableExtra")

#Install libraries for data transformation
pacman::p_load(
  rio,
  here,
  dplyr,
  epikit,
  forcats,
  readr,
  styler,
  tidyverse,
  janitor,
  ggplot2,
  corrr,
  gtsummary,
  broom,
  kableExtra,
  corrplot,
  Hmisc,
  feasts,
  forecast,
  tsibble
  
)

#Importing and viewing data------------------------------------------------------------------------
library(readxl)
timeseries <- read_excel("monthlydate_data.xlsx")
View(timeseries)
glimpse(timeseries)

#Data Cleaning----------------------------------------------------------------------------------
timeseries <- timeseries %>%
  clean_names() %>%
  
  rename( m_tempmax = tempmax,
          m_tempmin = tempmin, 
          m_temp = temp, 
          m_humid = humidity, 
          m_precip = precip, 
          m_precov = precipcover)

timeseries$state <- as.factor(timeseries$state)
timeseries$date <- as.Date(timeseries$date, format = "%Y-%m-%d")
timeseries$cases <- as.numeric(timeseries$cases)


glimpse(timeseries)
View(timeseries)

describe(ts_data)


timeseries %>% 
  ## keep the variables we are interested 
  select(date, cases, m_humid) %>% 
  ## change your data in to long format
  pivot_longer(
    ## use epiweek as your key
    !date,
    ## move column names to the new "measure" column
    names_to = "measure", 
    ## move cell values to the new "values" column
    values_to = "value") %>% 
  ## create a plot with the dataset above
  ## plot epiweek on the x axis and values (counts/celsius) on the y 
  ggplot(aes(x = date, y = value)) + 
  ## create a separate plot for temperate and case counts 
  ## let them set their own y-axes
  facet_grid(measure ~ ., scales = "free_y") +
  ## plot both as a line
  geom_line()

timeseries %>% 
  ## keep the variables we are interested 
  select(date, cases, m_precip) %>% 
  ## change your data in to long format
  pivot_longer(
    ## use epiweek as your key
    !date,
    ## move column names to the new "measure" column
    names_to = "measure", 
    ## move cell values to the new "values" column
    values_to = "value") %>% 
  ## create a plot with the dataset above
  ## plot epiweek on the x axis and values (counts/celsius) on the y 
  ggplot(aes(x = date, y = value)) + 
  ## create a separate plot for temperate and case counts 
  ## let them set their own y-axes
  facet_grid(measure ~ ., scales = "free_y") +
  ## plot both as a line
  geom_line() + theme_bw()


