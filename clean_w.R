#Installing package required for installing libraries
install.packages("pacman")

#Install libraries for data transformation
pacman::p_load(
  rio,
  here,
  dplyr,
  epikit,
  forcats,
  lubridate,
  readr,
  styler,
  tidyverse,
  janitor,
  ggplot2,
  corrr
)

#Importing and viewing data------------------------------------------------------------------------
library(readxl)
tdata <- read_excel("weeklydata.xlsx")
View(tdata)
class(tdata)

#Data Cleaning----------------------------------------------------------------------------------
 tdata <- tdata %>%
   
  clean_names() %>%
   
 rename( w_tempmax = tempmaxw,
           w_tempmin = tempminw, 
           w_temp = tempw, 
           w_humid = humidityw, 
           w_precip = precipw, 
           w_precov = precipcovw)
 
 tdata$state <- as.factor(tdata$state)
 tdata$epiweek <- as.factor(tdata$epiweek)
 tdata$year <- as.factor(tdata$year)
 tdata$cases <- as.numeric(tdata$cases)
 
 glimpse(tdata)
View(tdata)

#Summarizing and Grouping data---------------------------------------------------------------- 

#determine mean and standard deviation of climate variables  
mean_data <- tdata  %>%
  group_by(state,year) %>%
  summarise( meanw_tempmin = mean(w_tempmin),
             meanw_temp = mean(w_temp),
             meanw_tempmax = mean(w_tempmax),
             meanw_precip = mean(w_precip),
             meanw_precov = mean(w_precov),
             meanw_humid = mean(w_humid),
             mean_cases = mean(cases))
print(mean_data)

#Calculate standard deviations for all variables
standard_deviations <- tdata %>%
  group_by(state, year) %>%
  summarise(sd_cases = sd(cases),
            sd_w_tempmin = sd(w_tempmin),
            sd_w_temp = sd(w_temp),
            sd_w_tempmax = sd(w_tempmax),
            sd_w_humid = sd(w_humid),
            sd_w_precip = sd(w_precip),
            sd_w_precov = sd(w_precov))

print(standard_deviations)



# Merge standard deviations with the previously merged table
  merged_table <- left_join(mean_data, standard_deviations, by = c("state", "year"))
  print(merged_table)

View(merged_table)

#Data Visualization-------------------------------------------------------------------------------------
     
            #Determination of data distribution 

#cases
histogram <- tdata %>%
  ggplot(aes(x = cases)) + geom_histogram(bins = 10) +  facet_grid(state ~ year) +   
  labs(x = "Cases", y = "Frequency", title = "Cases by State and Year")
print(histogram)


histogram <- tdata %>%
  ggplot(aes(x = w_precip)) + geom_histogram(bins = 10) +  facet_grid(state ~ year) +   
  labs(x = "precipitation", y = "Frequency", title = "Precipitation by State and Year")
print(histogram)


              #Visualization of epidemiological data 

#bar chart of total number of cases per year by epiweek
  ggplot(data = tdata, aes(x = epiweek, y = cases)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ state) +
    labs(x = "Epiweek", y = "Cases", title = "Cases by Epiweek")

#side by side bar chart: Number of cases per epiweek across states
  ggplot(data = tdata, aes(x = epiweek, y = cases, fill = state))+ geom_bar (stat ="identity", position = "dodge") + 
    facet_wrap(~ year) + labs(title = "Weekly Case count across States") 
  
#line graph of cases by epiweek across years
  ggplot(data = tdata, aes(x = epiweek, y = cases, group = interaction(state, year), color = year)) + 
    geom_line() + facet_wrap(~ state) + labs(x = "Epiweek", y = "cases", title = "Cases by Epiweek") +
    theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) 
  
#line graph of cases by epiweek across years  
  epiweek_levels <- as.numeric(levels(tdata$epiweek))
  #Determine breaks (every 4th epiweek)
  breaks <- epiweek_levels[seq(1, length(epiweek_levels), by = 3)]
  
  ggplot(data = tdata, aes(x = epiweek, y = cases, group = interaction(state, year), color = year)) + 
    geom_line() + 
    facet_wrap(~ state) + 
    labs(x = "Epiweek", y = "cases", title = "Case count by Epiweek across years") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    scale_x_discrete(breaks = breaks)
  
  
#side by side barchart of cases per epiweek
  
  #Determine breaks (every 4th epiweek)
  breaks <- epiweek_levels[seq(1, length(epiweek_levels), by = 3)]
  
  # barchart with vertical month labels
  ggplot(data = tdata, aes(x = epiweek, y = cases, fill = state)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~ year) +
    labs(title = "Case Count by Epiweek") +
    theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust = 1))+theme_bw() +
    scale_x_discrete(breaks = breaks) + scale_fill_discrete(name = "Year") + theme_bw() + 
    scale_fill_brewer(palette = "Set1") 
  
  
          #Visualization of climate data 
  
#line graph of temperature by epiweek across years
  ggplot(data = tdata, aes(x = epiweek, y = w_temp, group = interaction(state, year), color = year)) + 
    geom_line() + facet_wrap(~ state) + labs(x = "Epiweek", y = "temperature", title = "Temperature by Epiweek")
  
#line graph of precipitation by epiweek across years
  ggplot(data = tdata, aes(x = epiweek, y = w_precip, group = interaction(state, year), color = year)) + 
    geom_line() + facet_wrap(~ state) + labs(x = "Epiweek", y = "precipitation", title = "Precipitation by Epiweek") 
  
#line graph of humidity by epiweek across years
  ggplot(data = tdata, aes(x = epiweek, y = w_humid, group = interaction(state, year), color = year)) + 
    geom_line() + facet_wrap(~ state) + labs(x = "Epiweek", y = "humidity", title = "Humidity by Epiweek")
  
#Assessing the relationship between variables----------------------------------------------------------------------------------------------

#Scatter plots (relationship between continuous variables)

  #cases versus precipitation  
  ggplot(data = tdata, aes(x = w_precip, y = cases, color = year)) + geom_point(size=2) +
    facet_wrap(~ state) +
    labs(x = "precipitation", y = "cases", title = "Cases against Precipitation by State") +
    geom_smooth(method =lm, se = F) + theme_bw()
  
  
  #cases versus precipitation cover  
  ggplot(data = tdata, aes(x = w_precov, y = cases, color = year)) + geom_point(size=2) +
    facet_wrap(~ state) +
    labs(x = "precipitation cover", y = "cases", title = "Cases against Precipitation cover by State") +
    geom_smooth(method =lm, se = F) + theme_bw()
  
  #cases versus humidity  
  ggplot(data = tdata, aes(x = w_humid, y = cases, color = year)) + geom_point(size=2) +
    facet_wrap(~ state) +
    labs(x = "humidity", y = "cases", title = "Cases against humidity by State")+
    geom_smooth(method =lm, se = F) + theme_bw()
  
  #cases versus temperature  
  ggplot(data = tdata, aes(x = w_temp, y = cases, color = year)) + geom_point(size=2) +
    facet_wrap(~ state) +
    labs(x = "temperature", y = "cases", title = "Cases against Temperature by State")+
    geom_smooth(method =lm, se = F) + theme_bw()
  
  #cases versus minimum temperature
  ggplot(data = tdata, aes(x = w_tempmin, y = cases, color = state)) + geom_point(size=2) +
    facet_wrap(~ year) +
    labs(x = "minimum temperature", y = "cases", title = "Cases against Minimum Temperature by year")+
    geom_smooth(method =lm, se = F) + theme_bw()
 
   #cases versus maximum temperature
  ggplot(data = tdata, aes(x = w_tempmax, y = cases, color = state)) + geom_point(size=2) +
    facet_wrap(~ year) +
    labs(x = "maximum temperature", y = "cases", title = "Cases against Maximum Temperature by year")+
    geom_smooth(method =lm, se = F) + theme_bw()

#Correlation analysis---------------------------------------------------------------------------------------------
  
#Spearman's correlation for direction and magnitude of relationship (data does not have a normal distribution

#correlation table grouped by state and year
 #Function to return correlations for each group
  calculate_group_correlations <- function(tdata) {
    correlations <- tdata %>%
      select(cases, w_tempmin, w_temp, w_tempmax, w_humid, w_precip, w_precov) %>%
      correlate(method = "spearman")
    return(correlations)
  }
#applying the function
  correlation_tab <- tdata %>%
    group_by(state, year) %>%
    do(calculate_group_correlations(.))
   print(correlation_tab)
  
View (correlation_tab)
  
#Correlation table for data grouped by state  
  
calculate_group_correlations <- function(tdata) {
  correlations <- tdata %>%
    select(cases, w_tempmin, w_temp, w_tempmax, w_humid, w_precip, w_precov) %>%
    correlate(method = "spearman")
  return(correlations)
}

correlation_tab <- tdata %>%
  group_by(state) %>%
  do(calculate_group_correlations(.))
print(correlation_tab)

View (correlation_tab)
   
