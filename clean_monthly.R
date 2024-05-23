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
  flextable,
  officer
)

#Importing and viewing data------------------------------------------------------------------------
library(readxl)
monthly_data <- read_excel("mlfdata.xlsx")
View(monthly_data)
glimpse(monthly_data)

#Data Cleaning----------------------------------------------------------------------------------
mdata <- monthly_data %>%
  clean_names() %>%
  
  rename( m_tempmax = tempmax,
          m_tempmin = tempmin, 
          m_temp = temp, 
          m_humid = humidity, 
          m_precip = precip, 
          m_precov = precipcover)

mdata$state <- as.factor(mdata$state)
mdata$year <- as.factor(mdata$year)
mdata$cases <- as.numeric(mdata$cases)
mdata$month <- as.factor(mdata$month)

glimpse(mdata)
View(mdata)

#Summarizing and Grouping data---------------------------------------------------------------- 
describe(mdata)
#determine mean of cases per state and year 
mean_cases <- mdata %>%
  group_by(state, year) %>%
  summarise(mean_cases = mean(cases)) %>%
  print()

#table of all variables for the whole period with mean and SD
mdata %>% select(!c(month, year)) %>% 
  tbl_summary(by = state, statistic = list 
  (all_continuous() ~ "{mean} ({sd})" ), digits = all_continuous() ~2) %>%
  add_p() %>% add_overall() 

# Round the values in the "cases" column to 1 decimal place
# mdata <-mdata %>%
#   mutate(cases = round(cases,1))
# print(mdata)
# 
# View(mdata)

#determine mean and standard deviation of climate variables  
mean_climate <- mdata  %>%
  group_by(state,year) %>%
  summarise( meanm_tempmin = mean(m_tempmin),
             meanm_temp = mean(m_temp),
             meanm_tempmax = mean(m_tempmax),
             meanm_precip = mean(m_precip),
             meanm_precov = mean(m_precov),
             meanm_humid = mean(m_humid))
print(mean_climate)

#Calculate standard deviations for all variables
standard_deviations <- mdata %>%
  group_by(state, year) %>%
  summarise(sd_cases = sd(cases),
            sd_m_tempmin = sd(m_tempmin),
            sd_m_temp = sd(m_temp),
            sd_m_tempmax = sd(m_tempmax),
            sd_m_humid = sd(m_humid),
            sd_m_precip = sd(m_precip),
            sd_m_precov = sd(m_precov))

print(standard_deviations)

#create table for mean and standard deviations

#Merge mean cases and mean climate, standard deviations 
monthmean_table <- mean_cases %>%
  left_join(mean_climate, by = c("state", "year")) %>%
  left_join(standard_deviations, by = c("state", "year"))
print(monthmean_table)

View(monthmean_table)


#Data Visualization-------------------------------------------------------------------------------------

#Determination of data distribution 

#cases
histogram <- mdata %>%
  ggplot(aes(x = cases)) + geom_histogram(bins = 10) +  facet_grid(state ~ year) +   
  labs(x = "Cases", y = "Frequency", title = "Cases by State and Year")
print(histogram)


#Visualization of epidemiological data 

# #bar chart of total number of cases per year by month
# ggplot(data = mdata, aes(x = month, y = cases)) +
#   geom_bar(stat = "identity", width = 0.5) +
#   facet_wrap(~ state) +
#   labs(x = "Month", y = "Cases", title = "Cases by Month")
# Create a bar chart with vertical month labels
# ggplot(data = mdata, aes(x = month, y = cases)) +
#   geom_bar(stat = "identity", width = 0.5) +
#   facet_wrap(~ state) +
#   labs(x = "Month", y = "Cases", title = "Cases by Month") +
#   theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

# Create a new variable combining month and year
mdata$month_year <- with(mdata, interaction(month, year, drop = TRUE))

# Reorder the levels of month_year
mdata$month_year <- factor(mdata$month_year, levels = unique(mdata$month_year))

# Create a bar chart with different colors for each year
ggplot(data = mdata, aes(x = month_year, y = cases, fill = as.factor(year))) +
  geom_bar(stat = "identity", width = 0.5) +
  facet_wrap(~ state) +
  labs(x = "Month", y = "Cases", title = "Cases by Month and Year") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  scale_fill_discrete(name = "Year") + theme_bw()


# #side by side bar chart: Number of cases per month across states
# ggplot(data = mdata, aes(x = month, y = cases, fill = state))+ geom_bar (stat ="identity", position = "dodge") + 
#   facet_wrap(~ year) + labs(title = "Monthly Case count across States") 

# Create the plot with vertical month labels# Reorder the levels of the month variable
mdata$month <- factor(mdata$month, levels = month.name)

# Create the plot with vertical month labels
ggplot(data = mdata, aes(x = cases, y = month, fill = state)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ year) +
  labs(title = "Monthly Case Count Across States") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+theme_bw()

#line graph of cases by month across years

ggplot(data = mdata, aes(x = month, y = cases, group = interaction(state, year), color = year)) + 
  geom_line() + facet_wrap(~ state) + labs(x = "Month", y = "cases", title = "Cases by Month") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 


#Visualization of climate data 

#line graph of temperature by month across years
ggplot(data = mdata, aes(x = month, y = m_temp, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "temperature", title = "Temperature by Month") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#line graph of precipitation by month across years
ggplot(data = mdata, aes(x = month, y = m_precip, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "precipitation", title = "Precipitation by Month") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#line graph of humidity by month across years
ggplot(data = mdata, aes(x = month, y = m_humid, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "humidity", title = "Humidity by Month") +
  theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Assessing the relationship between variables----------------------------------------------------------------------------------------------

#Scatter plots (relationship between continuous variables)

#cases versus precipitation  
ggplot(data = mdata, aes(x = m_precip, y = cases, color = year)) + geom_point(size=3) +
  facet_wrap(~ state) +
  labs(x = "precipitation", y = "cases", title = "Cases against Precipitation by State") +
  geom_smooth(method =lm, se = F) + theme_bw()

#cases versus precipitation cover  
ggplot(data = mdata, aes(x = m_precov, y = cases, color = year)) + geom_point() +
  facet_wrap(~ state) +
  labs(x = "precipitation cover", y = "cases", title = "Cases against Precipitation cover by State") +
  geom_smooth(method =lm, se = F) + theme_bw()

#cases versus humidity  
ggplot(data = mdata, aes(x = m_humid, y = cases, color = year)) + geom_point() +
  facet_wrap(~ state) +
  labs(x = "humidity", y = "cases", title = "Cases against humidity by State") +
  geom_smooth(method =lm, se = F) + theme_bw()

#cases versus temperature  
ggplot(data = mdata, aes(x = m_temp, y = cases, color = year)) + geom_point() +
  facet_wrap(~ state) +
  labs(x = "temperature", y = "cases", title = "Cases against Temperature by State") +
  geom_smooth(method =lm, se = F) + theme_bw()

#cases versus minimum temperature
ggplot(data = mdata, aes(x = m_tempmin, y = cases, color = year)) + geom_point() +
  facet_wrap(~ state) +
  labs(x = "minimum temperature", y = "cases", title = "Cases against Minimum Temperature by year") +
  geom_smooth(method =lm, se = F) + theme_bw()

#cases versus maximum temperature
ggplot(data = mdata, aes(x = m_tempmax, y = cases, color = year)) + geom_point() +
  facet_wrap(~ state) +
  labs(x = "maximum temperature", y = "cases", title = "Cases against Maximum Temperature by year")+
  geom_smooth(method =lm, se = F) + theme_bw()


#Correlation analysis---------------------------------------------------------------------------------------------

#Spearman's correlation for direction and magnitude of relationship (data does not have a normal distribution

#Creating a function that will return correlations for each group
calculate_group_correlations <- function(mdata) {
  correlations <- mdata %>%
    select(cases, m_tempmin, m_temp, m_tempmax, m_humid, m_precip, m_precov) %>%
    correlate(method = "spearman")
  return(correlations)
}
#applying the function to create a grouped correlation table by state and year
correlation_table <- mdata %>%
  group_by(state, year) %>%
  do(calculate_group_correlations(.) )
  
print(correlation_table)


#test code###################
calculate_group_correlations <- function(mdata) {
  correlations <- mdata %>%
    select(cases, m_tempmin, m_temp, m_tempmax, m_humid, m_precip, m_precov) %>%
    correlate(method = "spearman")
  
  # Calculate p-values for correlation coefficients
  p_values <- sapply(correlations$correlation, function(correlation_coefficient) {
    cor_test <- cor.test(correlation_coefficient)
    return(cor_test$p.value)
  })
  
  # Combine correlation coefficients and p-values into a data frame
  result <- data.frame(correlation = correlations$correlation, p_value = p_values)
  
  return(result)
}

# Apply the function to create a grouped correlation table by state and year
correlation_table <- mdata %>%
  group_by(state, year) %>%
  do({
    correlations <- calculate_group_correlations(.data)
    correlations
  })

# Print correlation table with correlation coefficients and p-values
print(correlation_table)


View(correlation_table)

#test code for autocorrealtion
mdata %>% 
  ## keep the variables we are interested 
  select(month, cases, m_temp) %>% 
  ## change your data in to long format
  pivot_longer(
    ## use epiweek as your key
    !month,
    ## move column names to the new "measure" column
    names_to = "measure", 
    ## move cell values to the new "values" column
    values_to = "value") %>% 
  ## create a plot with the dataset above
  ## plot epiweek on the x axis and values (counts/celsius) on the y 
  ggplot(aes(x = month, y = value)) + 
  ## create a separate plot for temperate and case counts 
  ## let them set their own y-axes
  facet_grid(measure ~ ., scales = "free_y") +
  ## plot both as a line
  geom_point() + geom_line()


#end of test code#########

# Apply the function to create a grouped correlation table by state and year
correlation_table <- mdata %>%
  group_by(state, year) %>%
  do(calculate_group_correlations(.))
  
# Print correlation table with correlation coefficients and p-values
print(correlation_table)

# Print correlation table with correlation coefficients and p-values
print(correlation_table)


str(correlation_table)


View (correlation_table)



glimpse(mdata)
#applying the function to create a grouped correlation table by state only
correlation_table2 <- mdata %>%
  group_by(state) %>%
  do(calculate_group_correlations(.))
print(correlation_table2)

View (correlation_table2)


#Correlation table for ungrouped data  
correlation_table <- mdata %>% 
  ungroup()

correlation_results <- correlation_table %>% 
  select(cases, m_tempmin, m_temp, m_tempmax, m_humid, m_precip, m_precov) %>%   
  correlate(method = "spearman")    
print(correlation_results)

View(correlation_results)
--------------------------------------------------------------------------------------------------
  
  tbl_strata <-
  mdata %>% select(!c(month)) %>%
  mutate(year = paste("Year", year)) %>%
  tbl_strata(
    strata= year,
    ~tbl_summary(.x, by = state, statistic = all_continuous() ~ "{mean} ({sd}")%>%
      add_p() %>% 
      modify_header(all_stat_cols() ~ "**{level}**"),
   theme_gtsummary_compact() )
print(tbl_strata)

ggsave("tbl_strata.png")
doc <- read_docx()
doc <- doc %>%
  body_add_img(src= "tbl_strata.png", width = 6, height = 4, style = "centered" )
print(doc, target = "plot_document.docx")










 