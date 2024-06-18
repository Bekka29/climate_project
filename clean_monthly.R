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
monthly_data <- read_excel("monthlydata.xlsx")
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
#mdata$month <- as.factor(mdata$month)
mdata$month <- factor(mdata$month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))

glimpse(mdata)
View(mdata)


#Summary Statistics---------------------------------------------------------------- 
describe(mdata)

#determine annual case count by state
mdata_summary <- mdata %>%
  group_by(state, year) %>%
  dplyr::summarize(total_cases = sum(cases))
print(mdata_summary)

#determine mean and standard deviation of all variables by state and year  
mean_data <- mdata  %>%
  group_by(state,year) %>%
  summarise( meanm_tempmin = mean(m_tempmin),
             meanm_temp = mean(m_temp),
             meanm_tempmax = mean(m_tempmax),
             meanm_precip = mean(m_precip),
             meanm_precov = mean(m_precov),
             meanm_humid = mean(m_humid),
             mean_cases = mean(cases))
print(mean_data)

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



#Table of annual means of variables grouped by state with p-values
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


#table of all variables for the whole period with mean and SD by state only
mdata %>% select(!c(month, year)) %>% 
  tbl_summary(by = state, statistic = list 
              (all_continuous() ~ "{mean} ({sd})" ), digits = all_continuous() ~2) %>%
  add_p() 



#Data Visualization-------------------------------------------------------------------------------------

                          #Determination of data distribution 

#cases
histogram <- mdata %>%
  ggplot(aes(x = cases)) + geom_histogram(bins = 10) +  facet_grid(state ~ year) +   
  labs(x = "Cases", y = "Frequency", title = "Cases by State and Year")
print(histogram)
                          

                          #Visualization of Epidemiological Data 

#Bar plot of annual case count by state and year
ggplot(mdata_summary, aes(x = factor(year), y = total_cases, fill = state)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Total Case Count by State and Year", x = "Year", y = "Total Cases") +
  theme_minimal() + scale_fill_brewer(palette = "Dark2")


# Bar chart with vertical month labels
ggplot(data = mdata, aes(x = cases, y = month, fill = state)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ year) +
  labs(title = "Monthly Case Count across States") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+theme_bw() +
  theme_bw() + scale_fill_brewer(palette = "Dark2")

#line graph of cases by month across years
ggplot(data = mdata, aes(x = month, y = cases, group = interaction(state, year), color = year)) + 
  geom_line() + facet_wrap(~ state) + labs(x = "Month", y = "cases", title = "Monthly Case count across years") + 
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



                          #Visualization of Climate Data 

#line graph of temperature by month across years
ggplot(data = mdata, aes(x = month, y = m_temp, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "temperature", title = "Monthly Average Temperature by State") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#line graph of maximum temperature by month across years
ggplot(data = mdata, aes(x = month, y = m_tempmax, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "temperature", title = " Monthly Maximum Temperature by State") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#line graph of minimum temperature by month across years
ggplot(data = mdata, aes(x = month, y = m_tempmin, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "temperature", title = "Monthly Minimum Temperature by State") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#line graph of precipitation by month across years
ggplot(data = mdata, aes(x = month, y = m_precip, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "precipitation", title = "Monthly Precipitation by State") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#line graph of precipitation cover by month across years
ggplot(data = mdata, aes(x = month, y = m_precov, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "precipitation", title = "Monthly Precipitation cover by State") +
  theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#line graph of humidity by month across years
ggplot(data = mdata, aes(x = month, y = m_humid, group = interaction(state, year), color = year)) + 
  geom_line() + geom_point() + facet_wrap(~ state) + labs(x = "Month", y = "humidity", title = "Monthly Humidity by State") +
  theme_bw() +theme(axis.text.x = element_text(angle = 45, hjust = 1)) 



#Relationship between Variables----------------------------------------------------------------------------------------------

                     #Scatter plots between climate and epidemiological variables

#cases versus precipitation  
ggplot(data = mdata, aes(x = m_precip, y = cases, color = year)) + geom_point(size=3) +
  facet_wrap(~ state) +
  labs(x = "precipitation", y = "cases", title = "Monthly Case count against Precipitation") +
  geom_smooth(method =lm, se = F) + theme_bw() + scale_color_brewer(palette = "Dark2")

#cases versus precipitation cover  
ggplot(data = mdata, aes(x = m_precov, y = cases, color = year)) + geom_point(size=3) +
  facet_wrap(~ state) +
  labs(x = "precipitation cover", y = "cases", title = "Monthly Case count against Precipitation Cover") +
  geom_smooth(method =lm, se = F) + theme_bw() + scale_color_brewer(palette = "Dark2")

#cases versus humidity  
ggplot(data = mdata, aes(x = m_humid, y = cases, color = year)) + geom_point(size=3) +
  facet_wrap(~ state) +
  labs(x = "humidity", y = "cases", title = "Monthly Case count against Humidity") +
  geom_smooth(method =lm, se = F) + theme_bw() + scale_color_brewer(palette = "Dark2")

#cases versus temperature  
ggplot(data = mdata, aes(x = m_temp, y = cases, color = year)) + geom_point(size=3) +
  facet_wrap(~ state) +
  labs(x = "temperature", y = "cases", title = "Monthly Case count against Average Temperature") +
  geom_smooth(method =lm, se = F) + theme_bw() + scale_color_brewer(palette = "Dark2")

#cases versus minimum temperature
ggplot(data = mdata, aes(x = m_tempmin, y = cases, color = year)) + geom_point(size=3) +
  facet_wrap(~ state) +
  labs(x = "minimum temperature", y = "cases", title = "Monthly Case count against Minimum Temperature") +
  geom_smooth(method =lm, se = F) + theme_bw() + scale_color_brewer(palette = "Dark2")

#cases versus maximum temperature
ggplot(data = mdata, aes(x = m_tempmax, y = cases, color = year)) + geom_point(size=3) +
  facet_wrap(~ state) +
  labs(x = "maximum temperature", y = "cases", title = "Monthly Case count against Maximum Temperature")+
  geom_smooth(method =lm, se = F) + theme_bw() + scale_color_brewer(palette = "Dark2")



#Correlation Analysis---------------------------------------------------------------------------------------------

            #Spearman's correlation for direction and magnitude of relationship (data does not have a normal distribution)


#Correlation table for data grouped by state and year WITHOUT p-values

#Funtion to return correlations for each group
calculate_group_correlations <- function(mdata) {
  correlations <- mdata %>%
    select(cases, m_tempmin, m_temp, m_tempmax, m_humid, m_precip, m_precov) %>%
    correlate(method = "spearman")
  return(correlations)
}
#apply function
correlation_table <- mdata %>%
  group_by(state, year) %>%
  do(calculate_group_correlations(.) )
  
print(correlation_table)


#######################################################
#correlation table for data grouped by BOTH state and year WITH p-values
calculate_group_correlations <- function(mdata) {
  # Select relevant columns
  selected_data <- mdata %>%
    select(cases, m_tempmin, m_temp, m_tempmax, m_humid, m_precip, m_precov)
  
  # Calculate Spearman correlations with p-values using rcorr
  correlation_matrix <- rcorr(as.matrix(selected_data), type = "spearman")
  
  # Extract correlation coefficients and p-values
  correlations <- as.data.frame(correlation_matrix$r)
  p_values <- as.data.frame(correlation_matrix$P)
  
  # Reshape data from wide to long format
  correlations_long <- correlations %>%
    rownames_to_column(var = "Variable1") %>%
    gather(key = "Variable2", value = "Correlation", -Variable1)
  
  p_values_long <- p_values %>%
    rownames_to_column(var = "Variable1") %>%
    gather(key = "Variable2", value = "P_Value", -Variable1)
  
  # Merge correlations and p-values
  results <- merge(correlations_long, p_values_long, by = c("Variable1", "Variable2"))
  
  return(results)
}

correlation_table1 <- mdata %>%
  group_by(state, year) %>%
  do(calculate_group_correlations(.))


print(correlation_table1)
View(correlation_table1)
################################################################

#correlation table for data grouped by state ONLY with p-values

# Function to calculate correlations with p-values
calculate_group_correlations <- function(mdata) {
  # Select relevant columns
  selected_data <- mdata %>%
    select(cases, m_tempmin, m_temp, m_tempmax, m_humid, m_precip, m_precov)
  
  # Calculate Spearman correlations with p-values using rcorr
  correlation_matrix <- rcorr(as.matrix(selected_data), type = "spearman")
  
  # Extract correlation coefficients and p-values
  correlations <- as.data.frame(correlation_matrix$r)
  p_values <- as.data.frame(correlation_matrix$P)
  
  # Reshape data from wide to long format
  correlations_long <- correlations %>%
    rownames_to_column(var = "Variable1") %>%
    pivot_longer(-Variable1, names_to = "Variable2", values_to = "Correlation")
  
  p_values_long <- p_values %>%
    rownames_to_column(var = "Variable1") %>%
    pivot_longer(-Variable1, names_to = "Variable2", values_to = "P_Value")
  
  # Merge correlations and p-values
  results <- merge(correlations_long, p_values_long, by = c("Variable1", "Variable2"))
  
  return(results)
}

# Applying the function to create a grouped correlation table by state
correlation_table2 <- mdata %>%
  group_by(state) %>%
  do(calculate_group_correlations(.)) %>%
  ungroup()

# Print and view the correlation table
print(correlation_table2)
View(correlation_table2)

###################################################

#Correlation table for ungrouped data  
correlation_tab <- mdata %>% 
  ungroup()

correlation_results4 <- correlation_tab %>% 
  select(cases, m_tempmin, m_temp, m_tempmax, m_humid, m_precip, m_precov) %>%   
  correlate(method = "spearman")    
print(correlation_results4)

View(correlation_results4)






 