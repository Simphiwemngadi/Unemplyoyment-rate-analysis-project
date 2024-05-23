#Part two
library(tidyverse)
library(ggplot2)
library(summarytools)
set.seed(04)
load("qlfs.RData") #load qlfs data file

nrow(qlfs)#number of rows
ncol(qlfs) #numberof columns


#Part three

set.seed(04)
# finding the `10% rows of the data
qlfs_sample <- qlfs %>%
  group_by(QDate) %>%
  slice_sample(prop = 0.1, replace = FALSE)
qlfs_sample

# summary responses for each of the 40 Quaters
summary_responses <- qlfs_sample %>%
  count(QDate)
print(summary_responses)

#Part four

# Drop the missing values in Variable Province and NEET
qlfs_sample<-qlfs_sample|>
  drop_na(PROVINCE,NEET)
# Relative frequency table of NEET variable
qlfs_sample |>
  group_by(PROVINCE,NEET)|>
  summarise(neet_count= n())|>
  mutate(relative_frequency = (neet_count /sum(neet_count) ) * 100)|>
  filter(NEET=="Yes")->freq_neet



# Part five
# drop the missing values in Q15POPULATION and STATUS variables
data_filtered<-qlfs_sample %>%
  drop_na(Q15POPULATION,STATUS)
# show the stacked bar graph of Q15POPULATION and STATUS
ggplot(data_filtered, aes(x = Q15POPULATION, fill = STATUS)) +
  geom_bar(position = "stack") +
  labs(
    x = "Q15POPULATION", # X-axis label
    y = "Counts", # Y-axis label
    title = "Stacked Bar Graph of Q15POPULATION by STATUS",
    caption = "Source: Statistics South Africa"
  ) +
  theme_minimal() +
  theme(legend.position = "right")

#Part six

#drop missing values in status
qlfs_sample <- qlfs_sample|>
  drop_na(STATUS)
#frequency table of Status
qlfs_sample |>filter(STATUS %in% c("Employed", "Unemployed"))|>
  group_by(QDate,STATUS)|>
  summarize(Count = n()) |>
  mutate(rate = (Count / sum(Count)) * 100)|>
  filter(STATUS == "Unemployed")->freq_unemployed 


#graph of unemployment rate over time
ggplot(freq_unemployed)+
  geom_line(mapping=aes(x=QDate,y=rate,
                        color="blue"))+
  geom_point(mapping=aes(x=QDate,y=rate,color="blue",size=2))+
  
  scale_x_date(date_breaks = "3 months", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(
    x = "Quarterly Period (QDate)",
    y = "Unemployment Rate (%)",
    title = "Unemployment Rate Over Time",
    caption = "Source:
Statistics South Africa")

  
#Part seven

# unemployment rate per quarter
qlfs_sample %>%
  group_by(QDate, QUARTER, STATUS) %>%
  summarise(Frequency = n()) %>%
  mutate(Percent = (Frequency / sum(Frequency)) * 100) %>%
  filter(STATUS == "Unemployed")->qlfs_rate

library(readxl)
inflat <- read_excel("inflation.xlsx") # reading an inflation data from excel
#converting quarter from being an integer to factor
qlfs_rate$QUARTER <- as.factor(qlfs_rate$QUARTER)
qlfs_rate

# combine two variables from different data sets
joined_data <- left_join(qlfs_rate, inflat, by = c("QDate" = "Date"))
joined_data

# scatter plot showing the correlation between Annualized inflation rate and
Unemployment rate
plot <- ggplot(joined_data, aes(x = Annualised_Inflation, y = Percent, shape
                                = QUARTER,color=QUARTER)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm",se=FALSE)+
  # Set axis labels and a title
  labs(
    x = "Annualised Inflation Rate",
    y = "Unemployment Rate",
    title = "Scatter Plot of Annualized Inflation Rate vs. Unemployment Rate"
  )
plot


#Part eight

# filtering data with both Quarter 1 and Year 2015
qlfs_sample|>
  
  filter(QUARTER==1 & year(QDate)==2015)|>
  select(matches("^Q419.*WRK$"))->dat
## Adding missing grouping variables: `QDate`
dat

#pivot data to put all selected columns in one column called Hours
pivoted_data <- dat %>%
  pivot_longer(cols = starts_with("Q419"), names_to = "WEEKDAY", values_to =
                 "HOURS")
pivoted_data

# mean Hours per weekday
mean_hours_per_weekday <- pivoted_data %>%
  group_by(WEEKDAY) %>%
  summarise(mean_hours = mean(HOURS, na.rm = TRUE)) %>%
  arrange(desc(mean_hours))
mean_hours_per_weekday

#Part nine

# histogram plot shows satisfaction per age group
histogram_plot <- ggplot(qlfs_sample, aes(x = Q14AGE, fill = Q416ASATISFIED))
+
  geom_histogram(binwidth = 5, color = "black") +
  
  labs(
    x = "Age",
    fill = "Satisfaction",
    title = "Age Distribution by Satisfaction")
histogram_plot


#question10

# Preparing data for logistic regression
library(forcats)
qlfs_sample|>filter(STATUS %in% c("Employed", "Unemployed"))|>
  filter(Q14AGE >= 15 & Q14AGE <= 64)|>
  mutate(STATUS=fct_relevel( "Unemployed"))->dat3
dat3

dat3$STATUS |>
  fct_relevel("Unemployed")->dat3$STATUS #Unemployed as reference level of
STATUS
dat3$EDUCATION_STATUS|>fct_lump( prop = 0.05, other_level = "Other")->dat3$EDUCATION_STATUS
dat3$GEO_TYPE|>fct_lump( n = 1, other_level = "other")->dat3$GEO_TYPE
dat3$GEO_TYPE|>fct_relevel("Urban")->dat3$GEO_TYPE
dat3$Q13GENDER|>fct_relevel("Female")->dat3$Q13GENDER
dat3$PROVINCE|>fct_relevel("Western Cape")->dat3$PROVINCE
dat3|>
  group_by(QUARTER,QDate)


periods <- c("Q3 2013", "Q4 2013", "Q1 2014", "Q2 2014", "Q3 2014", "Q4
2014", "Q1 2015", "Q2 2015", "Q3 2015", "Q4 2015",
             "Q1 2016", "Q2 2016", "Q3 2016", "Q4 2016", "Q1 2017", "Q2
2017", "Q3 2017", "Q4 2017", "Q1 2018", "Q2 2018",
             "Q3 2018", "Q4 2018", "Q1 2019", "Q2 2019", "Q3 2019", "Q4
2019", "Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020",
             "Q1 2021", "Q2 2021", "Q3 2021", "Q4 2021", "Q1 2022", "Q2
2022", "Q3 2022", "Q4 2022", "Q1 2023", "Q2 2023")
# Convert QDATE to Date format
library(lubridate)
dat3$QDate <- as.Date(dat3$QDate, format = "%Y-%m-%d")
# Extract year and quarter from QDATE
dat3$YEAR <- lubridate::year(dat3$QDate)
dat3$QUARTER <- lubridate::quarter(dat3$QDate)
# Create PERIOD variable using YEAR and QUARTER
dat3$PERIOD <- match(paste0("Q", dat3$QUARTER, " ", dat3$YEAR), periods)
library(broom)
library(glm2)

logistic_model <- glm(STATUS ~ Q14AGE + EDUCATION_STATUS + Q13GENDER +
                        PROVINCE +GEO_TYPE + PERIOD, data = dat3, family = "binomial")

tidy_output <- tidy(logistic_model)
print(tidy_output)



  