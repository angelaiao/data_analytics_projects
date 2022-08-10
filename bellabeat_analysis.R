install.packages(c("tidyverse", "dplyr", "tidyr", "janitor", "readr", "skimr", "ggplot2"))

library(lubridate)
library(dplyr)
library(tidyr)
library(readr)
library(janitor)

# Importing CSV files
dailyActivity <- read_csv("dailyActivity_merged.csv")
#View(dailyActivity)

sleepDay <- read.csv("sleepDay_merged.csv")
#View(sleepDay)

dailySteps <- read_csv("dailySteps_merged.csv")
#View(dailySteps)


#generals to our data
head(dailyActivity)
skim_without_charts(dailyActivity) #no missing values
str(dailyActivity)

head(dailySteps)
skim_without_charts(dailySteps) #no missing values
str(dailySteps)

head(sleepDay)
skim_without_charts(sleepDay) #no missing values
str(sleepDay)


#Cleaning- formatting dates from string to date and correcting data types
#Cleaning- adding another column 'Date' for activity and sleep so that I can merge later
activity <- dailyActivity %>% 
  mutate(ActivityDate = mdy(ActivityDate)) %>% 
  mutate(TotalSteps = as.integer(TotalSteps)) %>% 
  mutate(Date = ActivityDate)

sleep <- sleepDay %>% 
  mutate(SleepDay = mdy_hms(SleepDay)) %>% 
  mutate(TotalMinutesAsleep = as.numeric(TotalMinutesAsleep)) %>% 
  mutate(TotalTimeInBed = as.numeric(TotalTimeInBed)) %>% 
  mutate(Date = SleepDay)

steps <- dailySteps %>% 
  mutate(ActivityDay = mdy(ActivityDay)) %>% 
  mutate(Date = ActivityDay)

activity %>% 
  select(TotalSteps, VeryActiveMinutes, FairlyActiveMinutes, 
         LightlyActiveMinutes, SedentaryMinutes) %>% 
  summary(activity)

sleep %>% 
  select(Date, TotalSleepRecords, TotalMinutesAsleep, TotalTimeInBed) %>% 
  summary()

steps %>% 
  select(StepTotal) %>% 
  summary()


#Exploring- distinct users in each data file
users_activity <- unique(dailyActivity$Id)
length(users_activity) #33

users_steps <- unique(dailySteps$Id)
length(users_steps) #33

users_sleep <- unique(sleepDay$Id)
length(users_sleep) #24


#Exploring- counting number of entries
nrow(sleep) #413
nrow(activity) #940

#Exploring- how many times each user logged in
sleepDay %>% count(Id)


#Merging activity and sleep in a df by 'Id' and 'Date', excluding columns I don't need
sleep_activity <- merge(sleep, activity, by = c('Id', 'Date')) %>% 
  select(-c(TotalDistance, TrackerDistance, LoggedActivitiesDistance, 
            VeryActiveDistance, ModeratelyActiveDistance, LightActiveDistance, SedentaryActiveDistance))
View(sleep_activity)

#filtering out the data that captured sedentary minutes of 1440 mins (24 hrs)
sleep_activity_cleaned <- sleep_activity %>% 
  filter(SedentaryMinutes != 1440)

#filtered out the data that was tracked when user was not using it
sleep_activity_cleaned <- sleep_activity %>% 
  filter(SedentaryMinutes != 1440)
summary(sleep_activity_cleaned)
View(sleep_activity_cleaned)


#On average, how many users are active? (at least 10k steps/day)
avg_steps <- sleep_activity_cleaned %>% 
  group_by(Id) %>% 
  summarise(AverageSteps = mean(TotalSteps, na.rm = TRUE))

active_users_steps <- filter(avg_steps, AverageSteps >= 10000)
count(active_users_steps) #5

active_users_steps_percent <- nrow(active_users_steps)/nrow(avg_steps)*100
print(active_users_steps_percent) #20.83%


#How many users are sedentary? (less than 5,000 steps)
sedentary_users_steps <- filter(avg_steps, AverageSteps < 5000)
count(sedentary_users_steps) #5

sedentary_users_steps_percent <- nrow(sedentary_users_steps)/nrow(avg_steps)*100
print(sedentary_users_steps_percent) #20.83%


#On average, how many users are healthy sleepers? (at least 420 mins/day)
sleeping time for each user
avg_sleep_minutes <- sleep_activity_cleaned %>% 
  group_by(Id) %>% 
  summarise(AverageSleepMinutes = mean(TotalMinutesAsleep, na.rm = TRUE))
summary(avg_sleep_minutes$AverageSleepMinutes) #377.6 or 6.29 hrs


healthy_sleepers <- filter(avg_sleep_minutes, AverageSleepMinutes >= 420)
count(healthy_sleepers) #12

healthy_sleepers_percent <- nrow(healthy_sleepers)/nrow(avg_sleep_minutes)*100
print(healthy_sleepers_percent) #50%


#create new columns in sleep_activity_cleaned df
sleep_activity_cleaned$TotalLoggedMinutes = sleep_activity_cleaned$TotalTimeInBed + 
  sleep_activity_cleaned$VeryActiveMinutes + 
  sleep_activity_cleaned$FairlyActiveMinutes + 
  sleep_activity_cleaned$LightlyActiveMinutes + 
  sleep_activity_cleaned$SedentaryMinutes

sleep_activity_cleaned$TotalActiveMinutes = sleep_activity_cleaned$VeryActiveMinutes + 
  sleep_activity_cleaned$FairlyActiveMinutes + 
  sleep_activity_cleaned$LightlyActiveMinutes


#creating new column to categorize the activity level of users based on steps and determine whether they had sufficient sleep
sleep_steps <- sleep_activity_cleaned %>% 
  mutate(StepsActivityLevel = case_when(
    TotalSteps < 5000 ~ "Inactive", 
    TotalSteps >= 5000 & TotalSteps < 8750 ~ "Lightly Active", 
    TotalSteps >= 8750 & TotalSteps < 12500 ~ "Fairly Active", 
    TotalSteps >= 12500 ~ "Very Active"
  ), SleepSufficiency = case_when(
    TotalMinutesAsleep < 420 ~ "Not Enough",
    TotalMinutesAsleep >= 420 ~ "Enough"
  ))


#Analyzing- correlation b/w SedentaryMinutes and TotalMinutesAsleep
cor(sleep_steps$SedentaryMinutes, sleep_steps$TotalMinutesAsleep) #-0.599394

#Analyzing- counting how many observations in EnoughSleep
sleep_steps %>% count(SleepSufficiency) #Enough- 231 observations; Not Enough- 182 observations


#Analyzing- creating new column to categorize the activity level (based on activity minutes)
mean(sleep_activity_cleaned$SedentaryMinutes) # 712.1695
mean(sleep_activity_cleaned$LightlyActiveMinutes) # 216.8547
mean(sleep_activity_cleaned$FairlyActiveMinutes) # 18.03874
mean(sleep_activity_cleaned$VeryActiveMinutes) # 25.18886

sleep_activity_mins <- sleep_steps %>% 
  group_by(Id) %>%
  mutate(AwakeInBed = TotalTimeInBed - TotalMinutesAsleep, 
         ActivityLevelMins = case_when(
           SedentaryMinutes > 712.1965 &
             LightlyActiveMinutes < 216.8547 & 
             FairlyActiveMinutes < 18.03874 &
             VeryActiveMinutes < 25.18886 ~ "Inactive",
           SedentaryMinutes < 712.1965 &
             LightlyActiveMinutes > 216.8547 & 
             FairlyActiveMinutes < 18.03874 &
             VeryActiveMinutes < 25.18886 ~ "Lightly Active",
           SedentaryMinutes < 712.1965 &
             LightlyActiveMinutes < 216.8547 & 
             FairlyActiveMinutes > 18.03874 &
             VeryActiveMinutes < 25.18886 ~ "Fairly Active",
           SedentaryMinutes < 712.1965 &
             LightlyActiveMinutes < 216.8547 &
             FairlyActiveMinutes < 18.03874 & 
             VeryActiveMinutes > 25.18886 ~ "Very Active"), 
         SleepSufficiency = case_when(
           TotalMinutesAsleep < 420 ~ "Not Enough",
           TotalMinutesAsleep >= 420 ~ "Enough")) %>% 
  drop_na()


#Analyzing- correlation b/w AwakeInBed and TotalMinutesAsleep
cor(sleep_time$AwakeInBed, sleep_time$TotalMinutesAsleep) #-0.1385993

#On average, how long does it take users to sleep?
mean(sleep_time$AwakeInBed) #38.83 mins


#Analyzing- creating new column to categorize the activity level (based on activity minutes)
mean(sleep_activity_cleaned$SedentaryMinutes)
mean(sleep_activity_cleaned$LightlyActiveMinutes)
mean(sleep_activity_cleaned$FairlyActiveMinutes)
mean(sleep_activity_cleaned$VeryActiveMinutes)

sleep_activity_mins <- sleep_steps %>% 
  group_by(Id) %>%
  mutate(AwakeInBed = TotalTimeInBed - TotalMinutesAsleep, 
         ActivityLevelMins = case_when(
           SedentaryMinutes > 712.1965 &
             LightlyActiveMinutes < 216.8547 & 
             FairlyActiveMinutes < 18.03874 &
             VeryActiveMinutes < 25.18886 ~ "Inactive",
           SedentaryMinutes < 712.1965 &
             LightlyActiveMinutes > 216.8547 & 
             FairlyActiveMinutes < 18.03874 &
             VeryActiveMinutes < 25.18886 ~ "Lightly Active",
           SedentaryMinutes < 712.1965 &
             LightlyActiveMinutes < 216.8547 & 
             FairlyActiveMinutes > 18.03874 &
             VeryActiveMinutes < 25.18886 ~ "Fairly Active",
           SedentaryMinutes < 712.1965 &
             LightlyActiveMinutes < 216.8547 &
             FairlyActiveMinutes < 18.03874 & 
             VeryActiveMinutes > 25.18886 ~ "Very Active"), 
         SleepSufficiency = case_when(
           TotalMinutesAsleep < 420 ~ "Not Enough",
           TotalMinutesAsleep >= 420 ~ "Enough")) %>% 
  drop_na()


#Analyzing- correlation b/w AwakeInBed and TotalMinutesAsleep
cor(sleep_activity_mins$AwakeInBed, sleep_activity_mins$TotalMinutesAsleep) #-0.1385993

#On average, how long does it take users to sleep?
mean(sleep_activity_mins$AwakeInBed) #38.83 mins


#Visualization- look at relationship b/w total steps and calories
library(ggplot2)

#Who is our target audience (the highest number of users) by activity level?
ggplot(data = sleep_steps, aes(StepsActivityLevel)) + 
  geom_bar(aes(fill = StepsActivityLevel)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust =  -0.5) +
  labs(title = 'Number of Observations by Activity Level (Steps)')

#Who is our target audience (the highest number of users) by activity level (Mins)?
ggplot(data = sleep_activity_mins, aes(ActivityLevelMins)) + 
  geom_bar(aes(fill = ActivityLevelMins)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust =  -0.5) +
  labs(title = 'Number of Observations by Activity Level (Mins)')

#Total Active Minutes vs Calories by Activity Level (Steps)
ggplot(sleep_steps, mapping = aes(
  x = TotalActiveMinutes, y = Calories, fill = StepsActivityLevel, color = StepsActivityLevel)) + 
  geom_point() +
  labs(title = "Total Active Minutes vs Calories by Activity Level (Steps)")

#Are users getting enough sleep?
#creating a df to get an aggregate of observations (n) and the percentage for sleep sufficiency
pie_chart <- sleep_steps %>% 
  group_by(SleepSufficiency) %>% 
  count() %>% 
  ungroup() %>% 
  mutate(percent = n / sum(n)) %>% 
  arrange(percent) %>% 
  mutate(labels = scales::percent(percent))

#making a pie chart with annotations
ggplot(pie_chart, aes(x = "", y = percent, fill = SleepSufficiency)) +
  geom_col() +
  geom_label(aes(label = labels), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  geom_text(aes(label = labels), position = position_stack(vjust = 0.5), show.legend = FALSE) +
  coord_polar(theta = "y") + 
  theme_void() +
  labs(title = "Percentage of Sleep Sufficient Observations")

#Which activity level (steps) gets the most and least sleep?
sleep_steps %>% 
  group_by(SleepSufficiency) %>% 
  count(StepsActivityLevel) %>% 
  ggplot(sleep_activity2, mapping = aes(x = StepsActivityLevel, y = n)) + 
  geom_bar(aes(fill = SleepSufficiency), 
           stat = "identity", color = "white", 
           position = position_dodge(0.9)) + 
  labs(title = "Comparing Standard Sleep Sufficient Observations by Activity Level (Steps)")

#Comparing User Sleep Sufficiency by Activity Level (Mins)
sleep_activity_mins %>% 
  group_by(SleepSufficiency) %>% 
  count(ActivityLevelMins) %>% 
  ggplot(sleep_activity2, mapping = aes(x = ActivityLevelMins, y = n)) + 
  geom_bar(aes(fill = SleepSufficiency), 
           stat = "identity", color = "white", 
           position = position_dodge(0.9)) + 
  labs(title = "Comparing User Sleep Sufficient Observations by Activity Level (Mins)")

#How does being sedentary impact sleep?
ggplot(sleep_activity_cleaned, mapping = aes(x = SedentaryMinutes, y = TotalMinutesAsleep)) + geom_point() + geom_smooth() + labs(title = "Sedentary Minutes vs Total Minutes Asleep")

#How does being awake in bed relate to sleep?
ggplot(data = sleep_activity_mins, mapping = aes(x = AwakeInBed, y = TotalMinutesAsleep)) + 
  geom_point() + 
  geom_smooth() +
  labs(title = "Awake in Bed (Mins) vs Total Minutes Asleep")



