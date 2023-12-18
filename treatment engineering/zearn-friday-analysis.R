# Load necessary libraries and data
library(tidyverse)
library(zoo)
library(data.table)
library(glmnet)
library(MLmetrics)
library(pROC)
library(caret)
library(pbapply)

setwd('/Users/sean/Desktop/zearn_nudge')
load('raw_data/Zearn_PCS_cleaned20230711.RData')

# Delete users with fewer than 48 (k*8) log-ins
T[,n_logged:=sum(logged), Adult.User.ID]
T <- T[n_logged>0]

# Create folds
# T[,folds:=createFolds(logged,k=6, list = FALSE), Adult.User.ID]

# AUC and achievement
load("raw_data/Zearn_full_data_ICA.Rdata")
badges <- read_csv(file = "raw_data/Classroom Student Usage - Time Series 2020-10-09T1616.csv")
teachers <- read_csv(file = "raw_data/Classroom-Teacher Lookup 2020-10-09T1618.csv")
  
badges <- badges %>%
  inner_join(teachers, by = "Classroom ID") %>%
  group_by(`Teacher User ID`) %>%
  summarize(Badges.per.Active.User = mean(`Badges per Active User`))

teacher_student_usage_subset[c("Teacher.User.ID","Badges.per.Active.User")]

#----------------------------------------------------------------
#------------------------ Simpler Model -------------------------
#----------------------------------------------------------------
teacher_usage <- read.csv("raw_data/Teacher Usage - Time Series 2020-10-09T1635.csv")
# Convert Usage.Time to a date object and find the weekday of each date
# 1 means Monday
teacher_usage$weekday <- lubridate::wday(teacher_usage$Usage.Time)
teacher_usage$month <- lubridate::month(teacher_usage$Usage.Time)
# Find the incidence of logins for each user on each weekday
incidence_day <- with(teacher_usage, table(Adult.User.ID, weekday))
incidence_month <-  with(teacher_usage, table(Adult.User.ID, month))
# Normalize incidence by the total number of logins per user
incidence_day <- prop.table(incidence_day, 1)
incidence_month <- prop.table(incidence_month, 1)
# Convert the table to a data frame
incidence_df <- as.data.frame.table(incidence_day, responseName = "frequency")
names(incidence_df)[1:2] <- c("Adult.User.ID", "weekday")
# Make sure Adult.User.ID is numeric for proper merging
incidence_df$Adult.User.ID <- as.numeric(as.character(incidence_df$Adult.User.ID))
incidence_df <- incidence_df %>%
  pivot_wider(names_from = weekday, values_from = frequency)

incidence_mon_df <- as.data.frame.table(incidence_month, responseName = "frequency")
names(incidence_mon_df)[1:2] <- c("Adult.User.ID", "weekday")
# Make sure Adult.User.ID is numeric for proper merging
incidence_mon_df$Adult.User.ID <- as.numeric(as.character(incidence_mon_df$Adult.User.ID))
incidence_mon_df <- incidence_mon_df %>%
  pivot_wider(names_from = weekday, values_from = frequency)

incidence_df <- incidence_df %>%
  rename(mon = `1`,
         tue = `2`,
         wed = `3`,
         thu = `4`,
         fri = `5`,
         sat = `6`,
         sun = `7`) %>%
  full_join(incidence_mon_df, by = "Adult.User.ID")

teacher_avg_streak <- T %>%
  group_by(Adult.User.ID) %>%
  summarize(streak = mean(streak),
            streak_dow = mean(streak_dow),
            time_lag = mean(time_lag),
            prev_week_visits = mean(prev_week_visits),
            prev_min_zearn = mean(prev_min_zearn))

# Merge with teacher_student_usage_subset to get badge information
teacher_student_usage_subset %>%
  group_by(Classroom.ID, Teacher.User.ID, MDR.School.ID) %>% 
  select_if(is.numeric) %>%
  summarise(across(`Active.Users...Total`:`Tower.Alerts.per.Tower.Completion`, mean, na.rm = TRUE)) %>%
  inner_join(incidence_df,by = c("Teacher.User.ID" = "Adult.User.ID")) %>%
  inner_join(teacher_avg_streak, by = c("Teacher.User.ID" = "Adult.User.ID")) -> data_with_badges

# Perform regression with Badges as the outcome and weekday frequencies as predictors
# specify the model with mixed effects
library(lme4)
# specify the fixed effects model (!!! this one is without constant term == wrong)
# fixed_model <- lm(Badges.per.Active.User ~ 0 + `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + `11` +
#                     mon + tue + wed + thu + fri + sat +
#                     streak + streak_dow + time_lag + prev_week_visits + prev_min_zearn +
#                     factor(MDR.School.ID), data = data_with_badges)

fixed_model <- lm(Badges.per.Active.User ~ `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + `11` +
                    mon + tue + wed + thu + fri + sat +
                    streak + streak_dow + time_lag + prev_week_visits + prev_min_zearn +
                    factor(MDR.School.ID), data = data_with_badges)

summary(fixed_model)
sd(data_with_badges$fri) # 0.06177653
sd(data_with_badges$Badges.per.Active.User) # 1.004252
0.06177653/1.004252 =  0.06151497
0.06151497 * 1.718 = 0.1056827

fixed_model <- lm(Badges.per.Active.User ~ `1` + `2` + `3` + `4` + `5` + `6` + `7` + `8` + `9` + `10` + `11` +
                    mon + tue + wed + thu + fri + sat +
                    streak + streak_dow + time_lag + prev_week_visits + prev_min_zearn, data = data_with_badges)

summary(fixed_model)
