#############
## Zearn effects of teacher behavior on student badge achievement // Whole LA data
## Marcos Gallo
############
## DATA: this file will be independent of previous data processing code, so note for redundancy:
library(tidyverse)
library(zoo)

## Importing files:
setwd("/Users/sean/Desktop/zearn_nudge/raw_data")
teacher_usage <- read.csv("Teacher Usage - Time Series 2020-10-09T1635.csv")
classroom_student_usage <- read.csv("Classroom Student Usage - Time Series 2020-10-09T1616.csv")
classroom_info <- read.csv("Classroom Info 2020-10-09T1617.csv")
classroom_teacher_lookup <- read.csv("Classroom-Teacher Lookup 2020-10-09T1618.csv")
#teacher_info <- read.csv("Data/raw/2019/Teacher Info 20182019.csv")
school_info <- read.csv("School Info 2020-10-09T1619.csv")
# has_account <- school_info$MDR.School.ID[school_info$Has.School.Account..Yes...No. == "Yes"]
# classroom_has_account <- classroom_info$Classroom.ID[classroom_info$MDR.School.ID %in% has_account]
la_usage_types <- read_csv("la_usage_types.csv")

#####################################
### TEACHERS
# Merging with the classroom info for further merge with teacher info
classroom_student_usage_subset <- merge(classroom_student_usage, classroom_teacher_lookup,
                                        by = c("Classroom.ID"))
classroom_student_usage_subset$week = lubridate::week(classroom_student_usage_subset$Usage.Week)
classroom_student_usage_subset$year = lubridate::year(classroom_student_usage_subset$Usage.Week)

# Noting week, month, and year to summarize teacher behavior: 
teacher_usage$week = lubridate::week(teacher_usage$Usage.Time)
teacher_usage$year = lubridate::year(teacher_usage$Usage.Time)
teacher_usage$wday = lubridate::wday(teacher_usage$Usage.Time)
teacher_usage$hour = lubridate::hour(teacher_usage$Usage.Time)
teacher_usage$whour = (teacher_usage$wday - 1)*24 + teacher_usage$hour


# Find total behavior per week per teacher
teacher_usage$Event.Type[teacher_usage$Event.Type == "Resource Downloaded"] <- paste("RD.",teacher_usage$Curriculum.Resource.Category[teacher_usage$Event.Type == "Resource Downloaded"])
teacher_usage_total <- teacher_usage %>% 
  group_by(Adult.User.ID,Event.Type,week,year) %>% 
  summarise(Freq=n()) %>% 
  pivot_wider(id_cols = c(Adult.User.ID,week,year), names_from = Event.Type, values_from = Freq)
# Replace NAs with 0
teacher_usage_total[is.na(teacher_usage_total)] <- 0

# Merge teacher and student data
teacher_student_usage_subset <- merge(classroom_student_usage_subset, teacher_usage_total, 
                                      by.x = c("Teacher.User.ID","week","year"), by.y = c("Adult.User.ID","week","year"))

# Adding the number of classrooms each teacher has
T <- data.frame(table(classroom_teacher_lookup$Teacher.User.ID))
T <- rename(.data = T, teacher.number.classes = Freq)
teacher_student_usage_subset <- merge(teacher_student_usage_subset, T, 
                                      by.x = c("Teacher.User.ID"), by.y = c("Var1"))

teacher_student_usage_subset <- merge(teacher_student_usage_subset, classroom_info, 
                                      by = c("Classroom.ID"))
teacher_student_usage_subset$Grade.Level <- factor(teacher_student_usage_subset$Grade.Level, ordered = TRUE)


la_usage_types$curriculum <- 0
la_usage_types$curriculum[la_usage_types$`Schools and Districts Usage Type` == "20-21 Curriculum"] <- 1
la_usage_types$curriculum[la_usage_types$`Schools and Districts Usage Type` == "Core Complement"] <- 1

teacher_student_usage_subset <- merge(teacher_student_usage_subset, la_usage_types, 
                                      by.x = c("MDR.School.ID"), by.y = c("Schools and Districts MDR School ID"),
                                      all.x = TRUE)
teacher_student_usage_subset <- teacher_student_usage_subset[-44]

teacher_student_usage_subset <- merge(teacher_student_usage_subset, school_info[c(1,7)], 
                                      by = c("MDR.School.ID"),
                                      all.x = TRUE)
teacher_student_usage_subset$Demographics...Zipcode.Median.Income <- factor(teacher_student_usage_subset$Demographics...Zipcode.Median.Income, ordered = TRUE)

###################################################
### STUDENTS
## Remove classes inactive for more than 7 months
# Remove June/July/August
teacher_student_usage_subset$month = lubridate::month(teacher_student_usage_subset$Usage.Week)
teacher_student_usage_subset <- subset(teacher_student_usage_subset, month != 6 & month != 7 & month != 8)
teacher_student_usage_subset <- teacher_student_usage_subset[-46]
# Amount of data for each classroom
M <- table(teacher_student_usage_subset$Classroom.ID)
M <- data.frame(M)
active_classrooms <- M[M$Freq > 20,]$Var1
# Active for more than 5 months
teacher_student_usage_subset <- teacher_student_usage_subset[teacher_student_usage_subset$Classroom.ID %in% active_classrooms, ]
## We still have a lot of weird outliers. Let's look at the mean active students throughout the year.
average_student = aggregate(Active.Users...Total ~ Classroom.ID, data=teacher_student_usage_subset, FUN = mean)
# More than 5 active students on average:
teacher_student_usage_subset <- teacher_student_usage_subset[teacher_student_usage_subset$Classroom.ID %in% average_student$Classroom.ID[average_student$Active.Users...Total > 5],]
# Remove duplicate (classroom, week) pairs // I.E., remove classrooms with more than one teacher
duplicates <- teacher_student_usage_subset
duplicates$unique_id <- paste(duplicates$Classroom.ID,duplicates$Usage.Week) # concatenate to make unique ID
duplicates$duplicate = duplicated(duplicates$unique_id) # generate the duplicate variable
teacher_student_usage_subset <- subset(duplicates, duplicate=="FALSE") # delete the duplicates
# Replace NAs with 0
teacher_student_usage_subset[is.na(teacher_student_usage_subset$Badges.per.Active.User),]$Badges.per.Active.User <- 0
# Remove small classrooms
teacher_student_usage_subset <- subset(teacher_student_usage_subset, Students...Total>5) 




#---------------------------------------------------------
#---------------------Burstiness and memory---------------
#---------------------------------------------------------
# 
teacher_bursts <- teacher_usage[teacher_usage$Adult.User.ID %in% teacher_student_usage_subset$Teacher.User.ID,]

#aggregate by day
teacher_bursts$day = lubridate::day(teacher_bursts$Usage.Time)
teacher_bursts$month = lubridate::month(teacher_bursts$Usage.Time)
teacher_bursts = aggregate(Minutes.on.Zearn...Total ~day+week+month+year+Adult.User.ID, data=teacher_bursts[teacher_bursts$Event.Type == "User Session",], FUN = sum)
teacher_bursts$DATE <- paste(teacher_bursts$month, teacher_bursts$day, teacher_bursts$year, sep = "/")
# Find lags between activity
teacher_bursts$DATE <- as.Date(teacher_bursts$DATE, format="%m/%d/%Y")
teacher_bursts <- teacher_bursts[with(teacher_bursts, order(teacher_bursts$Adult.User.ID, teacher_bursts$DATE)),]
first <- c()
for (i in seq_along(teacher_bursts$DATE))
  if (i == 1) {
    first[1] <- 0
  } else if (teacher_bursts$Adult.User.ID[i] != teacher_bursts$Adult.User.ID[i-1]) {
    first[i] <- 0
  } else {
    first[i] <- teacher_bursts$DATE[i] - teacher_bursts$DATE[i-1]
  }
teacher_bursts$time_lag <- first

#B_M_daily = daily_24hf[attended == 1 & !is.na(time_lag) & num_obs >= 365]
#B_M_daily = B_M_daily[order(time)]
library(data.table)
library(dplyr)
B_M_daily = teacher_bursts
setDT(B_M_daily)
B_M_daily$lag_time_lag <- dplyr::lag(B_M_daily$time_lag, n=1)
B_M_daily = B_M_daily[,.(sigma_tau = sd(time_lag),
                         mean_tau = mean(time_lag),
                         memory   = cor(time_lag, lag_time_lag, use = 'complete.obs')), Adult.User.ID]
B_M_daily = merge(B_M_daily, teacher_bursts[,.(mean_visits = length(Minutes.on.Zearn...Total),
                                               mean_timelag = mean(time_lag, na.rm = TRUE)), Adult.User.ID], all.x = TRUE, by = c("Adult.User.ID"))
B_M_daily[,burstiness:=(sigma_tau - mean_tau)/(sigma_tau+mean_tau)]
hist(B_M_daily$burstiness)
# Plot:
B_M_daily[,unit := rep(c("Daily"), each = nrow(B_M_daily))]
ggplot(B_M_daily, aes(x = memory, y = burstiness)) +
  geom_point(color = 'darkgrey') +
  theme_bw() +
  geom_vline(xintercept = 0, lty = 'dashed') +
  geom_hline(yintercept = 0, lty ='dashed')

# save(B_M_daily, file = "Zearn_Burstiness_Memory.RData")

teacher_student_usage_subset <- merge(teacher_student_usage_subset, B_M_daily[,c(1,4,7)],
                                      by.x = c("Teacher.User.ID"), by.y = c("Adult.User.ID"))
# Delete NAs
teacher_student_usage_subset <- teacher_student_usage_subset[!is.na(teacher_student_usage_subset$burstiness),]
teacher_student_usage_subset <- teacher_student_usage_subset[!is.na(teacher_student_usage_subset$memory),]
# Take out spaces from names
teacher_student_usage_subset <- teacher_student_usage_subset %>%
  rename(
    Fluency.Completed = `Fluency Completed`,
    #Resource.Downloaded = `Resource Downloaded`,
    User.Session = `User Session`,
    Guided.Practice.Completed = `Guided Practice Completed`,
    Kindergarten.Activity.Completed = `Kindergarten Activity Completed`,
    Number.Gym.Activity.Completed = `Number Gym Activity Completed`,
    Tower.Stage.Failed = `Tower Stage Failed`,
    Tower.Completed = `Tower Completed`,
    Tower.Struggled = `Tower Struggled`
  )

#---------------------------------------------------------
#---------------------Run regressions---------------
#---------------------------------------------------------

library("PerformanceAnalytics")

#---------------------------------------------------------
#---------------------Panel Data---------------
#---------------------------------------------------------
library("plm")

## ICA
set.seed(123)
library(fastICA)
ica <- fastICA(X = teacher_student_usage_subset[c(14:39)], n.comp=3, row.norm = TRUE)
#ica8 <- fastICA(X = teacher_student_usage_subset[c(13:39)], n.comp=8, row.norm = TRUE)
pca <- prcomp(teacher_student_usage_subset[c(14:39)], tol = 0.1)
prop_variance = summary(pca)$importance[2,][1:10]

png("/Users/sean/Desktop/zearn_nudge/robustness_check/Elbow_plot.png",res = 120)
x <- 1:10
plot(x, prop_variance, type = "b", pch = 19, 
     col = "blue", xlab = "N-th cluster", ylab = "Proportion of Variance",
     main="Elbow method for\n determining optimal k")
dev.off()


ica.comps <- data.frame(ica$S)
teacher_student_usage_subset$ica1 <- ica.comps$X1
teacher_student_usage_subset$ica2 <- ica.comps$X2
teacher_student_usage_subset$ica3 <- ica.comps$X3
ica$A

#---------------------------------------------------------
#---------------------Panel Data---------------
#---------------------------------------------------------
library("plm")
library(performance)

fmla <- as.formula(paste("log(Badges.per.Active.User+1) ~ ", paste0(sprintf("`%s`", colnames(teacher_student_usage_subset[c(41:45,48:49,50:52)])), collapse = " + ")))
# 41: teacher.number.classes
# 42: Grad.level (not shown)
# 43: Student...Total
# 44: Curriculum
# 45: Demographics...Zipcode.Median.Income (not shown)
# 48: memory
# 49: burstiness
# 50~52: ia1~ia3

ols <- plm(fmla, data = teacher_student_usage_subset, index = c("Classroom.ID", "Usage.Week", "Teacher.User.ID"), model = "pooling")
# between <- update(ols, model = "between")
within <- update(ols, model = "within")
# fixed-effect
summary(within)

# Summary stats aggregated (pre-experimental intervention data)
# # of teachers
# N
length(unique(teacher_student_usage_subset$Teacher.User.ID))

# # of classes
# N
nrow(unique(teacher_student_usage_subset[,c('Teacher.User.ID','Classroom.ID')]))
# Mean
nrow(unique(teacher_student_usage_subset[,c('Teacher.User.ID','Classroom.ID')])) / length(unique(teacher_student_usage_subset$Teacher.User.ID))
# Quatile
temp = unique(teacher_student_usage_subset[,c('Teacher.User.ID','Classroom.ID')])
temp_l = vector(mode="numeric")
for (i in unique(teacher_student_usage_subset$Teacher.User.ID)){
  temp_l = append(temp_l,nrow(temp[temp$Teacher.User.ID == i, ]))
}
quantile(temp_l, probs = c(0,0.25,0.5,0.75,1)) 
# stdev
sd(temp_l)

# of badges
# N
teacher_student_usage_subset$Total.Badges = teacher_student_usage_subset$Active.Users...Total * teacher_student_usage_subset$Badges.per.Active.User
sum(teacher_student_usage_subset$Total.Badges)
# mean and quatile
temp = aggregate(teacher_student_usage_subset$Total.Badges, by=list(Category=teacher_student_usage_subset$Teacher.User.ID), FUN=sum)
sum(temp$x)/length(temp$Category)
quantile(temp$x, probs = c(0,0.25,0.5,0.75,1))
# stdev
sd(temp$x)

# of minutes on Zearn
temp <- teacher_usage[ ! teacher_usage$Adult.User.ID %in% unique(teacher_student_usage_subset$Teacher.User.ID), ]
temp = aggregate(temp$Minutes.on.Zearn...Total, by = list(temp$Adult.User.ID), FUN = sum)
# N
sum(temp$x)
# mean
sum(temp$x)/length(temp$Group.1)
# quatile
quantile(temp$x, probs = c(0,0.25,0.5,0.75,1)) 
# stdev
sd(temp$x)



# Summary stats student-week (pre-experimental intervention data)

# of badges
# N
teacher_student_usage_subset$Total.Badges = teacher_student_usage_subset$Active.Users...Total * teacher_student_usage_subset$Badges.per.Active.User
sum(teacher_student_usage_subset$Total.Badges)
# mean and quatile
sum(teacher_student_usage_subset$Total.Badges)/length(teacher_student_usage_subset$Teacher.User.ID)
quantile(teacher_student_usage_subset$Total.Badges, probs = c(0,0.25,0.5,0.75,1))
# stdev
sd(teacher_student_usage_subset$Total.Badges)

# of minutes on Zearn
temp <- teacher_usage[ ! teacher_usage$Adult.User.ID %in% unique(teacher_student_usage_subset$Teacher.User.ID), ]
# mean and quatile
sum(temp$Minutes.on.Zearn...Total)/length(temp$Minutes.on.Zearn...Total)
quantile(temp$Minutes.on.Zearn...Total, probs = c(0,0.25,0.5,0.75,1))
# stdev
sd(temp$Minutes.on.Zearn...Total)


# aggregated event type

temp <- teacher_usage[ ! teacher_usage$Adult.User.ID %in% unique(teacher_student_usage_subset$Teacher.User.ID), ]
temp$count = 1
temp = aggregate(temp$count, by = list(temp$Adult.User.ID, temp$Event.Type, temp$year, temp$week), FUN = sum)
temp_list = unique(temp$Group.2)
for (i in temp_list) {
  print(i)
  temp_spec <- temp[ temp$Group.2 == i, ]
  # N
  print(sum(temp_spec$x))
  # mean
  print(sum(temp_spec$x) / length(temp_spec$x))
  # quatile
  print(quantile(temp_spec$x, probs = c(0,0.25,0.5,0.75,1)))
  # stdev
  print(sd(temp_spec$x))
}
  
  

# correlation matrix for teacher activity
M <- cor(teacher_student_usage_subset[c(14:39)])
library('corrplot') #package corrplot
png("/Users/sean/Desktop/zearn_nudge/robustness_check/teacher_activity_corr_plot.png",res = 70)
corrplot(M, method = "color", type = 'lower', diag = FALSE) #plot matrix
dev.off()
cor(teacher_student_usage_subset[c(37:38)])
cor(teacher_student_usage_subset[c(24:28)])
  
# PCA regression without bursty and memory
fmla <- as.formula(paste("log(Badges.per.Active.User+1) ~ ", paste0(sprintf("`%s`", colnames(teacher_student_usage_subset[c(41:45,50:52)])), collapse = " + ")))
# 41: teacher.number.classes
# 42: Grad.level (not shown)
# 43: Student...Total
# 44: Curriculum
# 45: Demographics...Zipcode.Median.Income (not shown)
# 48: memory
# 49: burstiness
# 50~52: ia1~ia3

ols <- plm(fmla, data = teacher_student_usage_subset, index = c("Classroom.ID", "Usage.Week", "Teacher.User.ID"), model = "pooling")
# between <- update(ols, model = "between")
within <- update(ols, model = "within")
# fixed-effect
summary(within)
sd(teacher_student_usage_subset$ica1)
sd(teacher_student_usage_subset$ica2)
sd(teacher_student_usage_subset$ica3)
sd(log(teacher_student_usage_subset$Badges.per.Active.User+1))
1.000004/0.589562 = 1.696181

0.310, 0.118, 0.067
1.696181*0.183*.897 + 1.696181*0.070*0.042 +1.696181*0.040*(-0.027) = 0.2815847


