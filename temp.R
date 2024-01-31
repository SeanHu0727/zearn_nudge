# # Incidence of logins for each user on each weekday and month
# incidence_day <- df %>%
#   filter(logged == 1) %>%
#   count(Adult.User.ID, dayofweek) %>%
#   group_by(Adult.User.ID) %>%
#   mutate(frequency = n / sum(n)) %>%
#   select(-n) %>%
#   pivot_wider(names_from = dayofweek, values_from = frequency, values_fill = 0)
#
# incidence_month <- df %>%
#   filter(logged == 1) %>%
#   count(Adult.User.ID, Month) %>%
#   group_by(Adult.User.ID) %>%
#   mutate(frequency = n / sum(n)) %>%
#   select(-n) %>%
#   pivot_wider(names_from = Month, values_from = frequency, values_fill = 0)
#
# # Full join the two incidence dataframes
# incidence_df <- full_join(incidence_day, incidence_month, by = "Adult.User.ID")
#
# # Average streaks and other metrics
# teacher_avg_streak <- df %>%
#   group_by(Adult.User.ID) %>%
#   summarize(
#     streak = mean(streak, na.rm = TRUE),
#     streak_dow = mean(streak_dow, na.rm = TRUE),
#     time_lag = mean(time_lag, na.rm = TRUE),
#     per_week_visits = mean(week_visits, na.rm = TRUE),
#     min_zearn = mean(Minutes.on.Zearn...Total, na.rm = TRUE)
#   )
#
# # Merge with teacher_student_usage_subset to get badge information
# data_with_badges <- teacher_student_usage_subset %>%
#   group_by(Teacher.User.ID, MDR.School.ID) %>%
#   summarize(Badges.per.Active.User = mean(Badges.per.Active.User, na.rm = TRUE)) %>%
#   inner_join(incidence_df, by = c("Teacher.User.ID" = "Adult.User.ID")) %>%
#   inner_join(teacher_avg_streak, by = c("Teacher.User.ID" = "Adult.User.ID")) %>%
#   mutate(
#     MDR.School.ID = factor(MDR.School.ID, ordered = FALSE),
#     Teacher.User.ID = factor(Teacher.User.ID, ordered = FALSE),
#     logBadges = log(Badges.per.Active.User + 1)
#   )
#
# # Linear model
# fixed_model <- lm(Badges.per.Active.User ~
#                     `1` + `2` + `3` + `4` + `5` + `6` + # `7` July is omitted
#                     `8` + `9` + `10` + `11` + `12` +
#                     Monday + Tuesday + Wednesday +
#                     Thursday + Friday + Saturday + # Sunday is omitted
#                     streak + streak_dow +
#                     time_lag + per_week_visits + min_zearn +
#                     factor(MDR.School.ID), data = data_with_badges)
#

library(data.table)
library(dtplyr)
library(tidyverse)
library(ggrepel)
library(gt)
library(plm)
set.seed(794563797)

# Hausman Test supports Fixed Effects
ica_re_model <- pldv(fmla, data = pUsage, model = "random", objfun = "lsq",
                     lower = 0, sample = "cens")


summary(ica_fd_model)$coefficients[-1,"Estimate"] %*% imod.fast$M["Tower.Struggled",1:3] / sd(teacher_student_usage_subset$Tower.Struggled)
imod.fast$M["Tower.Struggled",1] /
  sd(teacher_student_usage_subset$Tower.Struggled) *
  summary(ica_fd_model)$coefficients["ic1","Estimate"] +
  imod.fast$M["Tower.Struggled", 2] / sd(teacher_student_usage_subset$Tower.Struggled) * summary(ica_fd_model)$coefficients["ic2","Estimate"]






