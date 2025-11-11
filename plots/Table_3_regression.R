# Read in the data
df <- readr::read_csv("/Users/benjaminmanning/Desktop/df_habits.csv")

df <- df %>%
  mutate(Adult.User.ID = factor(Adult.User.ID, ordered = FALSE),
         MDR.School.ID = factor(MDR.School.ID, ordered = FALSE),
         logBadges = log1p(Badges.per.Active.User))

habits_model <- lm(Badges.per.Active.User ~
                     Mon + Tue + Wed + Thu + Fri + Sat +
                     month_1 + month_2 + month_3 +
                     month_4 + month_5 + month_6 +
                     month_8 + month_9 +
                     month_10 + month_11 + month_12 +
                     Total_Minutes_on_Zearn + mean_streak +
                     mean_time_lag + mean_streak_dow + total_visits,
                   data = df)

habits_results <- broom::tidy(
  lmtest::coeftest(
    habits_model,
    vcov = sandwich::vcovCL(habits_model,
                            cluster = ~ MDR.School.ID,
                            type = "HC3")
  )
)
# Calculate values for additional rows
r_squared <- summary(habits_model)$r.squared
n_total <- nrow(df)

format_coef <- function(coef, se) {
  paste0(sprintf("%.3f", coef), " (", sprintf("%.3f", se), ")")
}

# Convert to a gt table
gt_table <- habits_results %>%
  as.data.frame() %>%
  select(-c("statistic")) %>%
  mutate(
    Coefficient = mapply(format_coef, estimate, `std.error`),
    p_value = mapply(format.pval, `p.value`, digits = 3, eps = .001),
    term = case_when(
      term == "Mon" ~ "`%`Monday",
      term == "Tue" ~ "`%`Tuesday",
      term == "Wed" ~ "`%`Wednesday",
      term == "Thu" ~ "`%`Thursday",
      term == "Fri" ~ "`%`Friday",
      term == "Sat" ~ "`%`Saturday",
      term == "month_1" ~ "`%`January",
      term == "month_2" ~ "`%`February",
      term == "month_3" ~ "`%`March",
      term == "month_4" ~ "`%`April",
      term == "month_5" ~ "`%`May",
      term == "month_6" ~ "`%`June",
      term == "month_8" ~ "`%`August",
      term == "month_9" ~ "`%`September",
      term == "month_10" ~ "`%`October",
      term == "month_11" ~ "`%`November",
      term == "month_12" ~ "`%`December",
      term == "Total_Minutes_on_Zearn" ~ "Avg. Minutes",
      term == "mean_streak" ~ "Avg. Streak",
      term == "mean_time_lag" ~ "Avg. Days Between Logins",
      term == "mean_streak_dow" ~ "Avg. Weekday Streak",
      term == "total_visits" ~ "Total Logins",
      TRUE ~ term  # Default case
    )) %>%
  mutate(p_value = sub("0.", ".", p_value)) %>%
  select(term, Coefficient, p_value) %>%
  gt() %>%
  tab_spanner(
    label = "Avg. Weekly Badges",
    columns = c(Coefficient, p_value)
  ) %>%
  cols_label(
    term = "",
    Coefficient = "Estimate (SE)",
    p_value = "P-value"
  ) %>%
  cols_align(
    align = "center",
    columns = c(Coefficient, p_value)
  ) %>%
  # Add rows for R-squared and N
  rows_add(
    term = c("R-squared", "N"),
    Coefficient = c(sprintf("%.3f", r_squared), as.character(n_total)),
    p_value = c("", "")
  ) %>%
  # Use tab_style to add double lines for model fit statistics
  tab_style(
    style = list(
      cell_borders(
        sides = "top",
        style = "double",
        color = "lightgray",
        weight = px(4)
      )),
    locations = cells_body(
      columns = everything(),
      rows = term %in% c("R-squared")
    )
  ) %>%
  fmt_markdown() %>%
  as_raw_html()

# Coefficient Plot
library(ggplot2)

# Prepare data for plotting
plot_data <- habits_results %>%
  filter(term != "(Intercept)") %>%  # Exclude intercept
  mutate(
    term_label = case_when(
      term == "Mon" ~ "Monday",
      term == "Tue" ~ "Tuesday",
      term == "Wed" ~ "Wednesday",
      term == "Thu" ~ "Thursday",
      term == "Fri" ~ "Friday",
      term == "Sat" ~ "Saturday",
      term == "month_1" ~ "January",
      term == "month_2" ~ "February",
      term == "month_3" ~ "March",
      term == "month_4" ~ "April",
      term == "month_5" ~ "May",
      term == "month_6" ~ "June",
      term == "month_8" ~ "August",
      term == "month_9" ~ "September",
      term == "month_10" ~ "October",
      term == "month_11" ~ "November",
      term == "month_12" ~ "December",
      term == "Total_Minutes_on_Zearn" ~ "Avg. Minutes",
      term == "mean_streak" ~ "Avg. Streak",
      term == "mean_time_lag" ~ "Avg. Days Between Logins",
      term == "mean_streak_dow" ~ "Avg. Weekday Streak",
      term == "total_visits" ~ "Total Logins",
      TRUE ~ term
    ),
    # Calculate 95% confidence intervals
    ci_lower = estimate - 1.96 * std.error,
    ci_upper = estimate + 1.96 * std.error
  )

# Create the coefficient plot
coef_plot <- ggplot(plot_data, aes(x = reorder(term_label, estimate), y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.5) +
  geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                width = 0.2, color = "steelblue", size = 0.8) +
  geom_point(size = 2, color = "steelblue") +
  labs(
    title = "",
    x = "",
    y = "Log(Avg. Weekly Badges/Student + 1)",
    caption = "Error bars represent 95% confidence intervals"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(face = "bold", hjust = 0.5),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(size = 10),
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10)
  )

coef_plot

# Save the plot
ggsave(
  filename = "/Users/benjaminmanning/Desktop/coefficient_plot.png", 
  plot = coef_plot, 
  width = 8, 
  height = 4, 
  dpi = 300
)
