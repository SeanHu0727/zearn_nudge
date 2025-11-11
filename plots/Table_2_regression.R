library(plm)
library(readr)
library(dplyr)
library(broom)
library(gt)
library(ggplot2)

teacher_student_usage_subset <- read_csv("/Users/benjaminmanning/Desktop/teacher_student_usage_subset.csv")

# Define the columns to be used in the formula
columns_used <- colnames(teacher_student_usage_subset %>%
                           select(ic1:ic3,
                                  teacher_number_classes,
                                  Students...Total,
                                  Grade.Level
                                  # MDR.School.ID,
                           ))

# Create the formula
fmla_str <- sprintf("logBadges ~ %s",
                    paste(sprintf("`%s`", columns_used), collapse = " + "))
fmla <- as.formula(fmla_str)

# Define the panel model index
pUsage <- pdata.frame(
  teacher_student_usage_subset %>%
    arrange(Classroom.ID, Adult.User.ID, year, week) %>%
    mutate(Classroom.ID = factor(Classroom.ID, ordered = FALSE),
           Adult.User.ID = factor(Adult.User.ID, ordered = FALSE),
           Usage.Week = as_date(Usage.Week),
           # MDR.School.ID = factor(MDR.School.ID, ordered = FALSE),
           # District.Rollup.ID = factor(District.Rollup.ID, ordered = FALSE),
           Grade.Level = factor(Grade.Level, ordered = TRUE),
           medianIncome = factor(medianIncome, ordered = TRUE, exclude = ""),
           povertyLevel = factor(povertyLevel, ordered = TRUE, exclude = ""),
           zipcode = factor(zipcode, ordered = FALSE),
           logBadges = log1p(Badges.per.Active.User)),
  index = c("Classroom.ID", "Usage.Week", "Adult.User.ID"))

# 'Within' model
ica_fe_model <- plm(fmla, data = pUsage, model = "within")

# Hausman Test supports Fixed Effects
# ica_re_model <- plm(fmla, data = pUsage, model = "random")
# phtest(ica_fe_model, ica_re_model)
# chisq = 81.31, df = 4, p-value < 2.2e-16
# alternative hypothesis: one model is inconsistent


# Subsample model
pUsage <- pUsage %>% filter(schoolAccount == 1, curriculum == 1)

subsample_model <- colnames(teacher_student_usage_subset %>%
                              select(ic1:ic3,
                                     teacher_number_classes,
                                     Students...Total,
                                     Grade.Level,
                                     medianIncome
                                     # MDR.School.ID,
                              ))
fmla_str <- sprintf("logBadges ~ %s",
                    paste(sprintf("`%s`", subsample_model), collapse = " + "))
fmla <- as.formula(fmla_str)

ica_fe_subsample <- plm(fmla, data = pUsage, model = "within")

## Graphing Helper Functions
# Function to format coefficients and standard errors
format_coef_se <- function(coef, stars, se) {
  paste0(sprintf("%.3f", coef),
         stars,
         "<br>",
         "(",
         sprintf("%.4f", se),
         ")")
}

# Function to add significance asterisks
add_significance <- function(p_value) {
  if (p_value < 0.001) {
    return("***")
  } else if (p_value < 0.01) {
    return("**")
  } else if (p_value < 0.05) {
    return("*")
  } else {
    return("")
  }
}

# Improve formatting for the Average Intercept, R-squared, and N
format_value <- function(value, digits = 3) {
  round(value, digits = digits)
}


# Extracting model summaries
ica_fe_summary <- broom::tidy(ica_fe_model)
ica_fe_summary[, c(3:5)] <-
  summary(ica_fe_model,
          vcov = vcovHC(ica_fe_model,
                        type = "HC3"))$coef[, c(2:4)]
ica_fe_subsample_summary <- broom::tidy(ica_fe_subsample)
ica_fe_subsample_summary[, c(3:5)] <-
  summary(ica_fe_subsample,
          vcov = vcovHC(ica_fe_subsample,
                        type = "HC3"))$coef[, c(2:4)]

# Merging summaries into one data frame
merged_summaries <- merge(
  ica_fe_summary,
  ica_fe_subsample_summary,
  by = "term",
  suffixes = c("_fe", "_fe_subsample")
)

# Apply formatting to coefficients, standard errors, and adding significance
merged_summaries <- merged_summaries %>%
  mutate(
    significance_fe = sapply(p.value_fe, add_significance),
    significance_fe_subsample = sapply(p.value_fe_subsample, add_significance),
    coef_se_fe = format_coef_se(estimate_fe,
                                significance_fe,
                                std.error_fe),
    coef_se_fe_subsample = format_coef_se(
      estimate_fe_subsample,
      significance_fe_subsample,
      std.error_fe_subsample
    )
  ) %>%
  select(term, coef_se_fe, coef_se_fe_subsample) %>%
  mutate(term = case_when(
    term == "ic1" ~ "IC 1",
    term == "ic2" ~ "IC 2",
    term == "ic3" ~ "IC 3",
    term == "teacher_number_classes" ~ "No. of Classes",
    .default = term  # Keep all other terms unchanged
  ))

gt_table <- gt(merged_summaries) %>%
  cols_label(
    term = "",
    coef_se_fe = "All Schools",
    coef_se_fe_subsample = "Zearn Curriculum"
  ) %>%
  tab_spanner(
    label = "ln(Badges + 1)",
    columns = c(coef_se_fe, coef_se_fe_subsample)
  ) %>%
  tab_footnote(
    footnote = "Note: * p<0.05; ** p<0.01; *** p<0.001"
  ) %>%
  rows_add(
    term = c(
      "Average Intercept",
      "R-squared",
      "N"
    ),
    coef_se_fe = c(
      format_value(mean(fixef(ica_fe_model))),
      format_value(summary(ica_fe_model)$r.squared[1]),
      paste(
        "Teachers: ", format_value(n_distinct(index(ica_fe_model$model)$Adult.User.ID)),
        "<br>Classes: ", format_value(pdim(ica_fe_model)$nT$n),
        "<br>Weeks: ", format_value(pdim(ica_fe_model)$nT$T),
        "<br>Total: ", format_value(pdim(ica_fe_model)$nT$N),
        sep = ""
      )
    ),
    coef_se_fe_subsample = c(
      format_value(mean(fixef(ica_fe_subsample))),
      format_value(summary(ica_fe_subsample)$r.squared[1]),
      paste(
        "Teachers: ", format_value(n_distinct(index(ica_fe_subsample$model)$Adult.User.ID)),
        "<br>Classes: ", format_value(pdim(ica_fe_subsample)$nT$n),
        "<br>Weeks: ", format_value(pdim(ica_fe_subsample)$nT$T),
        "<br>Total: ", format_value(pdim(ica_fe_subsample)$nT$N),
        sep = ""
      )
    )
  ) %>%
  tab_style(
    style = cell_text(weight = "bold", align = "center"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(align = "center"),
    locations = cells_body(columns = c(coef_se_fe, coef_se_fe_subsample))
  ) %>%
  tab_style(
    style = cell_text(align = "left"),
    locations = cells_stub()
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

#######################################
# Coefficient Plot
#######################################

# Prepare data for coefficient plot
coef_plot_data <- bind_rows(
  ica_fe_summary %>%
    mutate(
      model = "All Schools",
      lower = estimate - 1.96 * std.error,
      upper = estimate + 1.96 * std.error
    ),
  ica_fe_subsample_summary %>%
    mutate(
      model = "Zearn Curriculum",
      lower = estimate - 1.96 * std.error,
      upper = estimate + 1.96 * std.error
    )
) %>%
  # Filter for only IC1, IC2, IC3, and number of classes
  filter(term %in% c("ic1", "ic2", "ic3", "teacher_number_classes")) %>%
  mutate(term = case_when(
    term == "ic1" ~ "IC 1",
    term == "ic2" ~ "IC 2",
    term == "ic3" ~ "IC 3",
    term == "teacher_number_classes" ~ "No. of Classes",
    .default = term
  )) %>%
  # Reorder terms for better visualization
  mutate(term = factor(term, levels = c("No. of Classes", "IC 3", "IC 2", "IC 1")))

# Create coefficient plot
coef_plot <- ggplot(coef_plot_data, aes(x = estimate, y = term, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbarh(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.5),
    height = 0.2,
    linewidth = 0.8
  ) +
  scale_color_manual(values = c("All Schools" = "#4A7BA7", "Zearn Curriculum" = "#A64F4F")) +
  labs(
    x = "Coefficient Estimate (95% CI)",
    y = "",
    color = "Model",
    title = "Regression Coefficients: ln(Badges + 1)"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "bottom",
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    plot.title = element_text(size = 12, hjust = 0.5, face = "bold")
  )

print(coef_plot)

# Save plot
ggsave(
  filename = "/Users/benjaminmanning/Desktop/coefficient_plot.png",
  plot = coef_plot,
  width = 8,
  height = 2
)
