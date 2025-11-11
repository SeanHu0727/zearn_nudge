suppressPackageStartupMessages({
  suppressWarnings({
    library(here)
    library(plm)
    library(readr)
    library(broom)
    library(gt)
    library(conflicted)
    library(tidyr)
    library(ggplot2)
    library(dplyr)
    library(patchwork)
    library(cowplot)
    library(lubridate)
    
    # Explicitly prefer dplyr functions
    suppressMessages({
      conflicts_prefer(dplyr::filter)
      conflicts_prefer(dplyr::select)
      conflicts_prefer(dplyr::summarize)
      conflicts_prefer(dplyr::lead)
    })
  })
})


# IC1 — Empathy-driven engagement (student-perspective practice / scaffolding): Heavy weights on Tower of Power interactions (Struggled/Failed/Completed), Fluency, Guided Practice, Number Gym. Interpreted as teachers working through problems as students would to understand struggles and plan scaffolding.

# IC2 — Classroom activities / materials use: Large weights on Resource Downloads for Small-Group Lessons and Whole-Group Word Problems. Interpreted as preparing and running in-class activities.

# IC3 — Professional development orientation: Weights on Resource Downloads for PD course guides and course notes. Interpreted as engaging with professional-development materials (smaller share of variance, harder to interpret).
df.raw = read.csv(here("plots/ica_weights.csv"))

df <- df.raw |>
    pivot_longer(cols = -vars, names_to = "ic_name", values_to = "value") |>
    mutate(ic_name = case_when(
        ic_name == "V1" ~ "IC1 (15.4%)",
        ic_name == "V2" ~ "IC2 (12.6%)",
        ic_name == "V3" ~ "IC3 (6.3%)" 
    ),
    vars = factor(vars, levels = c(
"Kindergarten Activity Completed",
"Kindergarten Mission RD",
"Kindergarten Schedule RD",
"Elementary Schedule RD",
"Curriculum Map RD",
"Teaching and Learning Approach RD",
"Grade Level Overview RD",
"PD Course Notes RD",
"PD Course Guide RD",
"Mission Overview RD",
"Student Notes and Exit Tickets RD",
"Assessments Answer Key RD",
"Assessments RD",
"Optional Homework RD",
"Whole Group Fluency RD",
"Optional Problem Sets RD",
"Whole Group Word Problems RD",
"Small Group Lesson RD",
"Tower Completed",
"Number Gym Activity Completed",
"Guided Practice Completed",
"Fluency Completed",
"Tower Stage Failed",
"Tower Struggled"))
)
    # sort by IC1 value


g = ggplot(df, aes(x = value, y = vars)) +
    geom_col() +
    facet_wrap(~ic_name) +
    labs(x = "Weight", y = "") +
    theme_bw() +
    geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
    theme(
        panel.grid.minor = element_blank(),
        axis.text.y = element_text(size = 7),
        panel.grid.major = element_blank()
        )



teacher_student_usage_subset <- read.csv(here("plots/teacher_student_usage_subset.csv"))

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
    term == "teacher_number_classes" ~ "Num. Classes",
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
  geom_errorbar(
    aes(xmin = lower, xmax = upper),
    position = position_dodge(width = 0.5),
    width = 0.2,
    linewidth = 0.8,
    orientation = "y"
  ) +
  scale_color_manual(values = c("All Schools" = "#4A7BA7", "Zearn Curriculum" = "#A64F4F")) +
  labs(
    x = "",
    y = "",
    title = "Dep. Variable: ln(Badges + 1)"
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = 'bottom',
    legend.title = element_blank(),
  )

# Combine plots: 2/3 width for g, 1/3 width for coef_plot
# Using cowplot for more stable combining
combined_plot <- plot_grid(g, coef_plot, 
                           ncol = 2, 
                           rel_widths = c(2, 1),
                           align = "h",
                           axis = "tb")

# Display combined plot
print(combined_plot)

# Save combined plot
ggsave(
  filename = "/Users/benjaminmanning/Desktop/combined_ica_plots.png",
  plot = combined_plot,
  width = 12,
  height = 3,
  dpi = 300
)
