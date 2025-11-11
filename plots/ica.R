suppressPackageStartupMessages({
  suppressWarnings({
    library(conflicted)
    library(here)
    library(tidyr)
    library(lubridate)
    library(stringr)
    library(scales)
    library(ggplot2)
    library(dplyr)
    
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

df.raw = read_csv("/Users/benjaminmanning/Desktop/ica_weights.csv")
vars = nrow(df.raw)
df <- df.raw |>
    mutate(var = 1:vars) |>
    mutate(var = paste0("Variable ", var)) |>
    mutate(var = factor(var, levels = rev(var))) |>
    pivot_longer(cols = -var, names_to = "ic_name", values_to = "value") |>
    mutate(ic_name = case_when(
        ic_name == "V1" ~ "IC1 (15.4%)",
        ic_name == "V2" ~ "IC2 (12.6%)",
        ic_name == "V3" ~ "IC3 (6.3%)" 
    ))


g = ggplot(df, aes(x = value, y = var)) +
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
g

ggsave(
    filename = "/Users/benjaminmanning/Desktop/ica.png",
    plot = g,
    width = 8,
    height = 3
)
