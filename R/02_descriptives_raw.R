# R/02_descriptives_raw.R
# =========================================
# Produce raw time-series plot + basic descriptives
# =========================================

source("R/00_setup.R")

# -----------------------------------------
# Load merged clean dataset
# -----------------------------------------
tvp_data <- read_csv(
  "output/clean_data/TVP_Taylor_input_2010_2024.csv",
  show_col_types = FALSE
)

# -----------------------------------------
# 1) Plot raw time series (levels)
# -----------------------------------------
# Stack all four variables into long format and
# plot them in separate panels with free y-scales.
plot_data_raw <- tvp_data %>%
  pivot_longer(
    cols = c(repo_rate, cpif_yoy, unemployment_rate, cpi_expectation_1y),
    names_to  = "Variable",
    values_to = "Value"
  )

raw_plot <- ggplot(plot_data_raw, aes(x = Date, y = Value)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 2) +
  labs(
    title    = "Raw Time Series (Sweden, 2010–2024)",
    subtitle = "Repo rate, CPIF inflation (YoY), unemployment rate, and 1-year CPI expectations",
    x        = NULL,
    y        = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    plot.title      = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save figure used in the paper (Data section)
ggsave(
  filename = "figures/raw/raw_series_2010_2024.png",
  plot     = raw_plot,
  width    = 10,
  height   = 6,
  dpi      = 300
)

print(raw_plot)

# -----------------------------------------
# 2) Descriptive statistics table
# -----------------------------------------
# Compute mean, standard deviation, min, and max for
# each series and reshape to a tidy table for export.
desc_table <- tvp_data %>%
  select(repo_rate, cpif_yoy, unemployment_rate, cpi_expectation_1y) %>%
  summarise(
    across(
      everything(),
      list(
        mean = ~mean(.x, na.rm = TRUE),
        sd   = ~sd(.x,   na.rm = TRUE),
        min  = ~min(.x,  na.rm = TRUE),
        max  = ~max(.x,  na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    )
  ) %>%
  pivot_longer(
    cols         = everything(),
    names_to     = c("Variable", "Statistic"),
    names_pattern = "^(.*)_(mean|sd|min|max)$",
    values_to    = "Value"
  ) %>%
  pivot_wider(
    names_from  = Statistic,
    values_from = Value
  ) %>%
  arrange(Variable)

print(desc_table)

write_csv(
  desc_table,
  "output/clean_data/descriptives_table_2010_2024.csv"
)

# -----------------------------------------
# 3) Correlation matrix for main variables
# -----------------------------------------
# Compute and export the correlation matrix of the four series.
corr_mat <- tvp_data %>%
  select(repo_rate, cpif_yoy, unemployment_rate, cpi_expectation_1y) %>%
  cor(use = "pairwise.complete.obs")

print(round(corr_mat, 3))

write_csv(
  as.data.frame(corr_mat),
  "output/clean_data/correlation_matrix_2010_2024.csv"
)
