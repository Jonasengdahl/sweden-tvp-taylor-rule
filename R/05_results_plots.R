# R/05_results_plots.R
# =========================================
# 5. Interpretation & final results
# =========================================
# Use the smoothed TVP coefficients to:
#   - Merge back with the underlying data
#   - Define simple monetary policy regimes
#   - Compute regime-average coefficients
#   - Identify global min/max of β_t
#   - Prepare inputs for final figures and tables

source("R/00_setup.R")

# Ensure folder for final figures exists
dir.create("figures/results", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------
# 5A. Load TVP coefficients and merge with data
# -----------------------------------------
# Load smoothed coefficients from the Kalman smoother
theta_smooth_df <- read_csv(
  "output/clean_data/TVP_coefficients_smoothed.csv",
  show_col_types = FALSE
)

# Load prepared (scaled) TVP dataset
tvp_data <- read_csv(
  "output/clean_data/TVP_Taylor_input_prepared_scaled.csv",
  show_col_types = FALSE
) %>%
  drop_na(repo_rate, repo_rate_lag1,
          infl_gap_scaled, exp_gap_scaled, slack_scaled)

# Merge data and coefficients, and construct calendar year
tvp_full <- tvp_data %>%
  select(
    Date,
    repo_rate,
    repo_rate_lag1,
    infl_gap_scaled,
    exp_gap_scaled,
    slack_scaled
  ) %>%
  left_join(theta_smooth_df, by = "Date") %>%
  mutate(Year = year(Date))

# -----------------------------------------
# 5B. Define monetary policy regimes
# -----------------------------------------
# Classify each month into three regimes:
#   1) 2010–2014: post-crisis, low inflation
#   2) 2015–2019: negative rates and QE
#   3) 2020–2024: pandemic and inflation surge

tvp_full <- tvp_full %>%
  mutate(
    regime = case_when(
      Year >= 2010 & Year <= 2014 ~ "2010–2014: Post-crisis, low inflation",
      Year >= 2015 & Year <= 2019 ~ "2015–2019: Negative rates & QE",
      Year >= 2020 & Year <= 2024 ~ "2020–2024: Pandemic & inflation surge",
      TRUE ~ "Other"
    )
  )

# -----------------------------------------
# 5C. Regime-average TVP coefficients (table)
# -----------------------------------------
# Compute regime means of α_t, β_t, γ_t and the number of observations.

regime_summary <- tvp_full %>%
  filter(regime != "Other") %>%
  group_by(regime) %>%
  summarise(
    n_obs      = n(),
    alpha_mean = mean(alpha_t, na.rm = TRUE),
    beta_mean  = mean(beta_t,  na.rm = TRUE),
    gamma_mean = mean(gamma_t, na.rm = TRUE),
    .groups    = "drop"
  ) %>%
  arrange(regime)

print("Regime-average TVP coefficients:")
print(regime_summary)

# Save regime summary for LaTeX table
write_csv(
  regime_summary,
  "output/clean_data/TVP_coefficients_regime_means.csv"
)

# -----------------------------------------
# 5D. Extremes of the expectations coefficient β_t
# -----------------------------------------
# Identify the global minimum and maximum of β_t and the dates at which
# they occur.

beta_extremes <- tvp_full %>%
  summarise(
    beta_min = min(beta_t, na.rm = TRUE),
    beta_max = max(beta_t, na.rm = TRUE)
  )

print("Global min/max of beta_t:")
print(beta_extremes)

beta_extreme_dates <- tvp_full %>%
  filter(
    beta_t == min(beta_t, na.rm = TRUE) |
    beta_t == max(beta_t, na.rm = TRUE)
  ) %>%
  arrange(Date) %>%
  select(Date, beta_t, regime)

print("Dates at which beta_t hits its global min / max:")
print(beta_extreme_dates)

# Save dates of β_t extremes for LaTeX table
write_csv(
  beta_extreme_dates,
  "output/clean_data/TVP_beta_extremes_dates.csv"
)

# -----------------------------------------
# 5E. Plot coefficients with color-coded regimes
# -----------------------------------------
# Plot α_t, β_t, γ_t with regime-colored background bands.

regime_bands <- tibble(
  regime_short = c("2010–2014", "2015–2019", "2020–2024"),
  start        = as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")),
  end          = as.Date(c("2014-12-31", "2019-12-31", "2024-12-31")),
  fill_color   = c("#e6f2ff", "#fef4d4", "#f2e6ff")   # blue, yellow, purple
)

tvp_coeff_long <- tvp_full %>%
  select(Date, alpha_t, beta_t, gamma_t) %>%
  pivot_longer(
    cols      = c(alpha_t, beta_t, gamma_t),
    names_to  = "Coefficient",
    values_to = "Value"
  )

coeff_plot_regime <- ggplot() +
  geom_rect(
    data = regime_bands,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = fill_color),
    alpha = 0.6,
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +   # use fill_color as provided
  geom_line(
    data = tvp_coeff_long,
    aes(x = Date, y = Value),
    linewidth = 0.7
  ) +
  facet_wrap(~ Coefficient, scales = "free_y", ncol = 1) +
  labs(
    title = "Time-Varying Taylor Rule Coefficients with Policy Regimes",
    x     = NULL,
    y     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    strip.text       = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "figures/results/tvp_coefficients_regimes.png",
  plot     = coeff_plot_regime,
  width    = 8,
  height   = 10,
  dpi      = 300
)

print(coeff_plot_regime)


# -----------------------------------------
# 5F. Zoom-in on expectations coefficient β_t
# -----------------------------------------
# Plot β_t alone with the same color-coded regimes and mark global min/max.

beta_zoom_plot <- ggplot() +
  geom_rect(
    data        = regime_bands,
    aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf, fill = fill_color),
    alpha       = 0.6,
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  geom_line(
    data = tvp_full,
    aes(x = Date, y = beta_t),
    linewidth = 0.7
  ) +
  geom_point(
    data = beta_extreme_dates,
    aes(x = Date, y = beta_t),
    size = 2
  ) +
  labs(
    title = "Time-Varying Response to Inflation Expectations (β[t])",
    x     = NULL,
    y     = "Coefficient on expectations (β[t])"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.background = element_rect(fill = "white", colour = NA),
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "figures/results/tvp_beta_zoom.png",
  plot     = beta_zoom_plot,
  width    = 8,
  height   = 4,
  dpi      = 300
)

print(beta_zoom_plot)


# -----------------------------------------
# 5G. Actual vs TVP-implied policy rate
# -----------------------------------------
# Reconstruct the fitted policy rate from the estimated TVP rule:
#   i_hat_t = ρ_hat * i_{t-1}
#             + α_t * infl_gap_scaled
#             + β_t * exp_gap_scaled
#             + γ_t * slack_scaled
# Compute residuals and simple in-sample fit statistics (RMSE, MAE),
# and plot observed vs fitted repo rate.

# Reload estimated parameters from the Kalman optimisation
est    <- readRDS("output/model_objects/tvp_kalman_optim.Rds")
par_hat <- est$par
rho_hat <- par_hat[1]   # estimated interest-rate smoothing parameter

# Construct fitted values and residuals
tvp_fit <- tvp_full %>%
  filter(!is.na(alpha_t), !is.na(beta_t), !is.na(gamma_t)) %>%
  mutate(
    repo_fitted = rho_hat * repo_rate_lag1 +
      alpha_t * infl_gap_scaled +
      beta_t  * exp_gap_scaled +
      gamma_t * slack_scaled,
    repo_resid  = repo_rate - repo_fitted
  )

# Compute in-sample fit statistics
fit_summary <- tvp_fit %>%
  summarise(
    RMSE = sqrt(mean(repo_resid^2, na.rm = TRUE)),
    MAE  = mean(abs(repo_resid), na.rm = TRUE)
  )

print("In-sample fit of TVP Taylor rule (repo_rate):")
print(fit_summary)

write_csv(
  fit_summary,
  "output/clean_data/TVP_fit_summary.csv"
)

# Plot actual vs fitted policy rate
fit_plot <- ggplot(tvp_fit, aes(x = Date)) +
  geom_line(
    aes(y = repo_rate),
    linewidth = 0.7
  ) +
  geom_line(
    aes(y = repo_fitted),
    linewidth = 0.7,
    linetype  = "dashed"
  ) +
  labs(
    title = "Actual vs TVP-Implied Policy Rate",
    x     = NULL,
    y     = "Repo rate (percent)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank()
  )

ggsave(
  filename = "figures/results/tvp_fit_repo.png",
  plot     = fit_plot,
  width    = 8,
  height   = 4,
  dpi      = 300
)

print(fit_plot)
