# R/03_tvp_model.R
# =========================================
# Prepare variables for TVP model + tests + scaling
# =========================================

source("R/00_setup.R")

# -----------------------------------------
# 3A. Construct TVP variables
# -----------------------------------------
# Load merged dataset with repo, CPIF, unemployment, expectations.
tvp_data <- read_csv(
  "output/clean_data/TVP_Taylor_input_2010_2024.csv",
  show_col_types = FALSE
)

# 1) Construct lagged repo rate i_{t-1}
tvp_data <- tvp_data %>%
  arrange(Date) %>%
  mutate(
    repo_rate_lag1 = lag(repo_rate, 1)
  )

# 2) Construct inflation and expectations gaps (π_t - π*, exp_t - π*)
#    with π* = 2% inflation target.
tvp_data <- tvp_data %>%
  mutate(
    infl_gap = cpif_yoy - 2,
    exp_gap  = cpi_expectation_1y - 2
  )

# 3) Define slack variable
#    Here: unemployment rate in levels.
tvp_data <- tvp_data %>%
  mutate(
    slack = unemployment_rate
  )

# 4) Drop the first observation lost to the lag
tvp_data <- tvp_data %>% drop_na(repo_rate_lag1)

# 5) Save prepared dataset for TVP estimation (unscaled)
write_csv(
  tvp_data,
  "output/clean_data/TVP_Taylor_input_prepared.csv"
)

print("Prepared TVP dataset saved successfully.")
print(head(tvp_data))

# =========================================
# 3B. Stationarity checks (ADF + KPSS)
# =========================================
# Perform unit-root and stationarity tests for documentation.

library(urca)    # for ADF (ur.df)
library(tseries) # for KPSS

# Select the variables to test
vars_to_test <- tvp_data %>%
  select(repo_rate, repo_rate_lag1, infl_gap, exp_gap, slack)

# Run ADF test and summarise result for one series
adf_summary <- function(x) {
  test <- ur.df(x, type = "drift", selectlags = "AIC")
  stat <- test@teststat["statistic", 1]
  crit <- test@cval[1, "5pct"]
  decision <- ifelse(
    stat < crit,
    "Reject unit root (stationary)",
    "Fail to reject (non-stationary)"
  )
  return(c(adf_stat = stat, crit_5pct = crit, decision = decision))
}

# Run KPSS test and summarise result for one series
kpss_summary <- function(x) {
  test <- kpss.test(x, null = "Level")
  stat <- test$statistic
  crit <- test$critical["5%"]
  decision <- ifelse(
    stat < crit,
    "Fail to reject stationarity",
    "Reject (non-stationary)"
  )
  return(c(kpss_stat = stat, crit_5pct = crit, decision = decision))
}

# Apply tests to all selected variables
adf_results  <- lapply(vars_to_test, adf_summary)
kpss_results <- lapply(vars_to_test, kpss_summary)

# Convert results to data frames
adf_df <- data.frame(
  Variable = names(vars_to_test),
  do.call(rbind, adf_results),
  row.names = NULL
)

kpss_df <- data.frame(
  Variable = names(vars_to_test),
  do.call(rbind, kpss_results),
  row.names = NULL
)

# Print results to console for inspection
print("ADF test results:")
print(adf_df)

print("KPSS test results:")
print(kpss_df)

# Save test results for replication and documentation
write_csv(adf_df,  "output/clean_data/ADF_results_TVP.csv")
write_csv(kpss_df, "output/clean_data/KPSS_results_TVP.csv")

# =========================================
# 3C. Scale predictors for Kalman estimation
# =========================================
# Standardise regressors to mean 0 and variance 1 to stabilise
# the likelihood maximisation and improve numerical conditioning.

tvp_data <- tvp_data %>%
  mutate(
    infl_gap_scaled  = as.numeric(scale(infl_gap)),
    exp_gap_scaled   = as.numeric(scale(exp_gap)),
    slack_scaled     = as.numeric(scale(slack)),
    repo_lag1_scaled = as.numeric(scale(repo_rate_lag1))
  )

# Save scaled dataset used in the Kalman filter estimation
write_csv(
  tvp_data,
  "output/clean_data/TVP_Taylor_input_prepared_scaled.csv"
)

print("Scaled TVP dataset saved successfully.")
print(head(tvp_data))
