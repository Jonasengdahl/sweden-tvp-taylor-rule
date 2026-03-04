# R/04_tvp_kalman.R
# =========================================
# 4. TVP Taylor rule: Kalman filter estimation
# =========================================

source("R/00_setup.R")

# -----------------------------------------
# 4A. Load prepared & scaled dataset
# -----------------------------------------
# Load the scaled TVP dataset constructed in 03_tvp_model.R.
tvp_data <- read_csv(
  "output/clean_data/TVP_Taylor_input_prepared_scaled.csv",
  show_col_types = FALSE
)

# Drop any remaining observations with missing values.
tvp_data <- tvp_data %>%
  drop_na(repo_rate, repo_rate_lag1,
          infl_gap_scaled, exp_gap_scaled, slack_scaled)

# Define dependent variable and regressors:
#   y     = repo rate (i_t)
#   lag_y = lagged repo rate (i_{t-1})
#   X     = scaled gaps for inflation, expectations, and slack.
y     <- tvp_data$repo_rate             # i_t
lag_y <- tvp_data$repo_rate_lag1        # i_{t-1}

X <- as.matrix(tvp_data[, c("infl_gap_scaled",
                            "exp_gap_scaled",
                            "slack_scaled")])

T <- length(y)
k <- ncol(X)   # number of time-varying coefficients (α_t, β_t, γ_t)

# -----------------------------------------
# 4B. Kalman filter log-likelihood function
# -----------------------------------------
# Define the negative log-likelihood of the state-space model, evaluated
# via the Kalman filter. This is the objective for maximum likelihood.
kalman_loglik <- function(par, y, lag_y, X) {
  # Parameter vector structure:
  # par = (rho, log(q1), log(q2), log(q3), log(sigma2))
  rho        <- par[1]
  log_q      <- par[2:(1 + k)]      # k elements if k = 3
  log_sigma2 <- par[2 + k]

  Q      <- diag(exp(log_q), k)     # state noise covariance (diagonal)
  sigma2 <- exp(log_sigma2)         # observation noise variance

  # Initial state: diffuse but finite.
  theta_pred <- rep(0, k)           # E[θ_0]
  P_pred     <- diag(10, k)         # Var(θ_0)

  ll <- 0

  for (t in seq_len(length(y))) {
    # Prediction step for variance: P_{t|t-1} = P_{t-1|t-1} + Q
    P_pred <- P_pred + Q

    # Current regressors (1 × k)
    x_t <- X[t, , drop = FALSE]

    # Innovation: v_t = y_t − ρ i_{t-1} − x_t θ_{t|t-1}
    v_t <- y[t] - rho * lag_y[t] - as.numeric(x_t %*% theta_pred)

    # Innovation variance: F_t = x_t P_{t|t-1} x_t' + σ²
    F_t <- as.numeric(x_t %*% P_pred %*% t(x_t) + sigma2)

    # Guard against numerical issues
    if (!is.finite(F_t) || F_t <= 0) {
      return(1e10)
    }

    # Log-likelihood contribution
    ll <- ll - 0.5 * (log(2 * pi) + log(F_t) + (v_t^2) / F_t)

    # Kalman gain: K_t = P_{t|t-1} x_t' F_t^{-1}
    K_t <- P_pred %*% t(x_t) / F_t   # k × 1

    # Update state: θ_{t|t} = θ_{t|t-1} + K_t v_t
    theta_upd <- theta_pred + as.numeric(K_t) * v_t

    # Update covariance: P_{t|t} = P_{t|t-1} − K_t x_t P_{t|t-1}
    P_upd <- P_pred - K_t %*% x_t %*% P_pred

    # Prepare for next iteration (random-walk transition: θ_{t+1|t} = θ_{t|t})
    theta_pred <- theta_upd
    P_pred     <- P_upd
  }

  return(-ll)  # return negative log-likelihood for minimisation
}

# -----------------------------------------
# 4C. Choose starting values for optimisation
# -----------------------------------------
# Set initial guesses for ρ, Q, and σ² before maximising the likelihood.
rho_start       <- 0.8
log_q_start     <- rep(log(0.001), k)                     # small drift in θ_t
log_sigma_start <- log(var(y - rho_start * lag_y))        # rough residual variance

par_start <- c(rho_start, log_q_start, log_sigma_start)

# -----------------------------------------
# 4D. Estimate parameters by maximum likelihood
# -----------------------------------------
# Maximise the Kalman-filter likelihood using BFGS.
est <- optim(
  par    = par_start,
  fn     = kalman_loglik,
  y      = y,
  lag_y  = lag_y,
  X      = X,
  method = "BFGS",
  control = list(maxit = 1000, trace = 1, REPORT = 10)
)

cat("Convergence code (0 = OK):", est$convergence, "\n")
cat("Estimated parameters:\n")
print(est$par)

# Save optimisation output for later use (smoothing and plotting).
saveRDS(est, file = "output/model_objects/tvp_kalman_optim.Rds")


# -----------------------------------------
# 4E. Kalman filter + fixed-interval smoother
# -----------------------------------------
# Reload estimated parameters and reconstruct all objects required for
# the filtering and smoothing steps.

# Load optimisation output (from 4D)
est <- readRDS("output/model_objects/tvp_kalman_optim.Rds")

# Reload the prepared and scaled dataset
tvp_data <- read_csv(
  "output/clean_data/TVP_Taylor_input_prepared_scaled.csv",
  show_col_types = FALSE
) %>%
  drop_na(repo_rate, repo_rate_lag1,
          infl_gap_scaled, exp_gap_scaled, slack_scaled)

# Define variables
y     <- tvp_data$repo_rate
lag_y <- tvp_data$repo_rate_lag1
X     <- as.matrix(tvp_data[, c("infl_gap_scaled",
                                "exp_gap_scaled",
                                "slack_scaled")])

T <- length(y)
k <- ncol(X)

# -----------------------------------------
# Extract estimated hyperparameters
# -----------------------------------------
par_hat    <- est$par
rho_hat    <- par_hat[1]
log_q_hat  <- par_hat[2:(1 + k)]
log_sig2   <- par_hat[2 + k]

Q_hat      <- diag(exp(log_q_hat), k)   # state noise covariance
sigma2_hat <- exp(log_sig2)             # observation variance

cat("Using estimated rho =", rho_hat, "\n")
cat("Estimated Q diag =", exp(log_q_hat), "\n")
cat("Estimated sigma^2 =", sigma2_hat, "\n")

# -----------------------------------------
# Forward pass: Kalman filter
# -----------------------------------------
# Compute filtered estimates θ_{t|t} and their covariance matrices.

theta_filt <- matrix(NA_real_, nrow = T, ncol = k)
P_filt     <- array(NA_real_, dim = c(k, k, T))

# Initial state and covariance
theta_pred <- rep(0, k)        # E[θ_0]
P_pred     <- diag(10, k)      # Var(θ_0)

for (t in seq_len(T)) {

  # Prediction step: add state noise
  P_pred <- P_pred + Q_hat

  x_t <- X[t, , drop = FALSE]

  # Innovation (prediction error)
  v_t <- y[t] - rho_hat * lag_y[t] - as.numeric(x_t %*% theta_pred)

  # Innovation variance
  F_t <- as.numeric(x_t %*% P_pred %*% t(x_t) + sigma2_hat)

  if (!is.finite(F_t) || F_t <= 0) {
    stop("Non-positive innovation variance at t = ", t)
  }

  # Kalman gain
  K_t <- P_pred %*% t(x_t) / F_t

  # Update filtered state
  theta_upd <- theta_pred + as.numeric(K_t) * v_t

  # Update covariance
  P_upd <- P_pred - K_t %*% x_t %*% P_pred

  # Store filtered estimates
  theta_filt[t, ] <- theta_upd
  P_filt[, , t]   <- P_upd

  # Prepare for next iteration
  theta_pred <- theta_upd
  P_pred     <- P_upd
}

# -----------------------------------------
# Backward pass: Rauch–Tung–Striebel smoother
# -----------------------------------------
# Compute smoothed states θ_{t|T} using full-sample information.

theta_smooth <- matrix(NA_real_, nrow = T, ncol = k)
P_smooth     <- array(NA_real_, dim = c(k, k, T))

# Initialise at the final period
theta_smooth[T, ] <- theta_filt[T, ]
P_smooth[, , T]   <- P_filt[, , T]

for (t in (T-1):1) {

  P_t_filt  <- P_filt[, , t]
  P_t1_pred <- P_t_filt + Q_hat

  # Smoother gain
  J_t <- P_t_filt %*% solve(P_t1_pred)

  # Update smoothed state
  theta_smooth[t, ] <- theta_filt[t, ] +
    J_t %*% (theta_smooth[t + 1, ] - theta_filt[t + 1, ])

  # Update smoothed covariance
  P_smooth[, , t] <- P_t_filt +
    J_t %*% (P_smooth[, , t + 1] - P_t1_pred) %*% t(J_t)
}

# -----------------------------------------
# 4F. Save smoothed and filtered state results
# -----------------------------------------
# Store smoothed coefficients in a tidy table for plotting and analysis.

theta_smooth_df <- tibble(
  Date    = tvp_data$Date,
  alpha_t = theta_smooth[, 1],  # response to inflation gap
  beta_t  = theta_smooth[, 2],  # response to expectations gap
  gamma_t = theta_smooth[, 3]   # response to slack
)

print("Head of smoothed coefficients:")
print(head(theta_smooth_df))

# Save smoothed coefficient table
write_csv(
  theta_smooth_df,
  "output/clean_data/TVP_coefficients_smoothed.csv"
)

# Save full state-space objects for later use
saveRDS(
  list(
    theta_smooth = theta_smooth,
    P_smooth     = P_smooth,
    theta_filt   = theta_filt,
    P_filt       = P_filt,
    dates        = tvp_data$Date
  ),
  file = "output/model_objects/tvp_kalman_states.Rds"
)

cat("Smoothed TVP paths (and full state matrices) saved successfully.\n")

# -----------------------------------------
# 4G. Plot smoothed time-varying coefficients
# -----------------------------------------
# Plot α_t, β_t, γ_t in separate stacked panels.

tvp_coeff <- theta_smooth_df

tvp_coeff_long <- tvp_coeff %>%
  pivot_longer(
    cols      = c(alpha_t, beta_t, gamma_t),
    names_to  = "Coefficient",
    values_to = "Value"
  )

coeff_plot <- ggplot(tvp_coeff_long, aes(x = Date, y = Value)) +
  geom_line(linewidth = 0.7) +
  facet_wrap(
    ~ Coefficient,
    scales = "free_y",
    ncol = 1
  ) +
  labs(
    title = "Time-Varying Taylor Rule Coefficients (Smoothed)",
    x     = NULL,
    y     = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    strip.text       = element_text(size = 12, face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save plot
ggsave(
  filename = "figures/raw/tvp_coefficients_smoothed.png",
  plot     = coeff_plot,
  width    = 8,
  height   = 10,
  dpi      = 300
)

print(coeff_plot)
