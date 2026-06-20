library(tidyverse)
library(here)

tensors <- readRDS(here::here("data", "processed", "pm25_tensors.rds"))
meta    <- tensors$meta
src     <- tensors$source_array

pm_scaled <- src[, , , 1]
nx <- dim(pm_scaled)[1]
ny <- dim(pm_scaled)[2]
nt <- dim(pm_scaled)[3]

test_idx <- meta$split_idx$test
win      <- meta$window_size
n_test   <- length(test_idx)

inv_scale <- function(x) {
  unscaled <- x * (meta$pm25_max - meta$pm25_min) + meta$pm25_min
  if (isTRUE(meta$log_transform)) expm1(unscaled) else unscaled
}

target_days <- test_idx + win
fit_end     <- target_days[1] - 1

cat(sprintf("Fit days 1..%d; %d test targets %d..%d\n",
            fit_end, n_test, target_days[1], target_days[n_test]))

actuals_scaled <- array(NA_real_, dim = c(n_test, nx, ny))
for (k in seq_len(n_test)) actuals_scaled[k, , ] <- pm_scaled[, , target_days[k]]
actuals <- inv_scale(actuals_scaled)

y_fit   <- pm_scaled[, , 1:fit_end]
Tfit    <- dim(y_fit)[3]
y_t     <- y_fit[, , 2:Tfit]
y_lag   <- y_fit[, , 1:(Tfit - 1)]

mean_t   <- apply(y_t,   c(1, 2), mean)
mean_lag <- apply(y_lag, c(1, 2), mean)
cov_xy   <- apply(y_t * y_lag, c(1, 2), mean) - mean_t * mean_lag
var_x    <- apply(y_lag^2,     c(1, 2), mean) - mean_lag^2

stopifnot("Zero-variance cell in AR(1) fit window" = all(var_x > 0))
phi_ar   <- cov_xy / var_x
alpha_ar <- mean_t - phi_ar * mean_lag

ar1_preds_scaled <- array(NA_real_, dim = c(n_test, nx, ny))
for (k in seq_len(n_test)) {
  y_prev <- pm_scaled[, , target_days[k] - 1]
  ar1_preds_scaled[k, , ] <- alpha_ar + phi_ar * y_prev
}
ar1_preds <- pmax(inv_scale(ar1_preds_scaled), 0)

rmse_ar1 <- sqrt(mean((ar1_preds - actuals)^2, na.rm = TRUE))
mae_ar1  <- mean(abs(ar1_preds - actuals), na.rm = TRUE)

cat(sprintf("\nAR(1) per cell:  RMSE = %.3f µg/m³, MAE = %.3f\n", rmse_ar1, mae_ar1))
cat(sprintf("  mean phi = %.3f (persistence-equivalent = 1)\n",
            mean(phi_ar, na.rm = TRUE)))

spatial_lag <- function(mat) {
  nx <- nrow(mat); ny <- ncol(mat)
  padded <- matrix(NA_real_, nx + 2, ny + 2)
  padded[2:(nx + 1), 2:(ny + 1)] <- mat
  total <- matrix(0, nx, ny)
  count <- matrix(0L, nx, ny)
  for (di in -1:1) for (dj in -1:1) {
    if (di == 0 && dj == 0) next
    sh <- padded[(2 + di):(nx + 1 + di), (2 + dj):(ny + 1 + dj)]
    ok <- !is.na(sh)
    total <- total + ifelse(ok, sh, 0)
    count <- count + ok
  }
  total / count
}

cat("\nComputing spatial lags ...\n")
Wy <- array(NA_real_, dim = c(nx, ny, nt))
for (t in seq_len(nt)) Wy[, , t] <- spatial_lag(pm_scaled[, , t])

y_train    <- as.vector(pm_scaled[, , 2:fit_end])
ylag_train <- as.vector(pm_scaled[, , 1:(fit_end - 1)])
wylag_train <- as.vector(Wy[, , 1:(fit_end - 1)])

fit_star <- lm(y_train ~ ylag_train + wylag_train)
coefs    <- coef(fit_star)

cat("STAR coefficients:\n"); print(coefs)

star_preds_scaled <- array(NA_real_, dim = c(n_test, nx, ny))
for (k in seq_len(n_test)) {
  y_prev  <- pm_scaled[, , target_days[k] - 1]
  wy_prev <- Wy[, , target_days[k] - 1]
  star_preds_scaled[k, , ] <- coefs[1] + coefs[2] * y_prev + coefs[3] * wy_prev
}
star_preds <- pmax(inv_scale(star_preds_scaled), 0)

rmse_star <- sqrt(mean((star_preds - actuals)^2, na.rm = TRUE))
mae_star  <- mean(abs(star_preds - actuals), na.rm = TRUE)
cat(sprintf("\nSTAR (pooled):   RMSE = %.3f µg/m³, MAE = %.3f\n", rmse_star, mae_star))

saveRDS(
  list(
    ar1  = list(predictions = ar1_preds,  rmse = rmse_ar1,  mae = mae_ar1,
                phi = phi_ar, alpha = alpha_ar),
    star = list(predictions = star_preds, rmse = rmse_star, mae = mae_star,
                coefs = coefs),
    actuals = actuals
  ),
  here::here("data", "processed", "baselines_results.rds")
)

cat("\nSaved: data/processed/baselines_results.rds\n")
