library(tidyverse)
library(stars)
library(sf)
library(spdep)
library(rnaturalearth)
library(rnaturalearthdata)
library(here)

source(here::here("code", "_common_training.R"))

GRID_NX <- 110L
GRID_NY <- 60L
LON_CENTERS <- 13.95 + seq_len(GRID_NX) * 0.1 - 0.05
LAT_CENTERS <- 55.05 + seq_len(GRID_NY) * (-0.1) + 0.05

borders_sf <- rnaturalearth::ne_countries(scale = "medium",
                           country = c("Poland", "Germany", "Czech Republic",
                                       "Slovakia", "Ukraine", "Belarus",
                                       "Lithuania", "Russia", "Austria"),
                           returnclass = "sf")
poland_sf <- borders_sf %>% dplyr::filter(admin == "Poland")

grid_to_df <- function(mat, value_name = "value") {
  df <- expand.grid(lon = LON_CENTERS, lat = LAT_CENTERS)
  df[[value_name]] <- as.vector(mat)
  df
}

cities_df <- tibble::tibble(
  name = c("Warszawa", "Kraków", "Gdańsk", "Wrocław", "Poznań", "Katowice"),
  lon  = c(21.01, 19.94, 18.65, 17.04, 16.93, 19.03),
  lat  = c(52.23, 50.06, 54.35, 51.11, 52.41, 50.26)
)

results  <- readRDS(here::here("data", "processed", "cnn_lstm_skip_test_results.rds"))
headline <- readRDS(here::here("data", "processed", "convlstm_test_results.rds"))
tensors  <- readRDS(here::here("data", "processed", "pm25_tensors.rds"))
meta     <- tensors$meta

preds   <- headline$predictions
actuals <- headline$actuals
errors  <- preds - actuals

cat("=== Evaluation ===\n")
cat(sprintf("  Test samples: %d\n", dim(preds)[1]))
cat(sprintf("  Headline model (ConvLSTM, skip): RMSE %.2f µg/m³, MAE %.2f µg/m³\n",
            headline$rmse, headline$mae))
cat(sprintf("  CNN-LSTM (skip) reference row:   RMSE %.2f µg/m³, MAE %.2f µg/m³\n",
            results$rmse, results$mae))

out_fig <- here::here("output", "figures")
out_tab <- here::here("output", "tables")
if (!dir.exists(out_fig)) dir.create(out_fig, recursive = TRUE)
if (!dir.exists(out_tab)) dir.create(out_tab, recursive = TRUE)

inv_scale   <- make_inv_scale(meta)
persistence <- persistence_pm25(tensors, meta, inv_scale)

rmse_persist <- sqrt(mean((persistence - actuals)^2, na.rm = TRUE))
mae_persist  <- mean(abs(persistence - actuals), na.rm = TRUE)

Y_train_pm25 <- inv_scale(tensors$Y_train)
climatology_cell <- apply(Y_train_pm25, c(2, 3), mean, na.rm = TRUE)
climatology_grid <- array(rep(as.vector(climatology_cell), times = dim(actuals)[1]),
                          dim = c(dim(actuals)[2], dim(actuals)[3], dim(actuals)[1]))
climatology_grid <- aperm(climatology_grid, c(3, 1, 2))
rmse_spatial_mean <- sqrt(mean((climatology_grid - actuals)^2, na.rm = TRUE))
mae_spatial_mean  <- mean(abs(climatology_grid - actuals), na.rm = TRUE)

bl_file         <- here::here("data", "processed", "baselines_results.rds")
noskip_file     <- here::here("data", "processed", "noskip_test_results.rds")
lstm_file       <- here::here("data", "processed", "lstm_only_test_results.rds")
lstm_ns_file    <- here::here("data", "processed", "lstm_only_noskip_test_results.rds")
convlstm_file   <- here::here("data", "processed", "convlstm_test_results.rds")
convlstm_ns_file <- here::here("data", "processed", "convlstm_noskip_test_results.rds")
baselines   <- if (file.exists(bl_file))          readRDS(bl_file)          else NULL
noskip      <- if (file.exists(noskip_file))      readRDS(noskip_file)      else NULL
lstm_only   <- if (file.exists(lstm_file))        readRDS(lstm_file)        else NULL
lstm_only_ns <- if (file.exists(lstm_ns_file))    readRDS(lstm_ns_file)     else NULL
convlstm    <- if (file.exists(convlstm_file))    readRDS(convlstm_file)    else NULL
convlstm_ns <- if (file.exists(convlstm_ns_file)) readRDS(convlstm_ns_file) else NULL

acc_rows <- list(
  tibble::tibble(Model = "CNN-LSTM (skip)",     RMSE = results$rmse, MAE = results$mae)
)
if (!is.null(convlstm)) acc_rows <- c(acc_rows, list(
  tibble::tibble(Model = "ConvLSTM (skip)",     RMSE = convlstm$rmse,   MAE = convlstm$mae)))
if (!is.null(convlstm_ns)) acc_rows <- c(acc_rows, list(
  tibble::tibble(Model = "ConvLSTM (no skip)",  RMSE = convlstm_ns$rmse, MAE = convlstm_ns$mae)))
if (!is.null(lstm_only)) acc_rows <- c(acc_rows, list(
  tibble::tibble(Model = "Per-cell LSTM (skip)", RMSE = lstm_only$rmse, MAE = lstm_only$mae)))
if (!is.null(lstm_only_ns)) acc_rows <- c(acc_rows, list(
  tibble::tibble(Model = "Per-cell LSTM (no skip)", RMSE = lstm_only_ns$rmse, MAE = lstm_only_ns$mae)))
if (!is.null(noskip)) acc_rows <- c(acc_rows, list(
  tibble::tibble(Model = "CNN-LSTM (no skip)",  RMSE = noskip$rmse,          MAE = noskip$mae)))
if (!is.null(baselines)) acc_rows <- c(acc_rows, list(
  tibble::tibble(Model = "STAR (spatial+lag)",  RMSE = baselines$star$rmse,  MAE = baselines$star$mae),
  tibble::tibble(Model = "AR(1) per cell",      RMSE = baselines$ar1$rmse,   MAE = baselines$ar1$mae)))
acc_rows <- c(acc_rows, list(
  tibble::tibble(Model = "Persistence (naive)",            RMSE = rmse_persist,          MAE = mae_persist),
  tibble::tibble(Model = "Training climatology (per cell)", RMSE = rmse_spatial_mean,    MAE = mae_spatial_mean)))

accuracy <- dplyr::bind_rows(acc_rows) %>%
  dplyr::mutate(`RMSE vs persistence` = sprintf("%+.1f%%", (1 - RMSE / rmse_persist) * 100))

cat("\n=== Accuracy Comparison ===\n")
print(accuracy)
readr::write_csv(accuracy, file.path(out_tab, "accuracy_comparison.csv"))

exceedance_metrics <- function(preds_arr, actuals_arr, thr = 15) {
  a <- actuals_arr > thr; p <- preds_arr > thr
  tp <- sum(p &  a, na.rm = TRUE)
  fp <- sum(p & !a, na.rm = TRUE)
  fn <- sum(!p & a, na.rm = TRUE)
  tibble::tibble(actual_rate    = mean(a, na.rm = TRUE),
         predicted_rate = mean(p, na.rm = TRUE),
         recall         = if ((tp + fn) == 0) NA_real_ else tp / (tp + fn),
         precision      = if ((tp + fp) == 0) NA_real_ else tp / (tp + fp))
}

exc_rows <- list(
  dplyr::bind_cols(tibble::tibble(Model = "CNN-LSTM (skip)"),
            exceedance_metrics(results$predictions, actuals))
)
if (!is.null(convlstm)) exc_rows <- c(exc_rows, list(
  dplyr::bind_cols(tibble::tibble(Model = "ConvLSTM (skip)"),
            exceedance_metrics(convlstm$predictions, actuals))))
if (!is.null(convlstm_ns)) exc_rows <- c(exc_rows, list(
  dplyr::bind_cols(tibble::tibble(Model = "ConvLSTM (no skip)"),
            exceedance_metrics(convlstm_ns$predictions, actuals))))
if (!is.null(lstm_only)) exc_rows <- c(exc_rows, list(
  dplyr::bind_cols(tibble::tibble(Model = "Per-cell LSTM (skip)"),
            exceedance_metrics(lstm_only$predictions, actuals))))
if (!is.null(lstm_only_ns)) exc_rows <- c(exc_rows, list(
  dplyr::bind_cols(tibble::tibble(Model = "Per-cell LSTM (no skip)"),
            exceedance_metrics(lstm_only_ns$predictions, actuals))))
if (!is.null(noskip)) exc_rows <- c(exc_rows, list(
  dplyr::bind_cols(tibble::tibble(Model = "CNN-LSTM (no skip)"),
            exceedance_metrics(noskip$predictions, actuals))))
if (!is.null(baselines)) exc_rows <- c(exc_rows, list(
  dplyr::bind_cols(tibble::tibble(Model = "STAR (spatial+lag)"),
            exceedance_metrics(baselines$star$predictions, actuals)),
  dplyr::bind_cols(tibble::tibble(Model = "AR(1) per cell"),
            exceedance_metrics(baselines$ar1$predictions, actuals))))
exc_rows <- c(exc_rows, list(
  dplyr::bind_cols(tibble::tibble(Model = "Persistence"),
            exceedance_metrics(persistence, actuals))))

exceedance <- dplyr::bind_rows(exc_rows)
cat("\n=== Exceedance skill (>15 µg/m³, WHO 2021 AQG 24h, daily cells) ===\n")
print(exceedance)
readr::write_csv(exceedance, file.path(out_tab, "exceedance_skill.csv"))

rmse_daily <- sapply(1:dim(preds)[1], function(i) {
  sqrt(mean((preds[i, , ] - actuals[i, , ])^2, na.rm = TRUE))
})

test_times_samples <- meta$times[meta$split_idx$test + meta$window_size]

p_rmse_ts <- ggplot2::ggplot(
  tibble::tibble(time = test_times_samples, rmse = rmse_daily),
  ggplot2::aes(x = time, y = rmse)
) +
  ggplot2::geom_line(color = "steelblue", alpha = 0.7) +
  ggplot2::geom_smooth(method = "loess", span = 0.2, se = FALSE, color = "tomato") +
  ggplot2::labs(title = "ConvLSTM (skip) Forecast Error Over Time (Test Set)",
       y = expression(RMSE~"["*mu*g/m^3*"]"),
       x = NULL) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "test_rmse_timeseries.png"), p_rmse_ts,
       width = 10, height = 5)

mae_grid <- headline$mae_grid
bias_grid <- apply(errors, c(2, 3), mean, na.rm = TRUE)

mae_df <- grid_to_df(mae_grid, "mae")

p_mae_spatial <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = mae_df, ggplot2::aes(lon, lat, fill = mae)) +
  ggplot2::geom_sf(data = borders_sf, fill = NA, colour = "grey25", linewidth = 0.3) +
  ggplot2::geom_sf(data = poland_sf, fill = NA, colour = "black", linewidth = 0.6) +
  ggplot2::geom_point(data = cities_df, ggplot2::aes(lon, lat), colour = "white", size = 1.6) +
  ggplot2::geom_text(data = cities_df, ggplot2::aes(lon, lat, label = name),
            colour = "white", size = 3, nudge_y = 0.12) +
  ggplot2::scale_fill_viridis_c(name = expression(MAE~"["*mu*g/m^3*"]"),
                       option = "inferno") +
  ggplot2::coord_sf(xlim = range(LON_CENTERS), ylim = range(LAT_CENTERS), expand = FALSE) +
  ggplot2::labs(title = "Spatial distribution of ConvLSTM (skip) forecast error (MAE)",
       subtitle = "Headline model, averaged across the 270-day test set",
       x = NULL, y = NULL) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "test_mae_spatial.png"), p_mae_spatial,
       width = 8, height = 6, dpi = 150)

example_idx <- which.max(rmse_daily)
example_date <- test_times_samples[example_idx]

shared_limits <- range(c(actuals[example_idx, , ], preds[example_idx, , ]),
                       na.rm = TRUE)

map_layer <- function(df, value_col, title, scale_type = c("viridis", "diverging")) {
  scale_type <- match.arg(scale_type)
  g <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = df, ggplot2::aes(lon, lat, fill = .data[[value_col]])) +
    ggplot2::geom_sf(data = borders_sf, fill = NA, colour = "grey30", linewidth = 0.3) +
    ggplot2::geom_sf(data = poland_sf, fill = NA, colour = "black", linewidth = 0.6) +
    ggplot2::coord_sf(xlim = range(LON_CENTERS), ylim = range(LAT_CENTERS), expand = FALSE) +
    ggplot2::labs(title = title, x = NULL, y = NULL) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  if (scale_type == "viridis") {
    g + ggplot2::scale_fill_viridis_c(name = expression(mu*g/m^3),
                             limits = shared_limits)
  } else {
    g + ggplot2::scale_fill_gradient2(name = expression(mu*g/m^3),
                             low = "#2166ac", mid = "white", high = "#b2182b",
                             midpoint = 0)
  }
}

p_actual <- map_layer(grid_to_df(actuals[example_idx, , ], "v"), "v",
                      sprintf("Actual — %s", example_date))
p_pred   <- map_layer(grid_to_df(preds[example_idx, , ], "v"), "v",
                      sprintf("Predicted — %s", example_date))
p_err    <- map_layer(grid_to_df(errors[example_idx, , ], "v"), "v",
                      sprintf("Error (pred − actual)"),
                      scale_type = "diverging")

p_example <- cowplot::plot_grid(p_actual, p_pred, p_err, ncol = 3)

ggplot2::ggsave(file.path(out_fig, "test_example_prediction.png"), p_example,
       width = 15, height = 6, dpi = 150)

nx <- dim(errors)[2]
ny <- dim(errors)[3]
coords <- expand.grid(lon = LON_CENTERS, lat = LAT_CENTERS)
knn <- spdep::knearneigh(as.matrix(coords), k = 8)
nb  <- spdep::knn2nb(knn)
lw  <- spdep::nb2listw(nb, style = "W")

mean_errors <- apply(errors, c(2, 3), mean, na.rm = TRUE)
error_vec   <- as.vector(mean_errors)
valid       <- !is.na(error_vec)
coords_valid <- coords[valid, ]
error_valid  <- error_vec[valid]

if (sum(valid) < length(error_vec)) {
  knn_v <- spdep::knearneigh(as.matrix(coords_valid), k = 8)
  lw_mean <- spdep::nb2listw(spdep::knn2nb(knn_v), style = "W")
} else {
  lw_mean <- lw
}
moran_mean <- spdep::moran.test(error_valid, lw_mean)

n_test_days  <- dim(errors)[1]
moran_per_day <- numeric(n_test_days)
for (d in seq_len(n_test_days)) {
  err_day <- as.vector(errors[d, , ])
  valid_d <- !is.na(err_day)
  if (sum(valid_d) < 10) { moran_per_day[d] <- NA_real_; next }
  if (sum(valid_d) < length(err_day)) {
    knn_d <- spdep::knearneigh(as.matrix(coords[valid_d, ]),
                        k = min(8, sum(valid_d) - 1))
    lw_d  <- spdep::nb2listw(spdep::knn2nb(knn_d), style = "W")
    moran_per_day[d] <- spdep::moran.test(err_day[valid_d], lw_d)$estimate["Moran I statistic"]
  } else {
    moran_per_day[d] <- spdep::moran.test(err_day, lw)$estimate["Moran I statistic"]
  }
}

per_day_q <- quantile(moran_per_day, c(0.25, 0.5, 0.75), na.rm = TRUE)

cat("\n=== Moran's I on Forecast Errors ===\n")
cat(sprintf("  (a) Mean residual field (persistent bias):\n"))
cat(sprintf("      I = %.4f, p = %.6f\n",
            moran_mean$estimate["Moran I statistic"], moran_mean$p.value))
cat(sprintf("  (b) Per-day distribution (transient clustering), n = %d days:\n",
            n_test_days))
cat(sprintf("      median = %.4f, IQR = [%.4f, %.4f], range = [%.4f, %.4f]\n",
            per_day_q[2], per_day_q[1], per_day_q[3],
            min(moran_per_day, na.rm = TRUE),
            max(moran_per_day, na.rm = TRUE)))
if (moran_mean$p.value < 0.05) {
  cat("  -> Persistent spatial bias: same cells are systematically wrong.\n")
} else {
  cat("  -> No persistent bias detected on the mean field.\n")
}

moran_summary <- tibble::tibble(
  source    = c("mean_residual_field",
                "per_day_median", "per_day_iqr_low", "per_day_iqr_high",
                "per_day_min", "per_day_max"),
  statistic = c(as.numeric(moran_mean$estimate["Moran I statistic"]),
                per_day_q[2], per_day_q[1], per_day_q[3],
                min(moran_per_day, na.rm = TRUE),
                max(moran_per_day, na.rm = TRUE)),
  p_value   = c(moran_mean$p.value, rep(NA_real_, 5))
)
readr::write_csv(moran_summary, file.path(out_tab, "moran_i_residuals.csv"))

lisa <- spdep::localmoran(error_valid, lw_mean)

coords_valid$lisa_Ii    <- lisa[, "Ii"]
coords_valid$lisa_pval  <- lisa[, "Pr(z != E(Ii))"]
coords_valid$mean_error <- error_valid

lag_error <- spdep::lag.listw(lw_mean, error_valid)
coords_valid$quadrant <- dplyr::case_when(
  error_valid > 0 & lag_error > 0 ~ "High-High",
  error_valid < 0 & lag_error < 0 ~ "Low-Low",
  error_valid > 0 & lag_error < 0 ~ "High-Low",
  error_valid < 0 & lag_error > 0 ~ "Low-High"
)
coords_valid$quadrant[coords_valid$lisa_pval > 0.05] <- "Not significant"

p_lisa <- ggplot2::ggplot() +
  ggplot2::geom_raster(data = coords_valid, ggplot2::aes(lon, lat, fill = quadrant)) +
  ggplot2::geom_sf(data = borders_sf, fill = NA, colour = "grey25", linewidth = 0.3) +
  ggplot2::geom_sf(data = poland_sf, fill = NA, colour = "black", linewidth = 0.6) +
  ggplot2::geom_point(data = cities_df, ggplot2::aes(lon, lat), colour = "black", size = 1.3) +
  ggplot2::geom_text(data = cities_df, ggplot2::aes(lon, lat, label = name),
            colour = "black", size = 3, nudge_y = 0.15, fontface = "bold") +
  ggplot2::scale_fill_manual(values = c(
    "High-High"       = "#d73027",
    "Low-Low"         = "#4575b4",
    "High-Low"        = "#fdae61",
    "Low-High"        = "#abd9e9",
    "Not significant" = "grey90"
  )) +
  ggplot2::coord_sf(xlim = range(LON_CENTERS), ylim = range(LAT_CENTERS), expand = FALSE) +
  ggplot2::labs(title = "LISA cluster map of ConvLSTM (skip) forecast errors",
       subtitle = "Where does the headline model systematically over- or under-predict?",
       fill = "Cluster", x = NULL, y = NULL) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "lisa_error_clusters.png"), p_lisa,
       width = 8, height = 6, dpi = 150)

arch_models <- list()
arch_models[["CNN-LSTM (skip)"]] <- results$mae_grid
if (!is.null(noskip))       arch_models[["CNN-LSTM (no skip)"]]      <- noskip$mae_grid
if (!is.null(lstm_only))    arch_models[["Per-cell LSTM (skip)"]]    <- lstm_only$mae_grid
if (!is.null(lstm_only_ns)) arch_models[["Per-cell LSTM (no skip)"]] <- lstm_only_ns$mae_grid
if (!is.null(convlstm))     arch_models[["ConvLSTM (skip)"]]         <- convlstm$mae_grid
if (!is.null(convlstm_ns))  arch_models[["ConvLSTM (no skip)"]]      <- convlstm_ns$mae_grid

if (length(arch_models) >= 2) {
  arch_df <- purrr::imap_dfr(arch_models, ~ {
    df <- grid_to_df(.x, "mae"); df$Model <- .y; df
  })
  arch_df$Model <- factor(arch_df$Model, levels = names(arch_models))

  mae_cap <- quantile(arch_df$mae, 0.99, na.rm = TRUE)
  p_arch <- ggplot2::ggplot() +
    ggplot2::geom_raster(data = arch_df, ggplot2::aes(lon, lat, fill = pmin(mae, mae_cap))) +
    ggplot2::geom_sf(data = borders_sf, fill = NA, colour = "grey25", linewidth = 0.3) +
    ggplot2::geom_sf(data = poland_sf, fill = NA, colour = "black", linewidth = 0.6) +
    ggplot2::scale_fill_viridis_c(name = expression(MAE~"["*mu*g/m^3*"]"),
                         option = "inferno") +
    ggplot2::coord_sf(xlim = range(LON_CENTERS), ylim = range(LAT_CENTERS),
             expand = FALSE) +
    ggplot2::labs(title = "Spatial MAE by architecture",
         subtitle = "Where does spatial context help? Where does it fail?",
         x = NULL, y = NULL) +
    ggplot2::facet_wrap(~ Model, ncol = 2) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "bottom")
  ggplot2::ggsave(file.path(out_fig, "arch_comparison_mae_maps.png"), p_arch,
         width = 11, height = 8, dpi = 150)
}

if (!is.null(arch_models[["ConvLSTM (skip)"]])) {
  nearest_idx <- function(target, centers) which.min(abs(centers - target))
  ranking_cities <- tibble::tibble(
    name = c("Warszawa", "Kraków", "Łódź", "Wrocław", "Poznań", "Gdańsk",
             "Szczecin", "Bydgoszcz", "Lublin", "Katowice", "Białystok", "Rzeszów"),
    lon  = c(21.01, 19.94, 19.46, 17.04, 16.93, 18.65,
             14.55, 18.00, 22.57, 19.03, 23.17, 22.00),
    lat  = c(52.23, 50.06, 51.76, 51.11, 52.41, 54.35,
             53.43, 53.12, 51.25, 50.26, 53.13, 50.04)
  )
  headline_mae_grid <- arch_models[["ConvLSTM (skip)"]]
  city_mae <- ranking_cities %>%
    dplyr::mutate(MAE = mapply(function(lo, la)
                     headline_mae_grid[nearest_idx(lo, LON_CENTERS),
                                       nearest_idx(la, LAT_CENTERS)],
                   lon, lat)) %>%
    dplyr::arrange(MAE) %>%
    dplyr::mutate(name = factor(name, levels = name))
  p_city <- ggplot2::ggplot(city_mae, ggplot2::aes(MAE, name)) +
    ggplot2::geom_col(fill = "steelblue") +
    ggplot2::labs(title = "Per-city test MAE — ConvLSTM (skip)",
         x = expression(MAE~"["*mu*g/m^3*"]"), y = NULL) +
    ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(out_fig, "arch_comparison_mae_cities.png"), p_city,
         width = 8, height = 6, dpi = 150)
}

LR_STEP_SIZE <- 15
training_logs <- list(
  "CNN-LSTM (skip)"         = "training_log_cnnlstm.csv",
  "CNN-LSTM (no skip)"      = "training_log_cnnlstm_noskip.csv",
  "Per-cell LSTM (skip)"    = "training_log_lstm.csv",
  "Per-cell LSTM (no skip)" = "training_log_lstm_noskip.csv",
  "ConvLSTM (skip)"         = "training_log_convlstm.csv",
  "ConvLSTM (no skip)"      = "training_log_convlstm_noskip.csv"
)

log_rows <- list(); stop_pts <- list(); best_pts <- list(); decay_pts <- list()
for (m in names(training_logs)) {
  fp <- here::here("output", training_logs[[m]])
  if (!file.exists(fp)) next
  tl <- readr::read_csv(fp, show_col_types = FALSE)
  tl$model <- m
  log_rows[[m]] <- tl

  stop_pts[[m]] <- tibble::tibble(model = m, epoch = max(tl$epoch),
                          loss = tl$loss[tl$epoch == max(tl$epoch) &
                                          tl$set == "valid"][1])

  val_rows <- dplyr::filter(tl, set == "valid")
  best_row <- val_rows[which.min(val_rows$loss), ]
  best_pts[[m]] <- tibble::tibble(model = m, epoch = best_row$epoch, loss = best_row$loss)

  max_epoch <- max(tl$epoch)
  decays <- if (max_epoch >= LR_STEP_SIZE) {
    seq(LR_STEP_SIZE, max_epoch, by = LR_STEP_SIZE)
  } else integer(0)
  if (length(decays)) decay_pts[[m]] <- tibble::tibble(model = m, epoch = decays)
}

if (length(log_rows)) {
  tl_all    <- dplyr::bind_rows(log_rows)
  stop_all  <- dplyr::bind_rows(stop_pts)
  best_all  <- dplyr::bind_rows(best_pts)
  decay_all <- if (length(decay_pts)) dplyr::bind_rows(decay_pts)
               else tibble::tibble(model = character(), epoch = integer())

  p_curves <- ggplot2::ggplot(tl_all, ggplot2::aes(epoch, loss, color = set)) +
    ggplot2::geom_vline(data = decay_all, ggplot2::aes(xintercept = epoch),
               linetype = "dashed", colour = "grey60", linewidth = 0.3) +
    ggplot2::geom_line(linewidth = 0.6) +
    ggplot2::geom_point(data = stop_all, ggplot2::aes(epoch, loss),
               inherit.aes = FALSE, shape = 4, size = 2.5, stroke = 1) +
    ggplot2::geom_point(data = best_all, ggplot2::aes(epoch, loss),
               inherit.aes = FALSE, shape = 21, size = 2.5, stroke = 1,
               fill = "gold", colour = "black") +
    ggplot2::facet_wrap(~ model, scales = "free", ncol = 2) +
    ggplot2::scale_color_manual(values = c(train = "steelblue", valid = "tomato"),
                       labels = c(train = "Train", valid = "Validation")) +
    ggplot2::labs(title = "Training curves (MSE loss) with LR-decay, best-epoch, early-stop markers",
         x = "Epoch", y = "MSE loss", color = NULL,
         caption = paste(
           "Dashed lines: learning-rate decay steps.",
           "Gold circle: best validation epoch (weights restored via keep_best_model — test metrics reflect these).",
           "Cross: early-stop epoch.",
           sep = "  ")) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "top")

  ggplot2::ggsave(file.path(out_fig, "training_curves_all_models.png"), p_curves,
         width = 9, height = 7, dpi = 150)
}

cat("\n=== Figures saved ===\n")
cat("  output/figures/test_rmse_timeseries.png\n")
cat("  output/figures/test_example_prediction.png\n")
cat("  output/figures/test_mae_spatial.png\n")
cat("  output/figures/lisa_error_clusters.png\n")
cat("  output/figures/arch_comparison_mae_maps.png\n")
cat("  output/figures/arch_comparison_mae_cities.png\n")
cat("  output/figures/training_curves_all_models.png\n")
cat("\n=== Tables saved ===\n")
cat("  output/tables/accuracy_comparison.csv\n")
cat("  output/tables/moran_i_residuals.csv\n")
