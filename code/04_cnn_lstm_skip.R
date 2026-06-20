library(here)
source(here::here("code", "_common_training.R"))

tensors <- readRDS(here::here("data", "processed", "pm25_tensors.rds"))
meta    <- tensors$meta

cat("=== Loaded data ===\n")
cat(sprintf("  Source array: %s\n", paste(dim(tensors$source_array), collapse = " x ")))
cat(sprintf("  Y_train: %s\n", paste(dim(tensors$Y_train), collapse = " x ")))
cat(sprintf("  Y_val:   %s\n", paste(dim(tensors$Y_val), collapse = " x ")))
cat(sprintf("  Y_test:  %s\n", paste(dim(tensors$Y_test), collapse = " x ")))
cat(sprintf("  Grid: %d x %d, Window: %d, Channels: %d\n",
            meta$grid_dim[[1]], meta$grid_dim[[2]],
            meta$window_size, meta$n_channels))

tp  <- arch_hparams("spatial")
dls <- make_dataloaders(tensors, meta, batch_size = tp$batch_size)

batch <- dls$train$.iter()$.next()
cat(sprintf("\nBatch X shape: %s  (batch, T, C, H, W)\n",
            paste(batch$x$shape, collapse = " x ")))
cat(sprintf("Batch Y shape: %s  (batch, 1, H, W)\n",
            paste(batch$y$shape, collapse = " x ")))

cnn_lstm <- torch::nn_module(
  "cnn_lstm",
  initialize = function(in_channels, hidden_dim, lstm_layers, grid_h, grid_w) {
    self$encoder <- torch::nn_sequential(
      torch::nn_conv2d(in_channels, 32, kernel_size = 3, stride = 2, padding = 1),
      torch::nn_batch_norm2d(32),
      torch::nn_relu(),
      torch::nn_conv2d(32, 64, kernel_size = 3, stride = 2, padding = 1),
      torch::nn_batch_norm2d(64),
      torch::nn_relu(),
      torch::nn_conv2d(64, 64, kernel_size = 3, stride = 2, padding = 1),
      torch::nn_batch_norm2d(64),
      torch::nn_relu()
    )
    enc_h <- ceiling(grid_h / 8)
    enc_w <- ceiling(grid_w / 8)
    self$enc_fc <- torch::nn_linear(64 * enc_h * enc_w, hidden_dim)

    self$lstm <- torch::nn_lstm(
      input_size  = hidden_dim,
      hidden_size = hidden_dim,
      num_layers  = lstm_layers,
      batch_first = TRUE,
      dropout     = if (lstm_layers > 1) 0.2 else 0
    )

    self$dec_fc    <- torch::nn_linear(hidden_dim, 64 * enc_h * enc_w)
    self$dec_enc_h <- enc_h
    self$dec_enc_w <- enc_w
    self$decoder <- torch::nn_sequential(
      torch::nn_conv2d(64, 32, kernel_size = 3, padding = 1),
      torch::nn_relu(),
      torch::nn_conv2d(32, 1,  kernel_size = 3, padding = 1)
    )
    self$upsample <- torch::nn_upsample(size = c(grid_h, grid_w), mode = "bilinear",
                                 align_corners = FALSE)
  },
  forward = function(x) {
    b  <- x$shape[1]
    tt <- x$shape[2]

    skip <- x[, tt, 1, , ]$unsqueeze(2)

    x_flat <- x$reshape(c(b * tt, x$shape[3], x$shape[4], x$shape[5]))
    feat   <- self$encoder(x_flat)
    feat   <- feat$view(c(b * tt, -1))
    feat   <- self$enc_fc(feat)
    feat   <- feat$view(c(b, tt, -1))

    lstm_out <- self$lstm(feat)
    last_h   <- lstm_out[[1]][, tt, ]

    out   <- self$dec_fc(last_h)
    out   <- out$view(c(b, 64, self$dec_enc_h, self$dec_enc_w))
    delta <- self$decoder(out)
    delta <- self$upsample(delta)

    skip + delta
  }
)

HIDDEN_DIM  <- 128L
LSTM_LAYERS <- 1L
grid_h      <- as.integer(dim(tensors$source_array)[1])
grid_w      <- as.integer(dim(tensors$source_array)[2])

cat(sprintf("\n=== Model config ===\n"))
cat(sprintf("  hidden_dim:  %d\n", HIDDEN_DIM))
cat(sprintf("  lstm_layers: %d\n", LSTM_LAYERS))
cat(sprintf("  lr:          %.4f\n", tp$lr))
cat(sprintf("  epochs:      %d\n", EPOCHS))
cat(sprintf("  batch_size:  %d\n", tp$batch_size))
cat(sprintf("  grid:        %d x %d\n", grid_h, grid_w))

log_path <- here::here("output", "training_log_cnnlstm.csv")
fitted_model <- load_or_train(
  cnn_lstm,
  hparams  = list(in_channels = meta$n_channels,
                  hidden_dim  = HIDDEN_DIM,
                  lstm_layers = LSTM_LAYERS,
                  grid_h      = grid_h,
                  grid_w      = grid_w),
  dls            = dls,
  log_path       = log_path,
  model_filename = "cnn_lstm_final.pt",
  lr             = tp$lr,
  accelerator    = tp$accelerator,
  fig_path       = here::here("output", "figures", "training_loss_cnnlstm.png"),
  fig_title      = "CNN-LSTM Training Loss (MSE on log1p-scaled targets)"
)

inv_scale    <- make_inv_scale(meta)
rmse_persist <- sqrt(mean((persistence_pm25(tensors, meta, inv_scale)
                           - inv_scale(tensors$Y_test))^2, na.rm = TRUE))

result <- evaluate_and_save(
  fitted_model, dls$test, tensors, meta,
  results_path = here::here("data", "processed", "cnn_lstm_skip_test_results.rds"),
  label        = "CNN-LSTM (skip)"
)

cat(sprintf("\n=== Model vs persistence ===\n"))
cat(sprintf("  RMSE: %.2f µg/m³  (persistence: %.2f)\n",
            result$rmse, rmse_persist))

if (result$rmse >= rmse_persist) {
  warning(sprintf(
    "Model does not beat persistence (RMSE %.2f >= %.2f).",
    result$rmse, rmse_persist))
}
