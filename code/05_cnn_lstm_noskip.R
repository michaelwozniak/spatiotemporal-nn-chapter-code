library(here)
source(here::here("code", "_common_training.R"))

tensors <- readRDS(here::here("data", "processed", "pm25_tensors.rds"))
meta    <- tensors$meta

cat("=== CNN-LSTM (no skip) ===\n")
cat(sprintf("  Grid: %d x %d, Window: %d, Channels: %d\n",
            meta$grid_dim[[1]], meta$grid_dim[[2]],
            meta$window_size, meta$n_channels))

tp  <- arch_hparams("spatial")
dls <- make_dataloaders(tensors, meta, batch_size = tp$batch_size)

cnn_lstm_noskip <- torch::nn_module(
  "cnn_lstm_noskip",
  initialize = function(in_channels, hidden_dim, lstm_layers, grid_h, grid_w) {
    self$encoder <- torch::nn_sequential(
      torch::nn_conv2d(in_channels, 32, 3, stride = 2, padding = 1), torch::nn_batch_norm2d(32), torch::nn_relu(),
      torch::nn_conv2d(32, 64, 3, stride = 2, padding = 1), torch::nn_batch_norm2d(64), torch::nn_relu(),
      torch::nn_conv2d(64, 64, 3, stride = 2, padding = 1), torch::nn_batch_norm2d(64), torch::nn_relu()
    )
    enc_h <- ceiling(grid_h / 8); enc_w <- ceiling(grid_w / 8)
    self$enc_fc <- torch::nn_linear(64 * enc_h * enc_w, hidden_dim)
    self$lstm <- torch::nn_lstm(hidden_dim, hidden_dim, num_layers = lstm_layers,
                         batch_first = TRUE,
                         dropout = if (lstm_layers > 1) 0.2 else 0)
    self$dec_fc <- torch::nn_linear(hidden_dim, 64 * enc_h * enc_w)
    self$dec_enc_h <- enc_h; self$dec_enc_w <- enc_w
    self$decoder <- torch::nn_sequential(
      torch::nn_conv2d(64, 32, 3, padding = 1), torch::nn_relu(),
      torch::nn_conv2d(32, 1, 3, padding = 1)
    )
    self$upsample <- torch::nn_upsample(size = c(grid_h, grid_w), mode = "bilinear",
                                 align_corners = FALSE)
  },
  forward = function(x) {
    b <- x$shape[1]; tt <- x$shape[2]
    x_flat <- x$reshape(c(b * tt, x$shape[3], x$shape[4], x$shape[5]))
    feat <- self$encoder(x_flat)
    feat <- feat$view(c(b * tt, -1))
    feat <- self$enc_fc(feat)
    feat <- feat$view(c(b, tt, -1))
    lstm_out <- self$lstm(feat)
    last_h <- lstm_out[[1]][, tt, ]
    out <- self$dec_fc(last_h)
    out <- out$view(c(b, 64, self$dec_enc_h, self$dec_enc_w))
    out <- self$decoder(out)
    self$upsample(out)
  }
)

HIDDEN_DIM  <- 128L
LSTM_LAYERS <- 1L
grid_h      <- as.integer(dim(tensors$source_array)[1])
grid_w      <- as.integer(dim(tensors$source_array)[2])

cat(sprintf("  hidden_dim: %d, lstm_layers: %d, epochs: %d, batch: %d\n",
            HIDDEN_DIM, LSTM_LAYERS, EPOCHS, tp$batch_size))

log_path <- here::here("output", "training_log_cnnlstm_noskip.csv")
fitted_noskip <- load_or_train(
  cnn_lstm_noskip,
  hparams  = list(in_channels = meta$n_channels,
                  hidden_dim  = HIDDEN_DIM,
                  lstm_layers = LSTM_LAYERS,
                  grid_h      = grid_h,
                  grid_w      = grid_w),
  dls            = dls,
  log_path       = log_path,
  model_filename = "cnn_lstm_noskip_final.pt",
  lr             = tp$lr,
  accelerator    = tp$accelerator,
  fig_path       = here::here("output", "figures", "training_loss_cnnlstm_noskip.png"),
  fig_title      = "CNN-LSTM (no skip) — training loss"
)

evaluate_and_save(
  fitted_noskip, dls$test, tensors, meta,
  results_path = here::here("data", "processed", "noskip_test_results.rds"),
  label        = "CNN-LSTM (no skip)"
)
