library(here)
source(here::here("code", "_common_training.R"))

tensors <- readRDS(here::here("data", "processed", "pm25_tensors.rds"))
meta    <- tensors$meta

cat("=== Per-cell LSTM (no skip, ablation) ===\n")
cat(sprintf("  Grid: %d x %d, Window: %d, Channels: %d\n",
            meta$grid_dim[[1]], meta$grid_dim[[2]],
            meta$window_size, meta$n_channels))

tp  <- arch_hparams("percell")
dls <- make_dataloaders(tensors, meta, batch_size = tp$batch_size)

lstm_only_noskip <- torch::nn_module(
  "lstm_only_noskip",
  initialize = function(in_channels, hidden_dim, lstm_layers) {
    self$lstm <- torch::nn_lstm(
      input_size  = in_channels,
      hidden_size = hidden_dim,
      num_layers  = lstm_layers,
      batch_first = TRUE,
      dropout     = if (lstm_layers > 1) 0.2 else 0
    )
    self$head <- torch::nn_linear(hidden_dim, 1)
  },
  forward = function(x) {
    b <- x$shape[1]; tt <- x$shape[2]; cc <- x$shape[3]
    h <- x$shape[4]; w  <- x$shape[5]

    seq <- x$permute(c(1, 4, 5, 2, 3))$contiguous()$view(c(b * h * w, tt, cc))

    out <- self$lstm(seq)[[1]][, tt, ]
    self$head(out)$view(c(b, 1, h, w))
  }
)

HIDDEN_DIM  <- 32L
LSTM_LAYERS <- 1L

cat(sprintf("  hidden_dim: %d, lstm_layers: %d, epochs: %d, batch: %d\n",
            HIDDEN_DIM, LSTM_LAYERS, EPOCHS, tp$batch_size))

log_path <- here::here("output", "training_log_lstm_noskip.csv")
fitted_lstm_ns <- load_or_train(
  lstm_only_noskip,
  hparams  = list(in_channels = meta$n_channels,
                  hidden_dim  = HIDDEN_DIM,
                  lstm_layers = LSTM_LAYERS),
  dls            = dls,
  log_path       = log_path,
  model_filename = "lstm_only_noskip_final.pt",
  lr             = tp$lr,
  accelerator    = tp$accelerator,
  fig_path       = here::here("output", "figures", "training_loss_lstm_noskip.png"),
  fig_title      = "Per-cell LSTM (no skip) — training loss"
)

evaluate_and_save(
  fitted_lstm_ns, dls$test, tensors, meta,
  results_path = here::here("data", "processed", "lstm_only_noskip_test_results.rds"),
  label        = "Per-cell LSTM (no skip)"
)
