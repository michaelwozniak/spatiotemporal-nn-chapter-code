library(here)
source(here::here("code", "_common_training.R"))

tensors <- readRDS(here::here("data", "processed", "pm25_tensors.rds"))
meta    <- tensors$meta

cat("=== ConvLSTM (no skip, ablation) ===\n")
cat(sprintf("  Grid: %d x %d, Window: %d, Channels: %d\n",
            meta$grid_dim[[1]], meta$grid_dim[[2]],
            meta$window_size, meta$n_channels))

tp  <- arch_hparams("spatial")
dls <- make_dataloaders(tensors, meta, batch_size = tp$batch_size)

convlstm_cell <- torch::nn_module(
  "convlstm_cell",
  initialize = function(in_channels, hidden_channels, kernel_size = 3) {
    self$hidden_channels <- hidden_channels
    pad <- kernel_size %/% 2
    self$conv <- torch::nn_conv2d(
      in_channels  = in_channels + hidden_channels,
      out_channels = 4 * hidden_channels,
      kernel_size  = kernel_size,
      padding      = pad
    )
  },
  forward = function(x, h, c) {
    combined <- torch::torch_cat(list(x, h), dim = 2)
    gates    <- self$conv(combined)
    split    <- torch::torch_split(gates, self$hidden_channels, dim = 2)
    i_t <- torch::torch_sigmoid(split[[1]])
    f_t <- torch::torch_sigmoid(split[[2]])
    g_t <- torch::torch_tanh(split[[3]])
    o_t <- torch::torch_sigmoid(split[[4]])
    c_next <- f_t * c + i_t * g_t
    h_next <- o_t * torch::torch_tanh(c_next)
    list(h_next, c_next)
  }
)

convlstm_model_noskip <- torch::nn_module(
  "convlstm_model_noskip",
  initialize = function(in_channels, enc_channels, hidden_channels,
                        grid_h, grid_w) {
    self$encoder <- torch::nn_sequential(
      torch::nn_conv2d(in_channels, enc_channels, kernel_size = 3,
                stride = 2, padding = 1),
      torch::nn_batch_norm2d(enc_channels),
      torch::nn_relu(),
      torch::nn_conv2d(enc_channels, enc_channels, kernel_size = 3,
                stride = 2, padding = 1),
      torch::nn_batch_norm2d(enc_channels),
      torch::nn_relu()
    )
    self$cell <- convlstm_cell(enc_channels, hidden_channels, kernel_size = 3)

    self$decoder <- torch::nn_sequential(
      torch::nn_conv2d(hidden_channels, enc_channels, kernel_size = 3, padding = 1),
      torch::nn_relu(),
      torch::nn_conv2d(enc_channels, 1, kernel_size = 3, padding = 1)
    )
    self$upsample <- torch::nn_upsample(size = c(grid_h, grid_w), mode = "bilinear",
                                 align_corners = FALSE)
    self$hidden_channels <- hidden_channels
  },
  forward = function(x) {
    b <- x$shape[1]; tt <- x$shape[2]
    cc <- x$shape[3]; h <- x$shape[4]; w <- x$shape[5]

    x_flat <- x$reshape(c(b * tt, cc, h, w))
    feat   <- self$encoder(x_flat)
    feat   <- feat$view(c(b, tt, feat$shape[2],
                          feat$shape[3], feat$shape[4]))

    h_state <- torch::torch_zeros(b, self$hidden_channels, feat$shape[4], feat$shape[5],
                           device = feat$device, dtype = feat$dtype)
    c_state <- torch::torch_zeros_like(h_state)
    for (t in seq_len(tt)) {
      out <- self$cell(feat[, t, , , ], h_state, c_state)
      h_state <- out[[1]]; c_state <- out[[2]]
    }

    y_hat <- self$decoder(h_state)
    self$upsample(y_hat)
  }
)

ENC_CHANNELS    <- 16L
HIDDEN_CHANNELS <- 32L
grid_h          <- as.integer(dim(tensors$source_array)[1])
grid_w          <- as.integer(dim(tensors$source_array)[2])

cat(sprintf("  enc_c: %d, hidden_c: %d, epochs: %d, batch: %d\n",
            ENC_CHANNELS, HIDDEN_CHANNELS, EPOCHS, tp$batch_size))

log_path <- here::here("output", "training_log_convlstm_noskip.csv")
fitted_cl_ns <- load_or_train(
  convlstm_model_noskip,
  hparams  = list(in_channels     = meta$n_channels,
                  enc_channels    = ENC_CHANNELS,
                  hidden_channels = HIDDEN_CHANNELS,
                  grid_h          = grid_h,
                  grid_w          = grid_w),
  dls            = dls,
  log_path       = log_path,
  model_filename = "convlstm_noskip_final.pt",
  lr             = tp$lr,
  accelerator    = tp$accelerator,
  fig_path       = here::here("output", "figures", "training_loss_convlstm_noskip.png"),
  fig_title      = "ConvLSTM (no skip) — training loss"
)

evaluate_and_save(
  fitted_cl_ns, dls$test, tensors, meta,
  results_path = here::here("data", "processed", "convlstm_noskip_test_results.rds"),
  label        = "ConvLSTM (no skip)"
)
