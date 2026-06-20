suppressPackageStartupMessages({
  library(torch)
  library(luz)
  library(tidyverse)
  library(here)
})

set.seed(42)
torch::torch_manual_seed(42)

# Training profile, selectable at the shell with e.g.
#   TRAINING_PROFILE=gpu Rscript code/run_all_after_data_acquisition.R
#   "cpu" (default): every architecture trains at batch 4, lr 1e-3, on CPU.
#   "gpu": per-cell LSTM stays at batch 4 / lr 1e-3; CNN-LSTM and ConvLSTM use
#          batch 64 with lr scaled by the square-root rule (lr = 1e-3 * sqrt(batch/4)),
#          on CUDA when available (falls back to CPU otherwise; Apple MPS is
#          intentionally avoided — bilinear upsample issues on the 110x60 grid).
# Only matters on a fresh retrain; inference via load_or_train() ignores it.
TRAINING_PROFILE <- Sys.getenv("TRAINING_PROFILE", "cpu") # "cpu" | "gpu"

LR_BASE_BATCH <- 4L
LR_BASE <- 1e-3
lr_for_batch <- function(batch) LR_BASE * sqrt(batch / LR_BASE_BATCH)

EPOCHS <- 100L
PATIENCE <- 20L
STEP_SIZE <- 15L
GAMMA <- 0.5

.PROFILE_BATCH <- list(
  cpu = list(percell = 4L, spatial = 4L),
  gpu = list(percell = 4L, spatial = 64L)
)

# Resolve (batch_size, lr, accelerator) for an architecture family
# ("spatial" = CNN-LSTM & ConvLSTM, "percell" = per-cell LSTM) under the
# active profile. Each model script calls this once.
arch_hparams <- function(arch = c("percell", "spatial")) {
  arch <- match.arg(arch)
  bs <- .PROFILE_BATCH[[TRAINING_PROFILE]][[arch]]
  list(
    batch_size = bs,
    lr = lr_for_batch(bs),
    accelerator = if (identical(TRAINING_PROFILE, "gpu") && torch::cuda_is_available()) {
      luz::accelerator()
    } else {
      luz::accelerator(cpu = TRUE)
    }
  )
}

BATCH_SIZE <- .PROFILE_BATCH[[TRAINING_PROFILE]]$percell # 4 under cpu
LR <- lr_for_batch(BATCH_SIZE) # 1e-3 under cpu

cpu_acc <- luz::accelerator(cpu = TRUE)

pm25_dataset <- torch::dataset(
  name = "pm25_dataset",
  initialize = function(source_array, Y, sample_indices, window_size) {
    self$src <- source_array
    self$Y <- Y
    self$idx <- sample_indices
    self$win <- window_size
  },
  .getitem = function(i) {
    t_start <- self$idx[i]
    t_end <- t_start + self$win - 1
    x <- torch::torch_tensor(self$src[, , t_start:t_end, ])$permute(c(3, 4, 1, 2))
    y <- torch::torch_tensor(self$Y[i, , ])$unsqueeze(1)
    list(x = x, y = y)
  },
  .length = function() nrow(self$Y)
)

make_dataloaders <- function(tensors, meta, batch_size = BATCH_SIZE) {
  src <- tensors$source_array
  list(
    train = torch::dataloader(
      pm25_dataset(src, tensors$Y_train, meta$split_idx$train, meta$window_size),
      batch_size = batch_size, shuffle = TRUE
    ),
    val = torch::dataloader(
      pm25_dataset(src, tensors$Y_val, meta$split_idx$val, meta$window_size),
      batch_size = batch_size, shuffle = FALSE
    ),
    test = torch::dataloader(
      pm25_dataset(src, tensors$Y_test, meta$split_idx$test, meta$window_size),
      batch_size = batch_size, shuffle = FALSE
    )
  )
}

make_callbacks <- function(log_path) {
  list(
    luz::luz_callback_early_stopping(
      monitor = "valid_loss",
      patience = PATIENCE,
      mode = "min"
    ),
    luz::luz_callback_keep_best_model(
      monitor = "valid_loss",
      mode = "min"
    ),
    luz::luz_callback_lr_scheduler(
      lr_scheduler = torch::lr_step,
      step_size = STEP_SIZE,
      gamma = GAMMA
    ),
    luz::luz_callback_gradient_clip(max_norm = 1.0),
    luz::luz_callback_csv_logger(path = log_path)
  )
}

make_inv_scale <- function(meta) {
  function(x) {
    unscaled <- x * (meta$pm25_max - meta$pm25_min) + meta$pm25_min
    if (isTRUE(meta$log_transform)) expm1(unscaled) else unscaled
  }
}

persistence_pm25 <- function(tensors, meta, inv_scale = make_inv_scale(meta)) {
  src <- tensors$source_array
  n_test <- length(meta$split_idx$test)
  win <- meta$window_size
  out <- array(dim = c(n_test, dim(src)[1], dim(src)[2]))
  for (i in seq_len(n_test)) {
    t_last <- meta$split_idx$test[i] + win - 1
    out[i, , ] <- src[, , t_last, 1]
  }
  inv_scale(out)
}

fit_shared <- function(module, hparams, dls, log_path,
                       lr = LR, accelerator = cpu_acc) {
  stage1 <- luz::setup(module,
    loss      = torch::nn_mse_loss(),
    optimizer = torch::optim_adam,
    metrics   = list(luz::luz_metric_mae())
  )
  stage2 <- do.call(luz::set_hparams, c(list(stage1), hparams))
  stage3 <- luz::set_opt_hparams(stage2, lr = lr)
  luz::fit(stage3,
    data        = dls$train,
    valid_data  = dls$val,
    epochs      = EPOCHS,
    accelerator = accelerator,
    callbacks   = make_callbacks(log_path),
    verbose     = TRUE
  )
}

evaluate_and_save <- function(fitted, test_dl, tensors, meta,
                              results_path, label,
                              accelerator = cpu_acc) {
  inv_scale <- make_inv_scale(meta)
  test_actual_pm25 <- inv_scale(tensors$Y_test)

  preds <- predict(fitted, test_dl, accelerator = accelerator)
  preds_pm <- pmax(inv_scale(as.array(preds$cpu())[, 1, , ]), 0)

  rmse <- sqrt(mean((preds_pm - test_actual_pm25)^2, na.rm = TRUE))
  mae <- mean(abs(preds_pm - test_actual_pm25), na.rm = TRUE)
  mae_grid <- apply(abs(preds_pm - test_actual_pm25), c(2, 3), mean, na.rm = TRUE)

  cat(sprintf("\n%s: RMSE = %.2f µg/m³, MAE = %.2f\n", label, rmse, mae))

  result <- list(
    predictions = preds_pm, actuals = test_actual_pm25,
    rmse = rmse, mae = mae, mae_grid = mae_grid
  )
  saveRDS(result, results_path)
  cat(sprintf("Saved: %s\n", results_path))

  invisible(result)
}

save_training_curve <- function(log_path, fig_path, title,
                                loss_label = "MSE") {
  tl <- readr::read_csv(log_path, show_col_types = FALSE)
  p <- ggplot2::ggplot(tl, ggplot2::aes(epoch, loss, color = set)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::geom_point(size = 1.2) +
    ggplot2::labs(title = title, y = loss_label, x = "Epoch", color = NULL) +
    ggplot2::scale_color_manual(
      values = c(train = "steelblue", valid = "tomato"),
      labels = c(train = "Train", valid = "Validation")
    ) +
    ggplot2::theme_minimal()
  out_dir <- dirname(fig_path)
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  ggplot2::ggsave(fig_path, p, width = 8, height = 5)
}

MODEL_DIR <- here::here("models")

load_or_train <- function(module, hparams, dls, log_path, model_filename,
                          lr = LR, accelerator = cpu_acc,
                          fig_path = NULL, fig_title = NULL) {
  path <- file.path(MODEL_DIR, model_filename)
  if (file.exists(path)) {
    cat(sprintf("[load_or_train] loading pre-trained checkpoint: %s\n", path))
    cat("[load_or_train] skipping training (delete the .pt to force a retrain)\n")
    return(luz::luz_load(path))
  }
  cat(sprintf("[load_or_train] no checkpoint at %s -- training fresh\n", path))
  fitted <- fit_shared(module, hparams, dls, log_path, lr = lr, accelerator = accelerator)
  if (!dir.exists(MODEL_DIR)) dir.create(MODEL_DIR, recursive = TRUE)
  luz::luz_save(fitted, path)
  cat(sprintf("[load_or_train] saved freshly trained model: %s\n", path))
  if (!is.null(fig_path) && !is.null(fig_title)) {
    save_training_curve(log_path, fig_path, fig_title)
  }
  fitted
}
