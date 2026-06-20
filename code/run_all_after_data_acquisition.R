# run_all_after_data_acquisition.R
# Runs the full pipeline in order, starting AFTER data acquisition — i.e.
# everything from 02_data_prep.R through 12_didactic_figures.R. Assumes
# 00_install.R (packages) and 01_data_acquisition.R (raw download) have
# already been run; this script does NOT re-fetch the raw data.
#
# The six model scripts (04-09) each call load_or_train(), so they luz_load
# their checkpoint from models/ when present and only train fresh when the
# corresponding .pt is absent (see code/_common_training.R). To force a
# retrain, delete the relevant models/<name>_final.pt before running.
#
# Usage:
#   Rscript code/run_all_after_data_acquisition.R
#
# Retraining honours the TRAINING_PROFILE env var (see _common_training.R):
#   TRAINING_PROFILE=gpu Rscript code/run_all_after_data_acquisition.R
# trains the CNN-LSTM/ConvLSTM at batch 64 / lr 4e-3 on CUDA when available;
# the default "cpu" profile uses batch 4 / lr 1e-3 everywhere.
#
# Wall-clock: ~10-15 min on a laptop CPU for scripts 03-12 with all 6
# checkpoints present (CPU predict only); +20-30 min for each model whose
# checkpoint is absent and so must train fresh. 02_data_prep.R adds a few
# minutes on top of that.
#
# Each script runs in the GLOBAL environment and the objects it created are
# removed afterwards (followed by gc), so peak RAM still tracks the single
# heaviest script instead of accumulating the ~1.35 GB source_array across all
# eleven.
#
# Why globalenv() and not a throwaway new.env(): the model scripts (04-09)
# checkpoint their trained network with luz_save(), which serialises the fitted
# torch module. R's serialize() copies any *non-blessed* environment BY VALUE.
# If a script runs inside a new.env(), the module's closure chain reaches that
# env -- which holds the source_array, referenced again by every dataloader
# dataset -- so luz_save() writes the array several times and overflows R's
# 2 GB long-vector limit ("long vectors not supported yet"). globalenv() is a
# blessed environment that serialize() stores by reference, so the array is
# never copied into the checkpoint. (This is why the legacy Jupyter notebook,
# whose cells each run in globalenv, never hit this.)

library(here)

scripts <- c(
  "02_data_prep.R",
  "03_baselines.R",
  "04_cnn_lstm_skip.R",
  "05_cnn_lstm_noskip.R",
  "06_lstm_only_skip.R",
  "07_lstm_only_noskip.R",
  "08_convlstm_skip.R",
  "09_convlstm_noskip.R",
  "10_evaluation.R",
  "11_city_forecasts.R",
  "12_didactic_figures.R"
)

# Names that must survive the per-script cleanup: everything already present in
# globalenv (incl. any caller/notebook state) plus the loop's own machinery.
.runner_keep <- c(ls(globalenv(), all.names = TRUE), ".runner_keep", "s", "t0")

for (s in scripts) {
  cat(sprintf("\n\n========== %s ==========\n\n", s))
  t0 <- Sys.time()
  source(here::here("code", s), local = globalenv(), echo = FALSE,
         max.deparse.length = 500)
  rm(list = setdiff(ls(globalenv(), all.names = TRUE), .runner_keep),
     envir = globalenv())
  invisible(gc())
  cat(sprintf("\n[%s finished in %.1f min]\n",
              s, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

cat("\n\nDone. Pipeline (02-12) finished.\n")
