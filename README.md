# Spatio-Temporal Modelling with Deep Learning — companion code & data

Reproducibility materials for the book chapter **"Spatio-Temporal Modelling with
Deep Learning: Forecasting tomorrow's PM2.5 field over Poland on a 110 × 60 grid"**
by Michał Woźniak (ORCID [0000-0001-7313-864X](https://orcid.org/0000-0001-7313-864X)).

The project forecasts the **daily PM2.5 field over Poland** one day ahead on a
110 × 60, 0.1° CAMS grid (2018–2022), in R using `torch`/`luz`. It compares
classical baselines against three deep-learning architecture families
— **CNN-LSTM**, **per-cell LSTM**, and **ConvLSTM** — each in a residual-**skip**
(predict the change over persistence) and a **no-skip** variant. Headline model:
ConvLSTM (skip).

## Repository layout

| Path | Contents |
|------|----------|
| `code/` | Numbered R pipeline `00`–`12`, shared `_common_training.R`, and `run_all_after_data_acquisition.R`. |
| `code_kaggle/` | `run_end_to_end_current.ipynb` — one-click end-to-end run on Kaggle's free P100 GPU. |
| `data/processed/` | Pre-computed tensors, spatial features, and baseline + per-model test results (**Git LFS**). |
| `data/raw/` | Empty — raw CAMS / WorldPop inputs (~2.9 GB) are downloaded by `code/01`. |
| `models/` | Six trained model checkpoints, `*.pt` (**Git LFS**). |
| `output/` | `figures/` (PNG), `tables/` (CSV), and per-model `training_log_*.csv`. |

## Prerequisites

- **R ≥ 4.1** (the code uses the native `|>` pipe).
- **Git LFS** — required to pull the data and model files:
  ```bash
  git lfs install
  git clone https://github.com/michaelwozniak/spatiotemporal-nn-chapter-code.git
  cd spatiotemporal-nn-chapter-code
  git lfs pull          # fetch the .rds (data) and .pt (models) artifacts
  ```
- **R packages + LibTorch** — install everything with:
  ```bash
  Rscript code/00_install.R
  ```
  (installs `torch`, `luz`, `tidyverse`, `sf`, `stars`, `terra`, `spdep`,
  `ecmwfr`, `plotly`, … and the LibTorch backend).
- **A free Copernicus CDS account** — only needed to **re-download raw data**
  (`code/01`). Copy `.env.example` to `.env` and fill in `CDS_USER` / `CDS_API_KEY`
  (register at <https://cds.climate.copernicus.eu>).

## Reproduce

### A. Regenerate tables & figures without retraining (fastest)

The trained models and processed data ship with the repo (via LFS), so the
chapter's evaluation outputs reproduce directly — no GPU or data download needed:

```bash
git lfs pull
Rscript code/10_evaluation.R       # accuracy tables, error maps, LISA  -> output/
Rscript code/11_city_forecasts.R   # interactive Plotly city dashboard
Rscript code/12_didactic_figures.R # conceptual figures (conv, LSTM cell, …)
```

### B. Full pipeline from scratch (CPU or local GPU)

```bash
Rscript code/00_install.R            # packages + LibTorch
Rscript code/01_data_acquisition.R   # download CAMS + WorldPop -> data/raw  (needs .env)
Rscript code/run_all_after_data_acquisition.R   # 02 prep -> 03 baselines -> 04-09 train -> 10-12
```

Set the training profile before steps `04`–`09` to select batch-size / learning-rate presets:

```bash
export TRAINING_PROFILE=cpu   # or: gpu
```

### C. Kaggle (free GPU)

Upload `code_kaggle/run_end_to_end_current.ipynb` and run it with **R**, the **GPU
accelerator**, and **Internet on**. It sets `TRAINING_PROFILE=gpu` and runs the
whole pipeline from data preparation to the final tables and figures.

## Model variants

Each family is trained twice:

- **skip** — the network predicts the *change* over a persistence forecast; the
  final field is `persistence + Δ̂`.
- **no-skip** — the network predicts the next field directly.

| Family | skip | no-skip |
|--------|------|---------|
| CNN-LSTM | `models/cnn_lstm_final.pt` | `models/cnn_lstm_noskip_final.pt` |
| per-cell LSTM | `models/lstm_only_final.pt` | `models/lstm_only_noskip_final.pt` |
| ConvLSTM | `models/convlstm_final.pt` | `models/convlstm_noskip_final.pt` |

## License

- **Code** (`code/`, `code_kaggle/`): MIT — see [`LICENSE`](LICENSE).
- **Figures and tables** (`output/`): CC BY 4.0.
- **Data** (`data/processed/`): derived from CAMS atmospheric reanalysis
  (© Copernicus Atmosphere Monitoring Service), redistributed for reproducibility
  under the Copernicus licence — please attribute CAMS.

## Citation

If you use this code or data, please cite the book chapter "Spatio-Temporal
Modelling with Deep Learning: Forecasting tomorrow's PM2.5 field over Poland on a
110 × 60 grid" by Michał Woźniak (Routledge).
