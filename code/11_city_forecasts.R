library(tidyverse)
library(here)
library(plotly)
library(htmltools)
library(htmlwidgets)

nearest_idx <- function(target, centers) which.min(abs(centers - target))

cities_df <- tibble::tibble(
  name = c(
    "Warszawa", "Kraków", "Łódź", "Wrocław", "Poznań", "Gdańsk",
    "Szczecin", "Bydgoszcz", "Lublin", "Katowice", "Białystok", "Rzeszów"
  ),
  lon = c(
    21.01, 19.94, 19.46, 17.04, 16.93, 18.65,
    14.55, 18.00, 22.57, 19.03, 23.17, 22.00
  ),
  lat = c(
    52.23, 50.06, 51.76, 51.11, 52.41, 54.35,
    53.43, 53.12, 51.25, 50.26, 53.13, 50.04
  )
)

tensors <- readRDS(here::here("data", "processed", "pm25_tensors.rds"))
meta <- tensors$meta
test_dates <- meta$times[meta$split_idx$test + meta$window_size]

GRID_NX     <- as.integer(dim(tensors$source_array)[1])
GRID_NY     <- as.integer(dim(tensors$source_array)[2])
LON_CENTERS <- 13.95 + seq_len(GRID_NX) * 0.1 - 0.05
LAT_CENTERS <- 55.05 + seq_len(GRID_NY) * (-0.1) + 0.05

inv_scale <- function(x) {
  unscaled <- x * (meta$pm25_max - meta$pm25_min) + meta$pm25_min
  if (isTRUE(meta$log_transform)) expm1(unscaled) else unscaled
}

read_if <- function(f) if (file.exists(f)) readRDS(f) else NULL
cnn           <- read_if(here::here("data", "processed", "cnn_lstm_skip_test_results.rds"))
noskip        <- read_if(here::here("data", "processed", "noskip_test_results.rds"))
lstm_one      <- read_if(here::here("data", "processed", "lstm_only_test_results.rds"))
lstm_one_ns   <- read_if(here::here("data", "processed", "lstm_only_noskip_test_results.rds"))
convlstm      <- read_if(here::here("data", "processed", "convlstm_test_results.rds"))
convlstm_ns   <- read_if(here::here("data", "processed", "convlstm_noskip_test_results.rds"))
basel         <- read_if(here::here("data", "processed", "baselines_results.rds"))

actuals <- if (!is.null(cnn)) cnn$actuals else inv_scale(tensors$Y_test)

src <- tensors$source_array
persistence <- array(dim = c(length(meta$split_idx$test), GRID_NX, GRID_NY))
for (i in seq_along(meta$split_idx$test)) {
  t_last <- meta$split_idx$test[i] + meta$window_size - 1
  persistence[i, , ] <- src[, , t_last, 1]
}
persistence <- inv_scale(persistence)

model_preds <- list()
if (!is.null(cnn))         model_preds[["CNN-LSTM (skip)"]]         <- cnn$predictions
if (!is.null(noskip))      model_preds[["CNN-LSTM (no skip)"]]      <- noskip$predictions
if (!is.null(lstm_one))    model_preds[["Per-cell LSTM (skip)"]]    <- lstm_one$predictions
if (!is.null(lstm_one_ns)) model_preds[["Per-cell LSTM (no skip)"]] <- lstm_one_ns$predictions
if (!is.null(convlstm))    model_preds[["ConvLSTM (skip)"]]         <- convlstm$predictions
if (!is.null(convlstm_ns)) model_preds[["ConvLSTM (no skip)"]]      <- convlstm_ns$predictions
if (!is.null(basel))       model_preds[["AR(1) per cell"]]          <- basel$ar1$predictions
if (!is.null(basel))       model_preds[["STAR"]]                    <- basel$star$predictions
model_preds[["Persistence"]] <- persistence

model_colors <- c(
  "Actual"                  = "#000000",
  "CNN-LSTM (skip)"         = "#1f77b4",
  "CNN-LSTM (no skip)"      = "#9467bd",
  "Per-cell LSTM (skip)"    = "#ff7f0e",
  "Per-cell LSTM (no skip)" = "#d62728",
  "ConvLSTM (skip)"         = "#2ca02c",
  "ConvLSTM (no skip)"      = "#17becf",
  "AR(1) per cell"          = "#8c564b",
  "STAR"                    = "#e377c2",
  "Persistence"             = "#7f7f7f"
)

city_series <- purrr::pmap(cities_df, function(name, lon, lat) {
  i <- nearest_idx(lon, LON_CENTERS)
  j <- nearest_idx(lat, LAT_CENTERS)
  rows <- list(tibble::tibble(
    date = test_dates, model = "Actual",
    pm25 = actuals[, i, j]
  ))
  for (mn in names(model_preds)) {
    rows <- c(rows, list(tibble::tibble(
      date = test_dates, model = mn,
      pm25 = model_preds[[mn]][, i, j]
    )))
  }
  dplyr::bind_rows(rows) %>% dplyr::mutate(
    grid_lon = LON_CENTERS[i],
    grid_lat = LAT_CENTERS[j]
  )
})
names(city_series) <- cities_df$name

city_metrics <- dplyr::bind_rows(lapply(cities_df$name, function(cn) {
  df <- city_series[[cn]]
  act <- df %>%
    dplyr::filter(model == "Actual") %>%
    dplyr::pull(pm25)
  purrr::map_dfr(setdiff(unique(df$model), "Actual"), function(mn) {
    ph <- df %>%
      dplyr::filter(model == mn) %>%
      dplyr::pull(pm25)
    tibble::tibble(
      City = cn, Model = mn,
      RMSE = sqrt(mean((ph - act)^2, na.rm = TRUE)),
      MAE = mean(abs(ph - act), na.rm = TRUE)
    )
  })
}))
out_tab <- here::here("output", "tables")
if (!dir.exists(out_tab)) dir.create(out_tab, recursive = TRUE)
readr::write_csv(city_metrics, file.path(out_tab, "city_metrics.csv"))

build_city_plot <- function(city_df, cn) {
  levs <- names(model_colors)
  levs <- c("Actual", setdiff(levs[levs %in% unique(city_df$model)], "Actual"))

  p <- plotly::plot_ly(height = 560)
  for (lv in levs) {
    d <- city_df %>%
      dplyr::filter(model == lv) %>%
      dplyr::arrange(date)
    p <- p %>% plotly::add_trace(
      x = d$date, y = d$pm25,
      name = lv,
      type = "scatter", mode = "lines",
      line = list(
        color = unname(model_colors[lv]),
        width = if (lv == "Actual") 2.2 else 1.4,
        dash  = if (lv == "Actual") "solid" else "dot"
      ),
      hovertemplate = paste0(
        "<b>", lv,
        "</b><br>%{x|%Y-%m-%d}: %{y:.1f} µg/m³<extra></extra>"
      )
    )
  }
  p %>%
    plotly::layout(
      autosize = TRUE,
      title = list(text = paste0("<b>", cn, "</b>"), x = 0.02, y = 0.97),
      xaxis = list(title = NULL, rangeslider = list(visible = TRUE, thickness = 0.08)),
      yaxis = list(title = "PM₂.₅ (µg/m³)"),
      hovermode = "x unified",
      margin = list(t = 80, b = 60, l = 60, r = 20),
      legend = list(
        orientation = "h",
        x = 0, xanchor = "left",
        y = 1.08, yanchor = "bottom",
        bgcolor = "rgba(255,255,255,0.85)",
        bordercolor = "#ccc", borderwidth = 1,
        font = list(size = 11)
      ),
      shapes = list(list(
        type = "line",
        x0 = min(city_df$date), x1 = max(city_df$date),
        y0 = 15, y1 = 15,
        line = list(color = "red", width = 1, dash = "dash")
      )),
      annotations = list(list(
        x = min(city_df$date), y = 15, xref = "x", yref = "y",
        text = "WHO 2021 AQG 15 µg/m³", showarrow = FALSE,
        xanchor = "left", yanchor = "bottom",
        font = list(color = "red", size = 10)
      ))
    ) %>%
    plotly::config(responsive = TRUE)
}

processed_dir <- here::here("data", "processed")
if (!dir.exists(processed_dir)) dir.create(processed_dir, recursive = TRUE)
build_fn_path <- file.path(processed_dir, "city_forecasts_build_fn.R")
writeLines(
  c("build_city_plot <- ", deparse(build_city_plot)),
  build_fn_path
)

docs_dir <- here::here("docs")
if (!dir.exists(docs_dir)) dir.create(docs_dir, recursive = TRUE)
# Render from a project-local qmd (not a tempfile): Quarto resolves resource
# paths relative to the qmd's location, and a tempfile in /var/folders forces
# pandoc to climb to the filesystem root and fail with a permission error.
qmd_tmp <- file.path(docs_dir, "_city_forecasts.qmd")

header <- c(
  "---",
  "title: \"PM2.5 forecasts — top Polish cities\"",
  sprintf(
    "subtitle: \"Actual vs. predicted at the nearest 10 km cell across the %d-day test window\"",
    length(test_dates)
  ),
  "format:",
  "  html:",
  "    embed-resources: true",
  "    toc: true",
  "    toc-depth: 2",
  "    toc-location: left",
  "    theme: cosmo",
  "    page-layout: full",
  "    grid:",
  "      body-width: 1200px",
  "      sidebar-width: 240px",
  "      margin-width: 100px",
  "---",
  "",
  "```{r setup, include=FALSE}",
  "knitr::opts_chunk$set(echo = FALSE, warning = FALSE, message = FALSE)",
  "library(plotly); library(dplyr); library(knitr)",
  # Absolute paths baked in: the qmd renders from a temp/other dir where here::here()
  # cannot find the project root, so readRDS(here::here(...)) would fail to open.
  sprintf("city_series  <- readRDS(%s)",
          encodeString(file.path(processed_dir, "city_forecasts_series.rds"), quote = "\"")),
  sprintf("city_metrics <- readRDS(%s)",
          encodeString(file.path(processed_dir, "city_forecasts_metrics.rds"), quote = "\"")),
  sprintf("model_colors <- readRDS(%s)",
          encodeString(file.path(processed_dir, "city_forecasts_model_colors.rds"), quote = "\"")),
  sprintf("source(%s)", encodeString(build_fn_path, quote = "\"")),
  "```",
  "",
  "## About this dashboard {.unnumbered}",
  "",
  sprintf(
    "Each chart below shows the daily actual PM2.5 (solid black line) and every trained model's one-day-ahead forecast (dotted, colour-coded) at the grid cell closest to a given city, across the %d-day held-out test window. The red dashed line marks the WHO 2021 Global Air Quality Guideline 24-hour PM2.5 limit of 15 µg/m³. Use the range slider, hover on a date for exact values, or click the legend to toggle individual traces.",
    length(test_dates)
  ),
  ""
)

city_chunks <- unlist(lapply(cities_df$name, function(cn) {
  c(
    sprintf("## %s {#%s}", cn, gsub("[^A-Za-z0-9]+", "-", cn)),
    "",
    sprintf("```{r fig-%s}", gsub("[^A-Za-z0-9]+", "-", cn)),
    "#| column: page",
    "#| out-width: 100%",
    sprintf("build_city_plot(city_series[[\"%s\"]], \"%s\")", cn, cn),
    "```",
    "",
    sprintf("```{r tbl-%s}", gsub("[^A-Za-z0-9]+", "-", cn)),
    sprintf("city_metrics %%>%% filter(City == \"%s\") %%>%% arrange(RMSE) %%>%% select(-City) %%>%% kable(digits = 2, caption = \"Per-city test-set accuracy (sorted by RMSE)\")", cn),
    "```",
    ""
  )
}))

writeLines(c(header, city_chunks), qmd_tmp)

processed_dir <- here::here("data", "processed")
saveRDS(city_series, file.path(processed_dir, "city_forecasts_series.rds"))
saveRDS(city_metrics, file.path(processed_dir, "city_forecasts_metrics.rds"))
saveRDS(model_colors, file.path(processed_dir, "city_forecasts_model_colors.rds"))

out_html   <- here::here("docs", "city_forecasts.html")
quarto_bin <- Sys.which("quarto")
qmd_base   <- basename(qmd_tmp)
files_dir  <- file.path(docs_dir, paste0(tools::file_path_sans_ext(qmd_base), "_files"))
old_wd     <- getwd()
tryCatch({
  # Render in docs_dir so output and resource paths stay inside the project.
  setwd(docs_dir)
  if (requireNamespace("quarto", quietly = TRUE)) {
    quarto::quarto_render(qmd_base, output_format = "html",
                          output_file = basename(out_html))
  } else if (nzchar(quarto_bin)) {
    system2(quarto_bin,
      args = c("render", qmd_base, "--to", "html", "--output", basename(out_html)))
  } else {
    rmarkdown::render(qmd_base, output_file = basename(out_html),
                      output_dir = ".", quiet = FALSE)
  }
}, error = function(e) {
  warning("city-forecast dashboard render skipped (no working quarto/rmarkdown): ",
          conditionMessage(e), call. = FALSE)
}, finally = {
  setwd(old_wd)
  unlink(c(qmd_tmp, files_dir), recursive = TRUE)
})

if (file.exists(out_html)) {
  cat(sprintf("\nInteractive dashboard saved to: %s\n", out_html))
} else {
  cat("\nDashboard not rendered (no working quarto/rmarkdown); data sidecars saved under data/processed/.\n")
}
cat(sprintf(
  "  Cities: %d | Models: %d | Test days: %d\n",
  nrow(cities_df), length(model_preds), length(test_dates)
))
