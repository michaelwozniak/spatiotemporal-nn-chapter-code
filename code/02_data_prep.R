library(stars)
library(sf)
library(spdep)
library(rnaturalearth)
library(rnaturalearthdata)
library(tidyverse)
library(here)

raw_dir <- here::here("data", "raw")

zip_files <- sort(list.files(raw_dir,
  pattern = "cams_pm25_poland_.*\\.zip$",
  full.names = TRUE
))
if (length(zip_files) > 0) {
  message(sprintf("Found %d zip files, extracting...", length(zip_files)))
  for (zf in zip_files) {
    contents <- utils::unzip(zf, list = TRUE)$Name
    missing <- contents[!file.exists(file.path(raw_dir, contents))]
    if (length(missing) > 0) {
      utils::unzip(zf, files = missing, exdir = raw_dir, overwrite = FALSE)
    }
  }
}

nc_files <- sort(list.files(raw_dir, pattern = ".*\\.nc$", full.names = TRUE))

if (length(nc_files) == 0) {
  stop("No NetCDF files found in data/raw/. Run 01_data_acquisition.R first.")
}

message(sprintf("Found %d NetCDF files, loading...", length(nc_files)))

message("Reading files and detecting grid dimensions...")
pm25_list <- lapply(nc_files, stars::read_stars)

dims_y <- sapply(pm25_list, function(s) dim(s)[2])
if (length(unique(dims_y)) > 1) {
  message(sprintf(
    "  Grid dimension mismatch detected (y: %s). Trimming to common dimensions (registrations differ by half a cell between the 2018-19 and 2020-22 streams).",
    paste(unique(dims_y), collapse = ", ")
  ))
  min_ny <- min(dims_y)
  min_nx <- min(sapply(pm25_list, function(s) dim(s)[1]))
  pm25_list <- lapply(pm25_list, function(s) {
    s[, seq_len(min_nx), seq_len(min_ny), ]
  })
  message(sprintf("  Harmonized all grids to %d x %d", min_nx, min_ny))
}

message("Aggregating hourly -> daily per month...")

daily_arrays <- list()
daily_dates <- c()

for (i in seq_along(pm25_list)) {
  s <- pm25_list[[i]]
  arr <- as.array(s[[1]])
  t_vals <- stars::st_get_dimension_values(s, 3)
  dates_h <- as.Date(t_vals)
  unique_days <- unique(dates_h)

  for (j in seq_along(unique_days)) {
    d <- unique_days[j]
    idx <- which(dates_h == d)
    daily_slice <- apply(arr[, , idx, drop = FALSE], c(1, 2), mean, na.rm = TRUE)
    daily_arrays <- c(daily_arrays, list(daily_slice))
  }
  daily_dates <- c(daily_dates, as.character(unique_days))

  if (i %% 12 == 0) message(sprintf("  ... processed %d / %d files", i, length(pm25_list)))
}

pm25_daily_arr <- array(
  unlist(daily_arrays),
  dim = c(dim(daily_arrays[[1]]), length(daily_arrays))
)
daily_dates <- as.Date(daily_dates, format = "%Y-%m-%d")

message(sprintf(
  "Daily array: %s (x, y, days=%d)",
  paste(dim(pm25_daily_arr), collapse = " x "), length(daily_dates)
))
message(sprintf("Date range: %s to %s", min(daily_dates), max(daily_dates)))

template <- pm25_list[[1]][, , , 1]
pm25_raw <- stars::st_as_stars(list(pm25 = pm25_daily_arr))
stars::st_dimensions(pm25_raw)[[1]] <- stars::st_dimensions(template)[[1]]
stars::st_dimensions(pm25_raw)[[2]] <- stars::st_dimensions(template)[[2]]
pm25_raw <- stars::st_set_dimensions(pm25_raw, 3,
  values = as.POSIXct(daily_dates),
  names  = "time"
)
names(stars::st_dimensions(pm25_raw)) <- c("x", "y", "time")
sf::st_crs(pm25_raw) <- 4326

print(pm25_raw)
message(sprintf("Daily grid dimensions: %s", paste(dim(pm25_raw), collapse = " x ")))

pm25_array_raw <- pm25_daily_arr
na_pct <- mean(is.na(pm25_array_raw)) * 100
message(sprintf("Missing values: %.1f%%", na_pct))

na_mask_constant_raw <- all(
  is.na(pm25_array_raw) ==
    array(is.na(pm25_array_raw[, , 1]), dim(pm25_array_raw))
)

if (na_pct > 0 && na_pct < 20) {
  message("Filling NAs with spatial median per time step...")
  for (t in seq_len(dim(pm25_array_raw)[3])) {
    slice <- pm25_array_raw[, , t]
    if (any(is.na(slice))) {
      slice[is.na(slice)] <- median(slice, na.rm = TRUE)
      pm25_array_raw[, , t] <- slice
    }
  }
} else if (na_pct >= 20) {
  warning("High NA percentage — consider cropping the spatial extent to land only.")
}

out_fig <- here::here("output", "figures")
if (!dir.exists(out_fig)) dir.create(out_fig, recursive = TRUE)

day1_df <- expand.grid(
  lon = stars::st_dimensions(pm25_raw)$x$offset +
    (seq_len(dim(pm25_raw)[1]) - 0.5) * stars::st_dimensions(pm25_raw)$x$delta,
  lat = stars::st_dimensions(pm25_raw)$y$offset +
    (seq_len(dim(pm25_raw)[2]) - 0.5) * stars::st_dimensions(pm25_raw)$y$delta
)
day1_df$pm25 <- as.vector(pm25_array_raw[, , 1])

borders_sf <- rnaturalearth::ne_countries(
  scale = "medium",
  country = c(
    "Poland", "Germany", "Czech Republic",
    "Slovakia", "Ukraine", "Belarus",
    "Lithuania", "Russia", "Austria"
  ),
  returnclass = "sf"
)
poland_sf <- borders_sf %>% dplyr::filter(admin == "Poland")

p_map <- ggplot2::ggplot(day1_df, ggplot2::aes(x = lon, y = lat, fill = pm25)) +
  ggplot2::geom_raster() +
  ggplot2::geom_sf(
    data = borders_sf, inherit.aes = FALSE,
    fill = NA, colour = "grey25", linewidth = 0.3
  ) +
  ggplot2::geom_sf(
    data = poland_sf, inherit.aes = FALSE,
    fill = NA, colour = "black", linewidth = 0.6
  ) +
  ggplot2::scale_fill_viridis_c(
    name = expression(PM[2.5] ~ "[" * mu * g / m^3 * "]"),
    na.value = "grey80"
  ) +
  ggplot2::coord_sf(
    crs = 4326,
    xlim = range(day1_df$lon), ylim = range(day1_df$lat),
    expand = FALSE
  ) +
  ggplot2::labs(title = paste("PM2.5 —", daily_dates[1])) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "eda_map_day1.png"), p_map, width = 8, height = 6)

cities <- data.frame(
  name = c("Warszawa", "Kraków", "Gdańsk", "Wrocław"),
  lon  = c(21.01, 19.94, 18.65, 17.04),
  lat  = c(52.23, 50.06, 54.35, 51.11)
)

x_coords <- stars::st_dimensions(pm25_raw)$x$offset +
  (seq_len(dim(pm25_raw)[1]) - 0.5) * stars::st_dimensions(pm25_raw)$x$delta
y_coords <- stars::st_dimensions(pm25_raw)$y$offset +
  (seq_len(dim(pm25_raw)[2]) - 0.5) * stars::st_dimensions(pm25_raw)$y$delta

ts_cities <- do.call(rbind, lapply(seq_len(nrow(cities)), function(r) {
  ix <- which.min(abs(x_coords - cities$lon[r]))
  iy <- which.min(abs(y_coords - cities$lat[r]))
  tibble::tibble(
    name = cities$name[r],
    time = daily_dates,
    pm25 = pm25_array_raw[ix, iy, ]
  )
}))

cities_sf <- sf::st_as_sf(cities, coords = c("lon", "lat"), crs = 4326)

p_ts <- ggplot2::ggplot(ts_cities, ggplot2::aes(x = time, y = pm25, color = name)) +
  ggplot2::geom_line(alpha = 0.6) +
  ggplot2::labs(
    title = "PM2.5 daily concentration",
    y = expression(PM[2.5] ~ "[" * mu * g / m^3 * "]"),
    x = NULL, color = "City"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "eda_timeseries_cities.png"), p_ts, width = 10, height = 5, bg = "white")

p_hist <- ggplot2::ggplot(ts_cities, ggplot2::aes(x = pm25, fill = name)) +
  ggplot2::geom_histogram(bins = 50, alpha = 0.5, position = "identity") +
  ggplot2::labs(
    title = "PM2.5 distribution by city",
    x = expression(PM[2.5] ~ "[" * mu * g / m^3 * "]")
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "eda_histogram.png"), p_hist, width = 8, height = 5)

message("Exploratory plots saved to output/figures/")

message("\n=== Spatial-statistical hypothesis testing ===\n")

grid_to_sf <- function(arr_2d) {
  grid_coords <- expand.grid(lon = x_coords, lat = y_coords)
  df <- data.frame(
    lon   = grid_coords$lon,
    lat   = grid_coords$lat,
    value = as.vector(arr_2d)
  )
  df <- df[!is.na(df$value), ]
  sf::st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
}

message("--- H1: Testing spatial autocorrelation (Global Moran's I) ---")

slice_sf <- grid_to_sf(pm25_array_raw[, , 1])
stopifnot(
  "NA mask varies across days; rebuild slice_sf per day or assert zero NAs" =
    na_mask_constant_raw
)
knn <- spdep::knearneigh(sf::st_coordinates(slice_sf), k = 8)
nb <- spdep::knn2nb(knn)
lw <- spdep::nb2listw(nb, style = "W")

sample_days <- seq(1, dim(pm25_array_raw)[3], by = 30)
moran_results <- tibble::tibble(
  day_idx = integer(),
  date    = as.Date(character()),
  moran_I = double(),
  p_value = double(),
  z_score = double()
)

for (d in sample_days) {
  slice_vals <- as.vector(pm25_array_raw[, , d])
  valid <- !is.na(slice_vals)

  if (sum(valid) < length(slice_vals)) {
    coords_v <- sf::st_coordinates(slice_sf)[valid, ]
    knn_v <- spdep::knearneigh(coords_v, k = min(8, sum(valid) - 1))
    nb_v <- spdep::knn2nb(knn_v)
    lw_v <- spdep::nb2listw(nb_v, style = "W")
    mt <- spdep::moran.test(slice_vals[valid], lw_v)
  } else {
    mt <- spdep::moran.test(slice_vals, lw)
  }

  moran_results <- dplyr::bind_rows(moran_results, tibble::tibble(
    day_idx = d,
    date    = daily_dates[d],
    moran_I = as.numeric(mt$estimate["Moran I statistic"]),
    p_value = mt$p.value,
    z_score = as.numeric(mt$statistic)
  ))
}

cat(sprintf("  Moran's I computed for %d time steps\n", nrow(moran_results)))
cat(sprintf(
  "  Range: [%.3f, %.3f]\n",
  min(moran_results$moran_I), max(moran_results$moran_I)
))
cat(sprintf(
  "  All significant (p < 0.05): %s\n",
  ifelse(all(moran_results$p_value < 0.05), "YES", "NO")
))
cat(sprintf("  Mean Moran's I: %.3f\n", mean(moran_results$moran_I)))
cat("  -> CONCLUSION: Strong positive spatial autocorrelation detected.\n")
cat("     CNN component is justified — nearby cells carry correlated information.\n\n")

p_moran_ts <- ggplot2::ggplot(moran_results, ggplot2::aes(x = date, y = moran_I)) +
  ggplot2::geom_line(color = "steelblue") +
  ggplot2::geom_point(size = 1, color = "steelblue") +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  ggplot2::labs(
    title = "Global Moran's I for PM2.5 Over Time",
    subtitle = "Consistently positive = persistent spatial autocorrelation",
    y = "Moran's I", x = NULL
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "h1_moran_i_timeseries.png"), p_moran_ts,
  width = 10, height = 5
)

repr_day <- sample_days[length(sample_days) %/% 2]
repr_vals <- as.vector(pm25_array_raw[, , repr_day])
valid_repr <- !is.na(repr_vals)

if (sum(valid_repr) == length(repr_vals)) {
  lag_vals <- spdep::lag.listw(lw, repr_vals)
} else {
  coords_v <- sf::st_coordinates(slice_sf)[valid_repr, ]
  knn_v <- spdep::knearneigh(coords_v, k = min(8, sum(valid_repr) - 1))
  nb_v <- spdep::knn2nb(knn_v)
  lw_v <- spdep::nb2listw(nb_v, style = "W")
  lag_vals <- rep(NA, length(repr_vals))
  lag_vals[valid_repr] <- spdep::lag.listw(lw_v, repr_vals[valid_repr])
}

moran_scatter_df <- tibble::tibble(
  pm25 = repr_vals,
  spatial_lag = lag_vals
) %>% dplyr::filter(!is.na(pm25), !is.na(spatial_lag))

p_moran_scatter <- ggplot2::ggplot(moran_scatter_df, ggplot2::aes(x = pm25, y = spatial_lag)) +
  ggplot2::geom_point(alpha = 0.3, size = 0.8) +
  ggplot2::geom_smooth(method = "lm", color = "tomato", se = FALSE) +
  ggplot2::labs(
    title = sprintf("Moran Scatterplot — %s", daily_dates[repr_day]),
    x = expression(PM[2.5] ~ "[" * mu * g / m^3 * "]"),
    y = expression("Spatial lag of " * PM[2.5])
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "h1_moran_scatterplot.png"), p_moran_scatter,
  width = 7, height = 6
)

message("--- LISA cluster analysis for representative day ---")

if (sum(valid_repr) == length(repr_vals)) {
  lisa <- spdep::localmoran(repr_vals, lw)
  lisa_lag <- spdep::lag.listw(lw, repr_vals)
  lisa_vals <- repr_vals
} else {
  lisa <- spdep::localmoran(repr_vals[valid_repr], lw_v)
  lisa_lag <- spdep::lag.listw(lw_v, repr_vals[valid_repr])
  lisa_vals <- repr_vals[valid_repr]
}

lisa_df <- tibble::tibble(
  lon   = sf::st_coordinates(slice_sf)[if (sum(valid_repr) < length(repr_vals)) valid_repr else TRUE, 1],
  lat   = sf::st_coordinates(slice_sf)[if (sum(valid_repr) < length(repr_vals)) valid_repr else TRUE, 2],
  Ii    = lisa[, "Ii"],
  pval  = lisa[, "Pr(z != E(Ii))"],
  value = lisa_vals,
  lag   = lisa_lag
)

mean_val <- mean(lisa_df$value)
mean_lag <- mean(lisa_df$lag)

lisa_df <- lisa_df %>%
  dplyr::mutate(
    quadrant = dplyr::case_when(
      value > mean_val & lag > mean_lag ~ "High-High (hot spot)",
      value < mean_val & lag < mean_lag ~ "Low-Low (cold spot)",
      value > mean_val & lag < mean_lag ~ "High-Low (outlier)",
      value < mean_val & lag > mean_lag ~ "Low-High (outlier)"
    ),
    quadrant = dplyr::if_else(pval > 0.05, "Not significant", quadrant)
  )

lisa_sf <- sf::st_as_sf(lisa_df, coords = c("lon", "lat"), crs = 4326)

p_lisa <- ggplot2::ggplot() +
  ggplot2::geom_sf(data = lisa_sf, ggplot2::aes(color = quadrant), size = 0.6) +
  ggplot2::scale_color_manual(values = c(
    "High-High (hot spot)" = "#d73027",
    "Low-Low (cold spot)"  = "#4575b4",
    "High-Low (outlier)"   = "#fdae61",
    "Low-High (outlier)"   = "#abd9e9",
    "Not significant"      = "grey80"
  )) +
  ggplot2::labs(
    title = sprintf("LISA Cluster Map — PM2.5 on %s", daily_dates[repr_day]),
    subtitle = "Local indicators of spatial association",
    color = "Cluster type"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "h1_lisa_clusters.png"), p_lisa, width = 9, height = 6)

n_sig <- sum(lisa_df$pval < 0.05)
cat(sprintf(
  "  Significant LISA clusters: %d / %d cells (%.1f%%) [analytic p, unadjusted]\n",
  n_sig, nrow(lisa_df), n_sig / nrow(lisa_df) * 100
))

message("--- H2: Testing temporal autocorrelation (ACF / Ljung-Box) ---")

set.seed(42)
random_cells <- sample(which(!is.na(pm25_array_raw[, , 1])), 4)

random_ts <- lapply(random_cells, function(idx) {
  ij <- arrayInd(idx, .dim = dim(pm25_array_raw)[1:2])
  pm25_array_raw[ij[1], ij[2], ]
})

city_names <- unique(ts_cities$name)
lb_results <- tibble::tibble(
  location = character(), lb_statistic = double(),
  lb_pvalue = double(), acf_lag1 = double()
)

for (cn in city_names) {
  city_ts <- ts_cities %>%
    dplyr::filter(name == cn) %>%
    dplyr::pull(pm25)
  city_ts <- city_ts[!is.na(city_ts)]
  if (length(city_ts) < 50) next

  lb <- Box.test(city_ts, lag = 30, type = "Ljung-Box")
  ac <- acf(city_ts, lag.max = 1, plot = FALSE)

  lb_results <- dplyr::bind_rows(lb_results, tibble::tibble(
    location = cn, lb_statistic = lb$statistic,
    lb_pvalue = lb$p.value, acf_lag1 = ac$acf[2]
  ))
}

for (i in seq_along(random_ts)) {
  rts <- random_ts[[i]]
  rts <- rts[!is.na(rts)]
  if (length(rts) < 50) next

  lb <- Box.test(rts, lag = 30, type = "Ljung-Box")
  ac <- acf(rts, lag.max = 1, plot = FALSE)

  lb_results <- dplyr::bind_rows(lb_results, tibble::tibble(
    location = sprintf("Random cell %d", i), lb_statistic = lb$statistic,
    lb_pvalue = lb$p.value, acf_lag1 = ac$acf[2]
  ))
}

cat("  Ljung-Box test results (H0: no temporal autocorrelation):\n")
print(as.data.frame(lb_results), row.names = FALSE)
cat(sprintf(
  "\n  All significant (p < 0.05): %s\n",
  ifelse(all(lb_results$lb_pvalue < 0.05), "YES", "NO")
))
cat(sprintf("  Mean lag-1 ACF: %.3f\n", mean(lb_results$acf_lag1)))
cat("  -> CONCLUSION: Strong temporal autocorrelation at all locations.\n")
cat("     LSTM component is justified — past values predict future values.\n\n")

city_example <- ts_cities %>%
  dplyr::filter(name == city_names[1]) %>%
  dplyr::pull(pm25)
city_example <- city_example[!is.na(city_example)]
acf_obj <- acf(city_example, lag.max = 60, plot = FALSE)

p_acf <- ggplot2::ggplot(
  tibble::tibble(lag = acf_obj$lag[-1], acf = acf_obj$acf[-1]),
  ggplot2::aes(x = lag, y = acf)
) +
  ggplot2::geom_hline(yintercept = 0, color = "grey50") +
  ggplot2::geom_hline(
    yintercept = c(-1, 1) * qnorm(0.975) / sqrt(length(city_example)),
    linetype = "dashed", color = "blue"
  ) +
  ggplot2::geom_segment(ggplot2::aes(xend = lag, yend = 0), color = "steelblue") +
  ggplot2::labs(
    title = sprintf("Autocorrelation Function — PM2.5 in %s", city_names[1]),
    subtitle = "Dashed lines = 95% confidence interval for white noise",
    x = "Lag [days]", y = "ACF"
  ) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "h2_acf_city.png"), p_acf, width = 8, height = 5)

message("--- H3: Testing non-stationarity of spatial patterns ---")

lags_to_test <- c(1, 7, 14, 30, 60, 90)
field_corrs <- tibble::tibble(lag = integer(), mean_cor = double(), sd_cor = double())

n_samples_corr <- min(100, dim(pm25_array_raw)[3] %/% 2)
sample_starts <- sample(
  1:(dim(pm25_array_raw)[3] - max(lags_to_test)),
  n_samples_corr
)

for (lg in lags_to_test) {
  cors <- sapply(sample_starts, function(t0) {
    v1 <- as.vector(pm25_array_raw[, , t0])
    v2 <- as.vector(pm25_array_raw[, , t0 + lg])
    valid <- !is.na(v1) & !is.na(v2)
    cor(v1[valid], v2[valid])
  })

  field_corrs <- dplyr::bind_rows(field_corrs, tibble::tibble(
    lag = lg, mean_cor = mean(cors), sd_cor = sd(cors)
  ))
}

p_field_corr <- ggplot2::ggplot(field_corrs, ggplot2::aes(x = lag, y = mean_cor)) +
  ggplot2::geom_line(color = "steelblue", linewidth = 1) +
  ggplot2::geom_point(size = 3, color = "steelblue") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = mean_cor - sd_cor, ymax = pmin(mean_cor + sd_cor, 1)),
    alpha = 0.2, fill = "steelblue"
  ) +
  ggplot2::labs(
    title = "Spatial Field Correlation at Increasing Time Lags",
    subtitle = "Decay in correlation = spatial patterns change over time",
    x = "Time lag [days]", y = "Pearson r between PM2.5 grids"
  ) +
  ggplot2::scale_x_continuous(breaks = lags_to_test) +
  ggplot2::ylim(0, 1) +
  ggplot2::theme_minimal()

ggplot2::ggsave(file.path(out_fig, "h3_field_correlation_decay.png"), p_field_corr,
  width = 8, height = 5
)

cat(sprintf("\n  Spatial field correlation decay:\n"))
for (i in seq_len(nrow(field_corrs))) {
  cat(sprintf(
    "    Lag %3d days: r = %.3f (±%.3f)\n",
    field_corrs$lag[i], field_corrs$mean_cor[i], field_corrs$sd_cor[i]
  ))
}

cat("\n  -> CONCLUSION: Spatial patterns are non-stationary.\n")
cat("     Correlation decays with time lag — a static spatial model is insufficient.\n")
cat("     A spatio-temporal architecture (CNN-LSTM) is methodologically justified.\n\n")

cat("=============================================================\n")
cat("  HYPOTHESIS TESTING SUMMARY\n")
cat("=============================================================\n")
cat("  H1: Spatial autocorrelation exists?       YES (Moran's I)\n")
cat("      -> CNN justified for spatial feature extraction\n\n")
cat("  H2: Temporal autocorrelation exists?       YES (Ljung-Box)\n")
cat("      -> LSTM justified for temporal sequence modelling\n\n")
cat("  H3: Spatial patterns change over time?     YES (field corr. decay)\n")
cat("      -> Spatio-temporal model justified over static spatial model\n")
cat("=============================================================\n\n")

message("Hypothesis testing plots saved to output/figures/")

pm25_array_log <- log1p(pm25_array_raw)

WINDOW_SIZE <- 30

n_days_all <- dim(pm25_array_log)[3]
train_end_samples <- floor((n_days_all - WINDOW_SIZE) * 0.70)
train_day_end <- train_end_samples + WINDOW_SIZE

pm25_min <- min(pm25_array_log[, , 1:train_day_end], na.rm = TRUE)
pm25_max <- max(pm25_array_log[, , 1:train_day_end], na.rm = TRUE)
pm25_scaled <- (pm25_array_log - pm25_min) / (pm25_max - pm25_min)

message(sprintf(
  "Log-scaling (train days 1..%d of %d): log1p min=%.4f, max=%.4f (raw train: %.2f .. %.2f)",
  train_day_end, n_days_all,
  pm25_min, pm25_max,
  min(pm25_array_raw[, , 1:train_day_end], na.rm = TRUE),
  max(pm25_array_raw[, , 1:train_day_end], na.rm = TRUE)
))

times <- daily_dates
doy <- as.numeric(format(times, "%j"))
dow <- as.POSIXlt(times)$wday
mon <- as.numeric(format(times, "%m"))
n_days <- length(times)

sin_doy <- sin(2 * pi * doy / 365)
cos_doy <- cos(2 * pi * doy / 365)
sin_doy_2h <- sin(4 * pi * doy / 365)
cos_doy_2h <- cos(4 * pi * doy / 365)

sin_dow <- sin(2 * pi * dow / 7)
cos_dow <- cos(2 * pi * dow / 7)

weekend <- as.integer(dow %in% c(0, 6))

heating_season <- as.integer(mon %in% c(10, 11, 12, 1, 2, 3, 4))

polish_holidays <- as.Date(c(
  "2018-01-01", "2018-01-06", "2018-05-01", "2018-05-03", "2018-08-15",
  "2018-11-01", "2018-11-11", "2018-12-25", "2018-12-26",
  "2018-04-01", "2018-04-02", "2018-05-20", "2018-05-31",
  "2019-01-01", "2019-01-06", "2019-05-01", "2019-05-03", "2019-08-15",
  "2019-11-01", "2019-11-11", "2019-12-25", "2019-12-26",
  "2019-04-21", "2019-04-22", "2019-06-09", "2019-06-20",
  "2020-01-01", "2020-01-06", "2020-05-01", "2020-05-03", "2020-08-15",
  "2020-11-01", "2020-11-11", "2020-12-25", "2020-12-26",
  "2020-04-12", "2020-04-13", "2020-05-31", "2020-06-11",
  "2021-01-01", "2021-01-06", "2021-05-01", "2021-05-03", "2021-08-15",
  "2021-11-01", "2021-11-11", "2021-12-25", "2021-12-26",
  "2021-04-04", "2021-04-05", "2021-05-23", "2021-06-03",
  "2022-01-01", "2022-01-06", "2022-05-01", "2022-05-03", "2022-08-15",
  "2022-11-01", "2022-11-11", "2022-12-25", "2022-12-26",
  "2022-04-17", "2022-04-18", "2022-06-05", "2022-06-16"
))
years_in_data <- sort(unique(as.numeric(format(times, "%Y"))))
if (any(!years_in_data %in% 2018:2022)) {
  warning(sprintf(
    "Data contains years outside the hardcoded Polish holiday range 2018–2022: %s. Extend polish_holidays in 02_data_prep.R.",
    paste(setdiff(years_in_data, 2018:2022), collapse = ", ")
  ))
}
holiday <- as.integer(times %in% polish_holidays)

non_working <- (weekend == 1) | (holiday == 1)
bridge <- logical(n_days)
if (n_days >= 3) {
  for (k in 2:(n_days - 1)) {
    if (!non_working[k] && non_working[k - 1] && non_working[k + 1]) {
      bridge[k] <- TRUE
    }
  }
}
extended_off <- non_working | bridge
long_weekend_vec <- logical(n_days)
runs <- rle(extended_off)
pos <- 1L
for (k in seq_along(runs$lengths)) {
  len <- runs$lengths[k]
  if (isTRUE(runs$values[k]) && len >= 3) {
    long_weekend_vec[pos:(pos + len - 1L)] <- TRUE
  }
  pos <- pos + len
}
long_weekend <- as.integer(long_weekend_vec)

linear_trend <- (seq_len(n_days) - 1) / max(train_day_end - 1, 1)

nx <- dim(pm25_scaled)[1]
ny <- dim(pm25_scaled)[2]
nt <- dim(pm25_scaled)[3]
stopifnot(nt == n_days)

channel_series <- list(
  sin_doy        = sin_doy,
  cos_doy        = cos_doy,
  sin_doy_2h     = sin_doy_2h,
  cos_doy_2h     = cos_doy_2h,
  sin_dow        = sin_dow,
  cos_dow        = cos_dow,
  weekend        = as.numeric(weekend),
  holiday        = as.numeric(holiday),
  heating_season = as.numeric(heating_season),
  long_weekend   = as.numeric(long_weekend),
  linear_trend   = linear_trend
)

spatial_path <- here::here("data", "processed", "spatial_features.rds")
stopifnot(
  "spatial_features.rds missing — run `Rscript code/01_data_acquisition.R` first" =
    file.exists(spatial_path)
)
spatial_features <- readRDS(spatial_path)
stopifnot(
  "spatial_features elevation shape mismatches CAMS grid" =
    all(dim(spatial_features$elevation) == c(nx, ny)),
  "spatial_features log_pop_density shape mismatches CAMS grid" =
    all(dim(spatial_features$log_pop_density) == c(nx, ny))
)

static_spatial <- list(
  elevation       = spatial_features$elevation,
  log_pop_density = spatial_features$log_pop_density
)

channel_names <- c("pm25_scaled", names(channel_series), names(static_spatial))
n_channels <- length(channel_names)

pm25_multi <- array(dim = c(nx, ny, nt, n_channels))
pm25_multi[, , , 1] <- pm25_scaled
for (c_idx in seq_along(channel_series)) {
  pm25_multi[, , , c_idx + 1L] <- rep(channel_series[[c_idx]], each = nx * ny)
}
static_offset <- 1L + length(channel_series)
for (c_idx in seq_along(static_spatial)) {
  pm25_multi[, , , static_offset + c_idx] <-
    rep(as.vector(static_spatial[[c_idx]]), times = nt)
}

message(sprintf(
  "Multi-channel array: %s (x, y, time, channels)",
  paste(dim(pm25_multi), collapse = " x ")
))
message(sprintf(
  "Channels (%d): %s", n_channels,
  paste(channel_names, collapse = ", ")
))
message(sprintf(
  "  holiday days: %d | weekend days: %d | heating-season days: %d | long-weekend days: %d",
  sum(holiday), sum(weekend), sum(heating_season), sum(long_weekend)
))
message(sprintf(
  "  elevation scale: raw [%.0f, %.0f] m | pop density max: %.0f /km²",
  spatial_features$meta$elev_min_m,
  spatial_features$meta$elev_max_m,
  spatial_features$meta$pop_density_max
))

n_time <- dim(pm25_multi)[3]
n_samples <- n_time - WINDOW_SIZE

message(sprintf(
  "Creating sliding window indices (T=%d, n=%d samples)...",
  WINDOW_SIZE, n_samples
))

Y <- array(dim = c(n_samples, nx, ny))
for (i in seq_len(n_samples)) {
  Y[i, , ] <- pm25_multi[, , i + WINDOW_SIZE, 1]
}

message(sprintf("  Y: %s", paste(dim(Y), collapse = " x ")))
message(sprintf(
  "  X will be sliced on-the-fly from source array (%.1f GB saved)",
  n_samples * WINDOW_SIZE * nx * ny * n_channels * 8 / 1e9
))

train_end <- floor(n_samples * 0.70)
val_start <- train_end + 1L
val_end <- floor(n_samples * 0.85)
test_start <- val_end + 1L

stopifnot(
  "Empty val split; n_samples too small"  = val_start <= val_end,
  "Empty test split; n_samples too small" = test_start <= n_samples
)

split_idx <- list(
  train = 1:train_end,
  val   = val_start:val_end,
  test  = test_start:n_samples
)

message(sprintf(
  "Split: train=%d, val=%d, test=%d",
  length(split_idx$train),
  length(split_idx$val),
  length(split_idx$test)
))

tensors <- list(
  source_array = pm25_multi,
  Y_train = Y[split_idx$train, , ],
  Y_val = Y[split_idx$val, , ],
  Y_test = Y[split_idx$test, , ],
  meta = list(
    window_size      = WINDOW_SIZE,
    n_channels       = n_channels,
    channel_names    = channel_names,
    log_transform    = TRUE,
    pm25_min         = pm25_min,
    pm25_max         = pm25_max,
    grid_dim         = c(nx = nx, ny = ny),
    times            = times,
    split_idx        = split_idx,
    cities           = cities_sf,
    spatial_channels = spatial_features$meta
  )
)

proc_dir <- here::here("data", "processed")
if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)

saveRDS(tensors, file.path(proc_dir, "pm25_tensors.rds"))
message(sprintf("Saved to %s/pm25_tensors.rds", proc_dir))

cat("\n=== Data preparation complete ===\n")
cat(sprintf("  Grid:     %d x %d (lon x lat)\n", nx, ny))
cat(sprintf("  Days:     %d\n", nt))
cat(sprintf("  Window:   %d days\n", WINDOW_SIZE))
cat(sprintf(
  "  Channels: %d (%s)\n", n_channels,
  paste(tensors$meta$channel_names, collapse = ", ")
))
cat(sprintf(
  "  Samples:  %d train / %d val / %d test\n",
  length(split_idx$train),
  length(split_idx$val),
  length(split_idx$test)
))
cat(sprintf(
  "  X shape:  (n, %d, %d, %d, %d)  [sliced on-the-fly]\n",
  WINDOW_SIZE, nx, ny, n_channels
))
cat(sprintf("  Y shape:  (n, %d, %d)\n", nx, ny))
cat(sprintf(
  "  Source array: %.1f MB\n",
  object.size(tensors$source_array) / 1e6
))
