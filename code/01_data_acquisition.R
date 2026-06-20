suppressPackageStartupMessages({
  library(ecmwfr)
  library(terra)
  library(sf)
  library(here)
})

OUT_DIR <- here::here("data", "raw")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

cams_expected <- sprintf("cams_pm25_poland_%d_%02d",
                         rep(2018:2022, each = 12), rep(1:12, times = 5))
have_nc  <- all(file.exists(file.path(OUT_DIR, paste0(cams_expected, ".nc"))))
have_zip <- all(file.exists(file.path(OUT_DIR, paste0(cams_expected, ".zip"))))

if (have_nc || have_zip) {
  message(sprintf(
    "All 60 CAMS monthly files already present under %s — skipping download.",
    OUT_DIR))
} else {
  dotenv::load_dot_env(here::here(".env"))
  CDS_USER    <- Sys.getenv("CDS_USER")
  CDS_API_KEY <- Sys.getenv("CDS_API_KEY")

  if (CDS_USER == "" || CDS_API_KEY == "") {
    stop("Missing CDS_USER or CDS_API_KEY in .env file. See .env.example")
  }
  ecmwfr::wf_set_key(key = CDS_API_KEY, user = CDS_USER)

  NORTH <- 55; WEST <- 14; SOUTH <- 49; EAST <- 25

  YEARS  <- 2018:2022
  MONTHS <- sprintf("%02d", 1:12)

  for (yr in YEARS) {
    for (mo in MONTHS) {
      target_file <- sprintf("cams_pm25_poland_%d_%s.nc", yr, mo)
      out_path    <- file.path(OUT_DIR, target_file)

      if (file.exists(out_path)) {
        message(sprintf("Skipping %s (already exists)", target_file))
        next
      }

      message(sprintf("Requesting PM2.5 for %d-%s ...", yr, mo))

      request <- list(
        dataset_short_name = "cams-europe-air-quality-reanalyses",
        variable    = "particulate_matter_2.5um",
        model       = "ensemble",
        level       = "0",
        type        = "validated_reanalysis",
        year        = as.character(yr),
        month       = mo,
        data_format = "netcdf",
        area        = c(NORTH, WEST, SOUTH, EAST),
        target      = target_file
      )

      tryCatch(
        {
          ecmwfr::wf_request(
            request  = request,
            transfer = TRUE,
            path     = OUT_DIR,
            user     = CDS_USER,
            verbose  = TRUE
          )
          message(sprintf("  -> Saved: %s", out_path))
        },
        error = function(e) {
          message(sprintf("  -> ERROR for %d-%s: %s", yr, mo, e$message))
        }
      )
    }
  }

  message("CAMS download complete. Check data/raw/ for NetCDF files.")
}

SPATIAL_OUT <- here::here("data", "processed", "spatial_features.rds")

GRID_NX   <- 110L
GRID_NY   <- 60L
LON_MIN   <- 14.0; LON_MAX <- 24.9; LON_STEP <-  0.1
LAT_MAX   <- 55.0; LAT_MIN <- 49.1; LAT_STEP <- -0.1

lon_centres <- LON_MIN + seq_len(GRID_NX) * LON_STEP - LON_STEP / 2
lat_centres <- LAT_MAX + seq_len(GRID_NY) * LAT_STEP - LAT_STEP / 2

template <- terra::rast(
  xmin = LON_MIN - LON_STEP / 2, xmax = LON_MAX + LON_STEP / 2,
  ymin = LAT_MIN + LAT_STEP / 2, ymax = LAT_MAX - LAT_STEP / 2,
  ncols = GRID_NX, nrows = GRID_NY,
  crs  = "EPSG:4326"
)

spatial_needs_build <- TRUE
if (file.exists(SPATIAL_OUT)) {
  existing <- readRDS(SPATIAL_OUT)
  ok_shape <- identical(dim(existing$elevation), c(GRID_NX, GRID_NY)) &&
              identical(dim(existing$log_pop_density), c(GRID_NX, GRID_NY))
  if (ok_shape) {
    message(sprintf("spatial_features.rds exists with correct shape — nothing to do. Delete %s to rebuild.",
                    SPATIAL_OUT))
    spatial_needs_build <- FALSE
  } else {
    message("Existing spatial_features.rds has wrong shape; rebuilding.")
  }
}

if (spatial_needs_build) {
  raw_spatial_dir <- here::here("data", "raw", "spatial")
  dir.create(raw_spatial_dir, recursive = TRUE, showWarnings = FALSE)

  message("Fetching AWS Terrain Tiles DEM via elevatr (src='aws')…")

  bbox_sf <- sf::st_as_sfc(sf::st_bbox(c(
    xmin = LON_MIN - 0.2, xmax = LON_MAX + 0.2,
    ymin = LAT_MIN - 0.2, ymax = LAT_MAX + 0.2
  ), crs = 4326))
  bbox_sf <- sf::st_sf(geometry = bbox_sf, id = 1L)

  dem_raster <- elevatr::get_elev_raster(
    locations = bbox_sf,
    z         = 6,
    prj       = "EPSG:4326",
    clip      = "bbox"
  )
  dem_terra <- terra::rast(dem_raster)
  message(sprintf("  native DEM: %d x %d cells, res %.5f°",
                  terra::ncol(dem_terra), terra::nrow(dem_terra), terra::res(dem_terra)[1]))

  elev_10km <- terra::resample(dem_terra, template, method = "average")

  elev_mat <- t(matrix(as.vector(elev_10km), nrow = GRID_NY, ncol = GRID_NX,
                       byrow = TRUE))
  stopifnot(dim(elev_mat) == c(GRID_NX, GRID_NY))

  elev_mat <- pmax(elev_mat, 0)

  message(sprintf("  aggregated elevation: mean %.1f m, range [%.0f, %.0f] m, NAs: %d",
                  mean(elev_mat, na.rm = TRUE),
                  min(elev_mat,  na.rm = TRUE),
                  max(elev_mat,  na.rm = TRUE),
                  sum(is.na(elev_mat))))

  stopifnot(
    "Elevation grid has NAs — AWS DEM tile did not cover the full CAMS extent" =
      !anyNA(elev_mat),
    "Elevation range outside plausible bounds for Poland" =
      all(elev_mat >= 0 & elev_mat <= 2600)
  )

  elev_min <- min(elev_mat); elev_max <- max(elev_mat)
  elevation_scaled <- (elev_mat - elev_min) / (elev_max - elev_min)

  pop_url <- "https://data.worldpop.org/GIS/Population/Global_2000_2020/2018/POL/pol_ppp_2018.tif"
  pop_tif <- file.path(raw_spatial_dir, "pol_ppp_2018.tif")

  if (!file.exists(pop_tif)) {
    message("Downloading WorldPop Poland 2018 TIF (~260 MB, a few minutes on typical broadband)…")
    old_timeout <- getOption("timeout"); on.exit(options(timeout = old_timeout), add = TRUE)
    options(timeout = 1800)
    utils::download.file(pop_url, pop_tif, mode = "wb", quiet = FALSE)
  }
  stopifnot("WorldPop TIF did not download" = file.exists(pop_tif))

  pop_raster <- terra::rast(pop_tif)
  message(sprintf("  native WorldPop: %d x %d cells, res %.5f°",
                  terra::ncol(pop_raster), terra::nrow(pop_raster), terra::res(pop_raster)[1]))

  pop_sum <- terra::resample(pop_raster, template, method = "sum")

  earth_r   <- 6371.0088
  lat_rad   <- lat_centres * pi / 180
  cell_area <- (LON_STEP * pi / 180) * (-LAT_STEP * pi / 180) *
               earth_r^2 * cos(lat_rad)
  cell_area_mat <- matrix(cell_area, nrow = GRID_NX, ncol = GRID_NY, byrow = TRUE)

  pop_mat <- t(matrix(as.vector(pop_sum), nrow = GRID_NY, ncol = GRID_NX,
                      byrow = TRUE))
  stopifnot(dim(pop_mat) == c(GRID_NX, GRID_NY))

  pop_mat[is.na(pop_mat)] <- 0

  pop_density <- pop_mat / cell_area_mat
  log_pop     <- log1p(pop_density)

  message(sprintf("  aggregated pop density: mean %.1f /km², max %.1f /km², cells = 0: %d",
                  mean(pop_density), max(pop_density),
                  sum(pop_density == 0)))

  stopifnot(
    "Population density outside plausible bounds" =
      all(pop_density >= 0 & pop_density <= 30000)
  )

  log_pop_min <- min(log_pop); log_pop_max <- max(log_pop)
  log_pop_scaled <- (log_pop - log_pop_min) / (log_pop_max - log_pop_min)

  out <- list(
    elevation        = elevation_scaled,
    log_pop_density  = log_pop_scaled,
    meta = list(
      grid_dim          = c(nx = GRID_NX, ny = GRID_NY),
      elev_min_m        = elev_min,
      elev_max_m        = elev_max,
      log_pop_min       = log_pop_min,
      log_pop_max       = log_pop_max,
      pop_density_max   = max(pop_density),
      built_at          = Sys.time(),
      elevation_source  = "AWS Terrain Tiles via elevatr (src='aws'; composite of 3DEP, SRTM, GMTED2010, ETOPO1), z=6",
      population_source = "WorldPop Global 2000-2020 Poland 2018 top-down unconstrained (~100 m / 3 arc-second native)"
    )
  )

  proc_dir <- here::here("data", "processed")
  if (!dir.exists(proc_dir)) dir.create(proc_dir, recursive = TRUE)
  saveRDS(out, SPATIAL_OUT)

  message(sprintf("Saved %s (%.1f KB)", SPATIAL_OUT,
                  file.info(SPATIAL_OUT)$size / 1024))
}

message("Data acquisition complete.")
