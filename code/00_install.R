packages <- c(
  "ecmwfr",
  "dotenv",

  "stars",
  "sf",
  "spdep",
  "terra",
  "elevatr",
  "rnaturalearth",
  "rnaturalearthdata",

  "torch",
  "luz",

  "tidyverse",
  "here",
  "cowplot",
  "patchwork",
  "plotly",
  "quarto",
  "rmarkdown"
)

installed <- rownames(installed.packages())
to_install <- setdiff(packages, installed)

if (length(to_install) > 0) {
  message(sprintf("Installing %d packages: %s", length(to_install),
                  paste(to_install, collapse = ", ")))
  install.packages(to_install, repos = "https://cloud.r-project.org")
} else {
  message("All packages already installed.")
}

if (requireNamespace("torch", quietly = TRUE)) {
  if (!torch::torch_is_installed()) {
    message("Installing LibTorch backend...")
    torch::install_torch()
  } else {
    message("LibTorch backend already installed.")
  }
}

message("Done. You can now run scripts 01 through 12.")
