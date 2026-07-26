library(knitr)

preview_dir <- file.path(getwd(), ".tmp-videojuegos-preview")
dir.create(preview_dir, recursive = TRUE, showWarnings = FALSE)

knitr::opts_chunk$set(
  fig.path = file.path(preview_dir, "qmd-"),
  dev = "png",
  dpi = 120
)

knitr::knit(
  input = file.path("posts", "fundamentos", "2026-07-25-videojuegos-y-economia", "index.qmd"),
  output = file.path(preview_dir, "videojuegos.qmd.md"),
  quiet = TRUE
)

message("QMD_VALIDATION_OK")
