options(repos = c(CRAN = "https://cloud.r-project.org"))

packages <- c(
  "dplyr", "ggplot2", "readr", "readxl", "scales", "stringr", "tidyr",
  "svglite", "pdftools", "sf", "patchwork", "ragg", "here", "tidyverse"
)

library_path <- Sys.getenv("R_LIBS_USER")
if (!nzchar(library_path)) stop("R_LIBS_USER no esta definido.", call. = FALSE)
dir.create(library_path, recursive = TRUE, showWarnings = FALSE)
.libPaths(unique(c(library_path, .libPaths())))
cat("R_LIBS_USER=", library_path, "\n", sep = "")
cat("LIB_PATHS=", paste(.libPaths(), collapse = "|"), "\n", sep = "")

installed <- rownames(installed.packages(lib.loc = library_path))
missing <- setdiff(packages, installed)
if (length(missing)) {
  install.packages(missing, lib = library_path, dependencies = TRUE)
}

missing_after <- setdiff(packages, rownames(installed.packages(lib.loc = library_path)))
if (length(missing_after)) stop(paste("No se instalaron:", paste(missing_after, collapse = ", ")))
cat("R_PACKAGES_OK\n")
