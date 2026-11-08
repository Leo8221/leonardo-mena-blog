#!/usr/bin/env Rscript
# Read-only inspection of the user's processed ENCFT files.
stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(arrow); library(jsonlite); library(data.table)})
args <- commandArgs(trailingOnly = TRUE)
root <- if (length(args)) args[[1]] else Sys.getenv("ENCFT_SOURCE_ROOT", "C:/Users/leona/Desktop/encuesta_trabajo_rd")
out <- "research/mercado-laboral-dominicano/postgresql-recovery/encft/qa"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
files <- list.files(file.path(root, "procesado"), pattern = "^ENCFT_.*[.]parquet$", full.names = TRUE)
profiles <- lapply(files, function(path) {
  reader <- ParquetFileReader$create(path)
  schema <- reader$GetSchema()
  cols <- schema$names
  selected <- cols[grepl("anio|año|year|trimestre|periodo|factor|peso|expand|ponder|id_|^id$|folio|hogar|vivienda|panel|rotaci|^mes$|ocup|desoc|inact|condici|edad|sexo", cols, ignore.case = TRUE)]
  cat("\nFILE", basename(path), "ROWS", reader$num_rows, "COLS", length(cols), "\n")
  cat("NAMES", paste(cols, collapse = " | "), "\n")
  if (grepl("Diccionario|Tasa_Monedas", basename(path))) {
    dt <- as.data.table(read_parquet(path))
    fwrite(dt, file.path(out, sub("parquet$", "csv", basename(path))))
    print(head(dt, 10))
  } else {
    dt <- as.data.table(read_parquet(path, col_select = tidyselect::all_of(selected)))
    for (nm in selected) {
      x <- dt[[nm]]
      cat(nm, ": null=", sum(is.na(x)), " distinct=", uniqueN(x), sep = "")
      if (uniqueN(x) <= 48) cat(" values=", paste(head(sort(unique(x)), 48), collapse = ","), sep = "")
      if (is.numeric(x)) cat(" range=", paste(range(x, na.rm = TRUE), collapse = ".."), sep = "")
      cat("\n")
    }
  }
  list(file = normalizePath(path, winslash = "/"), bytes = file.info(path)$size,
       rows = reader$num_rows, columns = length(cols), names = cols, schema = schema$ToString())
})
write_json(profiles, file.path(out, "source-profile.json"), auto_unbox = TRUE, pretty = TRUE, na = "null")
