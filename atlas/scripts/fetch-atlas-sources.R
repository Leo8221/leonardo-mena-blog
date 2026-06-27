#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jsonlite)
})

options(encoding = "UTF-8")

root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
atlas_dir <- file.path(root, "atlas")
manifest_path <- file.path(atlas_dir, "data", "source-manifest.json")
raw_dir <- file.path(atlas_dir, "data", "raw")
run_path <- file.path(atlas_dir, "data", "source-run.json")

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) y else x
}

if (!file.exists(manifest_path)) {
  stop("No existe el manifiesto de fuentes: ", manifest_path, call. = FALSE)
}

dir.create(raw_dir, recursive = TRUE, showWarnings = FALSE)

manifest <- fromJSON(manifest_path, simplifyVector = FALSE)
sources <- manifest$sources %||% list()
if (!length(sources)) {
  stop("source-manifest.json no contiene fuentes.", call. = FALSE)
}

process_source <- function(source) {
  id <- source$id %||% ""
  if (!nzchar(id)) stop("Una fuente no tiene id.", call. = FALSE)

  mode <- source$mode %||% "local"
  kind <- source$kind %||% "file"
  required <- isTRUE(source$required)
  cache <- isTRUE(source$cache)
  target_dir <- file.path(raw_dir, id)
  dir.create(target_dir, recursive = TRUE, showWarnings = FALSE)

  base <- source_result(source, status = "pending", target_dir = rel_path(target_dir))

  if (mode == "manual") {
    return(modifyList(base, list(status = "manual", note = source$updatePolicy %||% "Actualizacion manual pendiente.")))
  }

  if (mode == "url") {
    return(fetch_url_source(source, base, target_dir))
  }

  if (mode == "bcrd_custom_view") {
    return(fetch_bcrd_custom_view_source(source, base, target_dir))
  }

  if (mode == "local") {
    return(fetch_local_source(source, base, target_dir, kind, cache, required))
  }

  modifyList(base, list(status = "missing", error = paste("Modo no soportado:", mode)))
}

fetch_url_source <- function(source, base, target_dir) {
  url <- source$url %||% ""
  if (!nzchar(url)) {
    return(modifyList(base, list(status = "missing", error = "Fuente URL sin url.")))
  }
  filename <- source$filename %||% basename(strsplit(url, "\\?")[[1]][1])
  if (!nzchar(filename)) filename <- paste0(source$id, ".dat")
  target <- file.path(target_dir, filename)

  ok <- tryCatch({
    download.file(url, target, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(error) {
    conditionMessage(error)
  })

  if (!isTRUE(ok)) {
    return(modifyList(base, list(status = "download_failed", url = url, error = as.character(ok))))
  }

  enrich_file_result(base, target, "downloaded", url = url)
}

fetch_bcrd_custom_view_source <- function(source, base, target_dir) {
  url <- source$url %||% source$customViewUrl %||% ""
  if (!nzchar(url)) {
    return(modifyList(base, list(status = "missing", error = "Fuente BCRD sin url.")))
  }

  html <- tryCatch(
    paste(readLines(url, warn = FALSE, encoding = "UTF-8"), collapse = "\n"),
    error = function(error) error
  )
  if (inherits(html, "error")) {
    return(modifyList(base, list(status = "download_failed", url = url, error = conditionMessage(html))))
  }

  links <- extract_excel_links(html, url)
  include_patterns <- source$includePatterns %||% list()
  selected <- links[vapply(links, function(link) {
    if (!length(include_patterns)) return(TRUE)
    any(vapply(include_patterns, function(pattern) {
      grepl(pattern, basename_no_query(link), ignore.case = TRUE)
    }, logical(1)))
  }, logical(1))]

  if (!length(selected)) {
    return(modifyList(base, list(
      status = "missing",
      url = url,
      discovered = length(links),
      error = "No se encontraron archivos Excel que coincidan con includePatterns."
    )))
  }

  max_files <- source$maxFiles %||% length(selected)
  selected <- selected[seq_len(min(length(selected), max_files))]
  file_results <- lapply(selected, function(link) download_bcrd_file(link, target_dir))
  ok <- vapply(file_results, function(item) identical(item$status, "downloaded"), logical(1))

  write_json(
    list(source = source$id, url = url, discovered = links, selected = selected, files = file_results),
    file.path(target_dir, "links.json"),
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null"
  )

  status <- if (any(ok)) "downloaded" else "download_failed"
  modifyList(base, list(
    status = status,
    url = url,
    discovered = length(links),
    selected = length(selected),
    downloaded = sum(ok),
    bytes = sum(vapply(file_results[ok], function(item) item$bytes %||% 0, numeric(1))),
    files = file_results
  ))
}

fetch_local_source <- function(source, base, target_dir, kind, cache, required) {
  source_path <- resolve_source_path(source)
  if (!nzchar(source_path) || !file.exists(source_path)) {
    status <- if (required) "missing" else "verified"
    return(modifyList(base, list(status = status, originalPath = source$path %||% "", error = "No existe la ruta local.")))
  }

  if (kind == "directory") {
    return(modifyList(base, list(
      status = "verified",
      originalPath = rel_path(source_path),
      modifiedAt = file_time(source_path)
    )))
  }

  if (!cache) {
    return(enrich_file_result(modifyList(base, list(originalPath = rel_path(source_path))), source_path, "verified"))
  }

  target <- file.path(target_dir, basename(source_path))
  ok <- tryCatch({
    file.copy(source_path, target, overwrite = TRUE)
  }, warning = function(warning) {
    FALSE
  }, error = function(error) {
    FALSE
  })

  if (!isTRUE(ok)) {
    return(modifyList(base, list(status = "copy_failed", originalPath = rel_path(source_path), error = "No se pudo copiar al cache raw.")))
  }

  enrich_file_result(modifyList(base, list(originalPath = rel_path(source_path))), target, "cached")
}

extract_excel_links <- function(html, page_url) {
  matches <- gregexpr("href\\s*=\\s*['\\\"]([^'\\\"]+[.]xlsx?(?:[?][^'\\\"]*)?)['\\\"]", html, ignore.case = TRUE, perl = TRUE)
  raw <- regmatches(html, matches)[[1]]
  if (!length(raw)) return(character())
  hrefs <- sub("^href\\s*=\\s*['\\\"]", "", raw, ignore.case = TRUE)
  hrefs <- sub("['\\\"]$", "", hrefs)
  hrefs <- gsub("&amp;", "&", hrefs, fixed = TRUE)
  unique(vapply(hrefs, absolute_url, character(1), page_url = page_url))
}

absolute_url <- function(href, page_url) {
  if (grepl("^https?://", href, ignore.case = TRUE)) return(href)
  if (startsWith(href, "//")) return(paste0("https:", href))
  origin <- sub("^(https?://[^/]+).*$", "\\1", page_url)
  if (startsWith(href, "/")) return(paste0(origin, href))
  base <- sub("/[^/]*$", "/", page_url)
  paste0(base, href)
}

download_bcrd_file <- function(url, target_dir) {
  filename <- basename_no_query(url)
  target <- file.path(target_dir, filename)
  ok <- tryCatch({
    download.file(url, target, mode = "wb", quiet = TRUE)
    TRUE
  }, error = function(error) {
    conditionMessage(error)
  })

  if (!isTRUE(ok)) {
    return(list(status = "download_failed", url = url, filename = filename, error = as.character(ok)))
  }

  info <- file.info(target)
  list(
    status = "downloaded",
    url = url,
    filename = filename,
    rawPath = rel_path(target),
    bytes = unname(info$size),
    modifiedAt = file_time(target),
    md5 = unname(tools::md5sum(target))
  )
}

basename_no_query <- function(url) {
  basename(strsplit(url, "\\?")[[1]][1])
}

source_result <- function(source, status, target_dir) {
  list(
    id = source$id,
    label = source$label %||% source$id,
    mode = source$mode %||% "local",
    kind = source$kind %||% "file",
    required = isTRUE(source$required),
    status = status,
    targetDir = target_dir,
    usedBy = source$usedBy %||% list(),
    updatePolicy = source$updatePolicy %||% ""
  )
}

enrich_file_result <- function(base, path, status, url = NULL) {
  info <- file.info(path)
  extra <- list(
    status = status,
    rawPath = rel_path(path),
    bytes = unname(info$size),
    modifiedAt = file_time(path),
    md5 = unname(tools::md5sum(path))
  )
  if (!is.null(url)) extra$url <- url
  modifyList(base, extra)
}

resolve_path <- function(path) {
  if (!nzchar(path)) return("")
  candidate <- if (grepl("^[A-Za-z]:|^/", path)) path else file.path(root, path)
  normalizePath(candidate, winslash = "/", mustWork = FALSE)
}

resolve_source_path <- function(source) {
  base_path <- resolve_path(source$path %||% "")
  pattern <- source$pattern %||% ""
  if (!nzchar(pattern)) return(base_path)
  if (!dir.exists(base_path)) return("")
  matches <- list.files(base_path, pattern = pattern, full.names = TRUE, ignore.case = TRUE)
  if (!length(matches)) return("")
  normalizePath(matches[[1]], winslash = "/", mustWork = FALSE)
}

rel_path <- function(path) {
  normalized <- normalizePath(path, winslash = "/", mustWork = FALSE)
  sub(paste0("^", gsub("([\\^$.|?*+(){}\\[\\]\\\\])", "\\\\\\1", root), "/?"), "", normalized)
}

file_time <- function(path) {
  info <- file.info(path)
  if (is.na(info$mtime)) return(NULL)
  format(as.POSIXct(info$mtime, tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
}

results <- lapply(sources, process_source)
failed_required <- vapply(results, function(item) {
  isTRUE(item$required) && item$status %in% c("missing", "download_failed", "copy_failed")
}, logical(1))

summary <- list(
  total = length(results),
  downloaded = sum(vapply(results, function(item) item$status == "downloaded", logical(1))),
  cached = sum(vapply(results, function(item) item$status == "cached", logical(1))),
  verified = sum(vapply(results, function(item) item$status == "verified", logical(1))),
  manual = sum(vapply(results, function(item) item$status == "manual", logical(1))),
  failed = sum(vapply(results, function(item) item$status %in% c("missing", "download_failed", "copy_failed"), logical(1)))
)

run <- list(
  generatedAt = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
  manifestVersion = manifest$version %||% NA_integer_,
  summary = summary,
  sources = results
)

write_json(run, run_path, pretty = TRUE, auto_unbox = TRUE, null = "null")

if (any(failed_required)) {
  failed_ids <- vapply(results[failed_required], function(item) item$id, character(1))
  stop("Fallaron fuentes obligatorias: ", paste(failed_ids, collapse = ", "), call. = FALSE)
}

message("Atlas sources checked: ", summary$total, " total, ",
        summary$downloaded, " downloaded, ",
        summary$cached, " cached, ",
        summary$verified, " verified, ",
        summary$manual, " manual.")
