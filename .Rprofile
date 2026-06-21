if (.Platform$OS.type == "windows") {
  local({
    target_locale <- "Spanish_Dominican Republic.utf8"
    for (category in c("LC_CTYPE", "LC_COLLATE", "LC_MONETARY", "LC_TIME")) {
      try(Sys.setlocale(category, target_locale), silent = TRUE)
    }
  })
}

options(encoding = "UTF-8")
