stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(readxl);library(data.table);library(jsonlite);library(DBI);library(RPostgres)})
base <- "research/mercado-laboral-dominicano/postgresql-recovery/expansion"
dir.create(file.path(base,"qa"),recursive=TRUE,showWarnings=FALSE)
paths <- c(list.files("atlas/data/raw",pattern="[.]xlsx?$",recursive=TRUE,full.names=TRUE),
 "posts/republica-en-un-grafico/2026-01-31-efectivo-en-la-republica-dominicana/ENIEF_Consolidado.xlsx",
 "research/mercado-laboral-dominicano/data/raw/enae_2024/base_de_datos_general_ENAE_2024.xlsx")
profiles <- list()
preview <- list()
problems <- list()
for(path in paths) {
  sheets <- excel_sheets(path)
  for(sheet in sheets) {
    x <- tryCatch(suppressWarnings(read_excel(path,sheet=sheet,col_names=FALSE,col_types="text",n_max=20,.name_repair="minimal")),error=function(e)e)
    if(inherits(x,"error")) {
      problems[[length(problems)+1L]] <- list(path=path,sheet=sheet,error=conditionMessage(x))
      next
    }
    key <- paste(basename(path),sheet,sep=" :: ")
    profiles[[length(profiles)+1L]] <- data.table(path=path,sheet=sheet,preview_rows=nrow(x),columns=ncol(x))
    if(!grepl("MIPYMES|ENIEF|ENAE",path,ignore.case=TRUE)) preview[[key]] <- as.matrix(x)
    else {
      tab <- suppressWarnings(read_excel(path,sheet=sheet,guess_max=100000,.name_repair="unique"))
      preview[[key]] <- list(rows=nrow(tab),columns=ncol(tab),names=names(tab),types=vapply(tab,function(z)paste(class(z),collapse="/"),character(1)))
    }
  }
}
fwrite(rbindlist(profiles),file.path(base,"qa","workbooks.csv"))
write_json(preview,file.path(base,"qa","workbook-previews.json"),pretty=TRUE,auto_unbox=TRUE,na="null")
write_json(problems,file.path(base,"qa","workbook-errors.json"),pretty=TRUE,auto_unbox=TRUE)
cat("Profiled",length(paths),"workbooks and",length(profiles),"sheets.\n")
if(nzchar(Sys.getenv("PG_RECOVERY_PASSWORD"))) {
  con <- dbConnect(Postgres(),host="127.0.0.1",port=5433,user="postgres",password=Sys.getenv("PG_RECOVERY_PASSWORD"),dbname="postgres",options="-c default_transaction_read_only=on")
  print(dbGetQuery(con,"SELECT datname,pg_size_pretty(pg_database_size(oid)) AS size FROM pg_database WHERE NOT datistemplate ORDER BY datname"))
  dbDisconnect(con)
}
