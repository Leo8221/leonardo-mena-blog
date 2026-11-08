#!/usr/bin/env Rscript
# Prepare metadata and recover only the small currency sheets missing from Parquet.
stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(arrow); library(data.table); library(jsonlite); library(readxl); library(digest)})
args <- commandArgs(trailingOnly=TRUE)
root <- if(length(args)) args[[1]] else Sys.getenv("ENCFT_SOURCE_ROOT","C:/Users/leona/Desktop/encuesta_trabajo_rd")
base <- "research/mercado-laboral-dominicano/postgresql-recovery/encft"
out <- file.path(base,"qa")
dir.create(out,recursive=TRUE,showWarnings=FALSE)
modules <- c(Miembros="miembros",Hogar="hogares",Vivienda="viviendas",Diccionario="diccionario",Tasa_Monedas="tasas_monedas")
rates_path <- file.path(root,"procesado","ENCFT_Tasa_Monedas_2016_2026.parquet")
rates <- as.data.table(read_parquet(rates_path))
rate_sources <- list()
for(year in 2016:2026) {
  if(year %in% rates$ANIO_ARCHIVO) next
  book <- list.files(file.path(root,year),pattern="[.]xlsx$",full.names=TRUE)
  stopifnot(length(book)==1L)
  sheets <- excel_sheets(book)
  sheet <- sheets[tolower(trimws(sheets))=="tasa monedas"]
  stopifnot(length(sheet)==1L)
  dt <- as.data.table(read_excel(book,sheet=sheet,.name_repair="check_unique"))
  setnames(dt,toupper(names(dt)))
  stopifnot(setequal(names(dt),setdiff(names(rates),c("ANIO_ARCHIVO","ARCHIVO_ORIGEN"))))
  dt[,`:=`(ANIO_ARCHIVO=as.integer(year),ARCHIVO_ORIGEN=basename(book))]
  rates <- rbindlist(list(rates,dt),use.names=TRUE)
  rate_sources[[length(rate_sources)+1L]] <- data.table(module="tasas_monedas",kind="excel_recuperado",path=normalizePath(book,winslash="/"),sheet=sheet,rows=nrow(dt))
}
setorder(rates,PERIODO,MONEDA)
stopifnot(nrow(rates)==399L, !anyDuplicated(rates[,.(PERIODO,MONEDA)]))
write_parquet(rates,file.path(out,"tasas-monedas-completas.parquet"))
manifest <- rbindlist(lapply(names(modules),function(mod) {
  p <- file.path(root,"procesado",paste0("ENCFT_",mod,"_2016_2026.parquet"))
  reader <- ParquetFileReader$create(p)
  data.table(module=unname(modules[[mod]]),kind="parquet_procesado",path=normalizePath(p,winslash="/"),sheet=NA_character_,rows=reader$num_rows)
}))
manifest <- rbindlist(c(list(manifest),rate_sources),fill=TRUE)
manifest[,`:=`(bytes=file.info(path)$size,sha256=vapply(path,function(p) digest(file=p,algo="sha256"),character(1)),source_modified=format(file.info(path)$mtime,"%Y-%m-%dT%H:%M:%S%z"))]
fwrite(manifest,file.path(out,"source-manifest.csv"))
inventory <- fread(file.path(out,"yearly-parquet-inventory.csv"))
excel <- as.data.table(fromJSON(file.path(out,"xlsx-inventory.json")))
excel[,`:=`(year=as.integer(basename(dirname(file))),module=fifelse(tolower(sheet)=="tasa monedas","Tasa_Monedas",sheet),excel_rows=as.integer(sub(".*[A-Z]+([0-9]+)$","\\1",dimension))-1L)]
recon <- merge(inventory,excel[,.(module,year,excel_rows)],by=c("module","year"),all=TRUE)
recon[,status:=fifelse(is.na(rows)&module=="Tasa_Monedas","recovered_from_excel",fifelse(rows==excel_rows,"ok","review"))]
stopifnot(!any(recon$status=="review",na.rm=TRUE),!anyNA(recon$status))
fwrite(recon,file.path(out,"source-reconciliation.csv"))
cat("Prepared",nrow(rates),"currency rows and reconciled",nrow(recon),"module-year combinations.\n")
print(manifest[,.(module,kind,rows,bytes)])
