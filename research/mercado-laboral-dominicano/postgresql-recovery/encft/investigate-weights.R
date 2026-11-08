#!/usr/bin/env Rscript
stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(arrow);library(data.table)})
root <- Sys.getenv("ENCFT_SOURCE_ROOT","C:/Users/leona/Desktop/encuesta_trabajo_rd")
qa <- "research/mercado-laboral-dominicano/postgresql-recovery/encft/qa"
keys <- c("PERIODO","UPM","VIVIENDA","HOGAR")
attrs <- c("FACTOR_EXPANSION","ID_HOGAR","ID_PROVINCIA","ID_MUNICIPIO","CANTIDAD_MIEMBROS_HOGAR")
read <- function(module) as.data.table(read_parquet(file.path(root,"procesado",paste0("ENCFT_",module,"_2016_2026.parquet")),col_select=tidyselect::all_of(c(keys,attrs))))
m <- read("Miembros"); h <- read("Hogar")
j <- merge(m,h,by=keys,suffixes=c("_miembro","_hogar"),all.x=TRUE)
stopifnot(nrow(j)==nrow(m))
j[,delta:=FACTOR_EXPANSION_miembro-FACTOR_EXPANSION_hogar]
summary <- j[abs(delta)>1e-8,.(registros=.N,hogares=uniqueN(.SD),diferencia_min=min(delta),diferencia_max=max(delta),diferencia_absoluta_max=max(abs(delta)),diferencia_relativa_max=max(abs(delta)/FACTOR_EXPANSION_hogar),ids_distintos=sum(ID_HOGAR_miembro!=ID_HOGAR_hogar),provincia_distinta=sum(ID_PROVINCIA_miembro!=ID_PROVINCIA_hogar),municipio_distinto=sum(ID_MUNICIPIO_miembro!=ID_MUNICIPIO_hogar),tamano_hogar_distinto=sum(CANTIDAD_MIEMBROS_HOGAR_miembro!=CANTIDAD_MIEMBROS_HOGAR_hogar)),by=PERIODO,.SDcols=keys[-1]]
print(summary)
fwrite(summary,file.path(qa,"weight-differences-source.csv"))
cat("All linked rows: province differences",j[ID_PROVINCIA_miembro!=ID_PROVINCIA_hogar,.N],"municipality differences",j[ID_MUNICIPIO_miembro!=ID_MUNICIPIO_hogar,.N],"household-size differences",j[CANTIDAD_MIEMBROS_HOGAR_miembro!=CANTIDAD_MIEMBROS_HOGAR_hogar,.N],"\n")
# Select numeric cells for an independent spot check in the original 2023 Excel.
a <- as.data.table(read_parquet(file.path(root,"procesado/parquet_por_anio/Miembros/2023.parquet"),col_select=tidyselect::all_of(c(keys,"FACTOR_EXPANSION")))); a[,excel_row:=.I+1L]
b <- as.data.table(read_parquet(file.path(root,"procesado/parquet_por_anio/Hogar/2023.parquet"),col_select=tidyselect::all_of(c(keys,"FACTOR_EXPANSION")))); b[,excel_row:=.I+1L]
pairs <- merge(a,b,by=keys,suffixes=c("_m","_h"))[abs(FACTOR_EXPANSION_m-FACTOR_EXPANSION_h)>1e-8]
pairs[,diff_abs:=abs(FACTOR_EXPANSION_m-FACTOR_EXPANSION_h)]
selected <- unique(rbind(pairs[1:2],pairs[which.max(diff_abs)]))
cells <- rbindlist(lapply(seq_len(nrow(selected)),function(i) rbindlist(lapply(c("m","h"),function(s) {
  row <- selected[[paste0("excel_row_",s)]][[i]]
  data.table(sheet=if(s=="m") "Miembros" else "Hogar",cell=paste0(c("B","H","K","L","M"),row),expected=c(selected$PERIODO[[i]],selected$UPM[[i]],selected$VIVIENDA[[i]],selected$HOGAR[[i]],selected[[paste0("FACTOR_EXPANSION_",s)]][[i]]))
}))))
jsonlite::write_json(cells,file.path(qa,"source-spotcheck-cells.json"),dataframe="rows",auto_unbox=TRUE,pretty=TRUE,digits=16)
