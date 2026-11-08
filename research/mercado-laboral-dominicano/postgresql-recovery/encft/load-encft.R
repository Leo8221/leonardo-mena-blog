#!/usr/bin/env Rscript
# Additive, resumable import. Originals are never changed; no DROP/TRUNCATE.
stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(arrow); library(DBI); library(RPostgres); library(data.table); library(digest); library(jsonlite)})
options(warn=1)
args <- commandArgs(trailingOnly=TRUE)
base <- "research/mercado-laboral-dominicano/postgresql-recovery/encft"
for(script in list.files(base,pattern="[.]R$",full.names=TRUE)) invisible(parse(script,encoding="UTF-8"))
qa <- file.path(base,"qa")
source_root <- Sys.getenv("ENCFT_SOURCE_ROOT","C:/Users/leona/Desktop/encuesta_trabajo_rd")
pg_host <- Sys.getenv("PGHOST","127.0.0.1")
pg_port <- as.integer(Sys.getenv("PGPORT","5433"))
pg_user <- Sys.getenv("PGUSER","postgres")
pg_db <- Sys.getenv("ENCFT_DATABASE","encft")
password <- Sys.getenv("PG_RECOVERY_PASSWORD")
modules <- data.table(module=c("viviendas","hogares","miembros","diccionario","tasas_monedas"),
                     schema=c(rep("microdatos",3),rep("meta",2)),
                     file=c("ENCFT_Vivienda_2016_2026.parquet","ENCFT_Hogar_2016_2026.parquet","ENCFT_Miembros_2016_2026.parquet","ENCFT_Diccionario_2016_2026.parquet","tasas-monedas-completas.parquet"),
                     expected=c(258607L,258764L,790993L,115178L,399L))
modules[,path:=ifelse(module=="tasas_monedas",file.path(qa,file),file.path(source_root,"procesado",file))]
integer_fields <- tolower(c("ANIO_ARCHIVO","TRIMESTRE","PERIODO","ANO","MES","ORDEN_REGION","ESTRATO","UPM","VIVIENDA","HOGAR","MIEMBRO","ID_PROVINCIA","ID_MUNICIPIO","ZONA","SEXO","EDAD","PARENTESCO","PET","PET10","OCUPADO","SUBOCUPADO","PEA","DESOCUPADO_INI","DESOCUPADO","CESANTE","NUEVO","AMPLIADO","INACTIVO"))
preflight <- function(path) {
  r <- ParquetFileReader$create(path)
  s <- r$GetSchema()
  original <- s$names
  normalized <- tolower(original)
  stopifnot(!anyDuplicated(normalized),all(grepl("^[a-z_][a-z0-9_]*$",normalized)),all(nchar(normalized,type="bytes")<=63L))
  arrow_types <- vapply(seq_along(original),function(i) s$GetFieldByName(original[[i]])$type$ToString(),character(1))
  pg_types <- ifelse(arrow_types=="string","text",ifelse(arrow_types=="bool","boolean",ifelse(arrow_types=="int32","integer",ifelse(arrow_types=="double","double precision",NA_character_))))
  stopifnot(!anyNA(pg_types))
  pg_types[normalized %in% integer_fields] <- "integer"
  list(rows=r$num_rows,names=normalized,original=original,arrow_types=arrow_types,pg_types=pg_types,sha256=digest(file=path,algo="sha256"))
}
profiles <- lapply(modules$path,preflight)
stopifnot(all(vapply(profiles,`[[`,numeric(1),"rows")==modules$expected))
# Check the scanner API before a database can be created.
probe <- Scanner$create(open_dataset(modules$path[[1]]),batch_size=20000L,use_threads=FALSE)$ToRecordBatchReader()
probe_batch <- probe$read_next_batch()
stopifnot(probe_batch$num_rows>0L,probe_batch$num_rows<=20000L)
rm(probe,probe_batch)
cat("Preflight passed: five modules, preserved source types and bounded batches.\n")
if(!"--load" %in% args) quit(status=0)
stopifnot(nzchar(password),grepl("^[a-z][a-z0-9_]*$",pg_db))
connect <- function(db) dbConnect(Postgres(),host=pg_host,port=pg_port,user=pg_user,password=password,dbname=db,options="-c application_name=encft_import -c lock_timeout=5000")
admin <- connect("postgres")
exists <- nrow(dbGetQuery(admin,"SELECT datname FROM pg_database WHERE datname=$1",params=list(pg_db)))>0
if(!exists) {
  dbExecute(admin,paste("CREATE DATABASE",dbQuoteIdentifier(admin,pg_db),"TEMPLATE template0 ENCODING 'UTF8'"))
  cat("Created database",pg_db,"\n")
}
dbDisconnect(admin)
con <- connect(pg_db)
on.exit(dbDisconnect(con),add=TRUE)
existing <- dbGetQuery(con,"SELECT n.nspname,c.relname FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace WHERE n.nspname NOT LIKE 'pg_%' AND n.nspname<>'information_schema' AND c.relkind IN ('r','v','m','f')")
if(nrow(existing)>0 && !dbExistsTable(con,Id(schema="meta",table="ingestas"))) stop("Database is not an ENCFT import owned by this loader; refusing to change it.")
for(schema in c("microdatos","meta","analitica")) dbExecute(con,paste("CREATE SCHEMA IF NOT EXISTS",schema))
dbExecute(con,"CREATE TABLE IF NOT EXISTS meta.ingestas (modulo text PRIMARY KEY, ruta text NOT NULL, sha256 text NOT NULL, filas bigint NOT NULL, columnas integer NOT NULL, completado_en timestamptz NOT NULL DEFAULT now())")
dbExecute(con,"CREATE TABLE IF NOT EXISTS meta.columnas (modulo text NOT NULL, posicion integer NOT NULL, nombre_original text NOT NULL, nombre_sql text NOT NULL, tipo_origen text NOT NULL, tipo_sql text NOT NULL, PRIMARY KEY(modulo,posicion))")
for(i in seq_len(nrow(modules))) {
  mod <- modules[i]
  pr <- profiles[[i]]
  id <- Id(schema=mod$schema,table=mod$module)
  target <- as.character(dbQuoteIdentifier(con,id))
  if(dbExistsTable(con,id)) {
    saved <- dbGetQuery(con,"SELECT sha256,filas FROM meta.ingestas WHERE modulo=$1",params=list(mod$module))
    actual <- dbGetQuery(con,paste("SELECT count(*) AS n FROM",target))$n
    if(nrow(saved)!=1L || saved$sha256!=pr$sha256 || as.numeric(actual)!=pr$rows) stop("Existing module differs from verified import; refusing to replace it: ",mod$module)
    cat("Verified existing module:",mod$module,pr$rows,"rows\n")
    next
  }
  cat("Loading",mod$module,"—",pr$rows,"rows,",length(pr$names),"source columns\n")
  dbWithTransaction(con,{
    definitions <- paste(as.character(dbQuoteIdentifier(con,pr$names)),pr$pg_types)
    dbExecute(con,paste0("CREATE TABLE ",target," (fila_origen bigint NOT NULL, ",paste(definitions,collapse=", "),")"))
    reader <- Scanner$create(open_dataset(mod$path),batch_size=20000L,use_threads=FALSE)$ToRecordBatchReader()
    loaded <- 0L
    repeat {
      batch <- reader$read_next_batch()
      if(is.null(batch)) break
      df <- as.data.frame(batch)
      names(df) <- pr$names
      for(nm in names(df)[pr$pg_types=="integer"]) {
        x <- df[[nm]]
        stopifnot(all(is.na(x)|(is.finite(x)&x==trunc(x)&abs(x)<=.Machine$integer.max)))
        df[[nm]] <- as.integer(x)
      }
      df <- cbind(data.frame(fila_origen=seq.int(loaded+1L,loaded+nrow(df))),df)
      inserted <- dbAppendTable(con,id,df)
      stopifnot(as.numeric(inserted)==nrow(df))
      loaded <- loaded+nrow(df)
      if(loaded %% 100000L==0L || loaded==pr$rows) cat(" ",mod$module,loaded,"/",pr$rows,"\n")
      rm(df,batch)
    }
    stopifnot(loaded==pr$rows)
    dbExecute(con,paste0("ALTER TABLE ",target," ADD PRIMARY KEY (fila_origen)"))
    dbExecute(con,"INSERT INTO meta.ingestas(modulo,ruta,sha256,filas,columnas) VALUES($1,$2,$3,$4,$5)",params=list(mod$module,normalizePath(mod$path,winslash="/"),pr$sha256,loaded,length(pr$names)))
    cols <- data.frame(modulo=mod$module,posicion=seq_along(pr$names),nombre_original=pr$original,nombre_sql=pr$names,tipo_origen=pr$arrow_types,tipo_sql=pr$pg_types)
    dbAppendTable(con,Id(schema="meta",table="columnas"),cols)
  })
  dbExecute(con,paste("ANALYZE",target))
}
manifest <- as.data.frame(fread(file.path(qa,"source-manifest.csv")))
if(!dbExistsTable(con,Id(schema="meta",table="archivos"))) dbWriteTable(con,Id(schema="meta",table="archivos"),manifest,row.names=FALSE)
for(sql_file in c("create-analytics.sql")) {
  sql <- paste(readLines(file.path(base,sql_file),encoding="UTF-8",warn=FALSE),collapse="\n")
  # Each statement is delimited by an explicit standalone marker, not semicolons in strings.
  for(stmt in strsplit(sql,"\n-- statement\n",fixed=TRUE)[[1]]) if(nzchar(trimws(stmt))) dbExecute(con,stmt)
}
cat("ENCFT modules and analytical views are loaded. Run verify-encft.R before central registration.\n")
dbDisconnect(con)
