#!/usr/bin/env Rscript
# Register only the new ENCFT source. Existing census/ENHOGAR objects are untouched.
stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(DBI);library(RPostgres);library(jsonlite)})
base <- "research/mercado-laboral-dominicano/postgresql-recovery/encft"
validation <- fromJSON(file.path(base,"qa","validation-summary.json"))
stopifnot(validation$quarters==41L)
password <- Sys.getenv("PG_RECOVERY_PASSWORD")
stopifnot(nzchar(password))
pg_host <- Sys.getenv("PGHOST","127.0.0.1")
pg_port <- Sys.getenv("PGPORT","5433")
pg_user <- Sys.getenv("PGUSER","postgres")
pg_db <- Sys.getenv("ENCFT_DATABASE","encft")
stopifnot(validation$database==pg_db)
con <- dbConnect(Postgres(),host=pg_host,port=as.integer(pg_port),user=pg_user,password=password,dbname="censos_linea_tiempo",options="-c application_name=encft_register -c lock_timeout=5000")
q <- function(s) as.character(dbQuoteString(con,s))
dbWithTransaction(con,{
  dbExecute(con,"CREATE EXTENSION IF NOT EXISTS postgres_fdw")
  existing <- dbGetQuery(con,"SELECT o.option_name,o.option_value FROM pg_foreign_server s CROSS JOIN LATERAL pg_options_to_table(s.srvoptions) o WHERE srvname='encft_server'")
  if(nrow(existing)) {
    options <- setNames(existing$option_value,existing$option_name)
    stopifnot(options[["dbname"]]==pg_db,options[["host"]]==pg_host,options[["port"]]==pg_port)
  } else dbExecute(con,sprintf("CREATE SERVER encft_server FOREIGN DATA WRAPPER postgres_fdw OPTIONS (host %s,port %s,dbname %s)",q(pg_host),q(pg_port),q(pg_db)))
  mapping <- dbGetQuery(con,"SELECT 1 FROM pg_user_mappings WHERE srvname='encft_server' AND usename=current_user")
  if(!nrow(mapping)) dbExecute(con,sprintf("CREATE USER MAPPING FOR CURRENT_USER SERVER encft_server OPTIONS (user %s,password %s)",q(pg_user),q(password)))
  dbExecute(con,"CREATE SCHEMA IF NOT EXISTS fdw_encft")
  names <- c("personas","hogares","viviendas","indicadores_trimestrales","promedios_anuales")
  for(name in names) {
    obj <- dbGetQuery(con,"SELECT c.relkind,s.srvname FROM pg_class c JOIN pg_namespace n ON n.oid=c.relnamespace LEFT JOIN pg_foreign_table f ON f.ftrelid=c.oid LEFT JOIN pg_foreign_server s ON s.oid=f.ftserver WHERE n.nspname='fdw_encft' AND c.relname=$1",params=list(name))
    if(nrow(obj)) stopifnot(obj$relkind=="f",obj$srvname=="encft_server")
    else dbExecute(con,paste0("IMPORT FOREIGN SCHEMA analitica LIMIT TO (",as.character(dbQuoteIdentifier(con,name)),") FROM SERVER encft_server INTO fdw_encft"))
  }
  dbExecute(con,"CREATE SCHEMA IF NOT EXISTS analitica")
  dbExecute(con,"CREATE OR REPLACE VIEW analitica.encft_indicadores_trimestrales AS SELECT * FROM fdw_encft.indicadores_trimestrales")
  dbExecute(con,"CREATE OR REPLACE VIEW analitica.encft_promedios_anuales AS SELECT * FROM fdw_encft.promedios_anuales")
  dbExecute(con,"INSERT INTO catalogo.fuentes(fuente_id,titulo,organismo,periodo,base_datos,tabla_canonica,unidad_analisis,es_microdato,url,fecha_corte,notas) VALUES('encft_miembros','Encuesta Nacional Continua de Fuerza de Trabajo','BCRD','2016-T1 a 2026-T1',$1,'analitica.personas','observacion de persona por trimestre',true,'https://www.bancentral.gov.do/a/d/2541-encuesta-continua-encft',DATE '2026-03-31','Factores trimestrales. Indicadores laborales de 15 anos y mas. No sumar personas entre trimestres ni unir con microdatos censales. Los ID_HOGAR fuente no coinciden entre modulos en 2023; usar claves estructurales. Panel longitudinal no validado. Fuentes originales y advertencias en encft.meta.') ON CONFLICT(fuente_id) DO UPDATE SET titulo=EXCLUDED.titulo,organismo=EXCLUDED.organismo,periodo=EXCLUDED.periodo,base_datos=EXCLUDED.base_datos,tabla_canonica=EXCLUDED.tabla_canonica,unidad_analisis=EXCLUDED.unidad_analisis,es_microdato=EXCLUDED.es_microdato,url=EXCLUDED.url,fecha_corte=EXCLUDED.fecha_corte,notas=EXCLUDED.notas,actualizado_en=now()",params=list(pg_db))
})
# Full counts were checked locally. Verify remote routing at both coverage ends
# without forcing a second complete scan of the 616-column microdata table.
sample_counts <- dbGetQuery(con,"SELECT trimestre,count(*) AS personas FROM fdw_encft.personas WHERE trimestre IN(20161,20261) GROUP BY trimestre ORDER BY trimestre")
stopifnot(identical(as.integer(sample_counts$trimestre),c(20161L,20261L)),identical(as.numeric(sample_counts$personas),c(21697,16988)))
last <- dbGetQuery(con,"SELECT trimestre,muestra_personas_total,ocupados FROM analitica.encft_indicadores_trimestrales WHERE trimestre=20261")
stopifnot(nrow(last)==1L,as.numeric(last$muestra_personas_total)==16988L)
write_json(list(database="censos_linea_tiempo",server="encft_server",schema="fdw_encft",counts_verified_locally=validation$rows,routed_sample_counts=sample_counts,last_quarter=last,registered_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z")),file.path(base,"qa","central-registration.json"),auto_unbox=TRUE,pretty=TRUE)
print(sample_counts)
dbDisconnect(con)
