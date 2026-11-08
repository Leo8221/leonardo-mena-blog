#!/usr/bin/env Rscript
# Verify the actual database against source files and independent R aggregates.
stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(DBI);library(RPostgres);library(data.table);library(jsonlite);library(arrow)})
base <- "research/mercado-laboral-dominicano/postgresql-recovery/encft"
qa <- file.path(base,"qa")
root <- Sys.getenv("ENCFT_SOURCE_ROOT","C:/Users/leona/Desktop/encuesta_trabajo_rd")
password <- Sys.getenv("PG_RECOVERY_PASSWORD")
stopifnot(nzchar(password))
con <- dbConnect(Postgres(),host=Sys.getenv("PGHOST","127.0.0.1"),port=as.integer(Sys.getenv("PGPORT","5433")),user=Sys.getenv("PGUSER","postgres"),password=password,dbname=Sys.getenv("ENCFT_DATABASE","encft"),options="-c application_name=encft_verify -c statement_timeout=600000 -c work_mem=64MB")
results <- list()
cat("Reading the quarterly view once, then checking row-level invariants in one pass.\n")
q <- as.data.table(dbGetQuery(con,"SELECT * FROM analitica.indicadores_trimestrales ORDER BY trimestre"))
stopifnot(nrow(q)==41L,all(q$meses_disponibles==3L))
bulk <- dbGetQuery(con,"SELECT count(*) AS filas_miembros,
 count(DISTINCT m.periodo) AS periodos_mensuales,
 count(*)-count(DISTINCT (m.trimestre,m.id_persona)) AS sin_duplicados_persona_trimestre,
 count(*) FILTER(WHERE h.fila_origen IS NULL) AS miembros_sin_hogar_clave_estructural,
 count(*) FILTER(WHERE m.factor_expansion IS NULL OR m.factor_expansion<=0 OR m.factor_expansion IN ('NaN'::float8,'Infinity'::float8,'-Infinity'::float8)) AS miembros_peso_invalido,
 count(*) FILTER(WHERE abs(m.factor_expansion-h.factor_expansion)>0.00000001) AS peso_miembro_hogar_distinto,
 count(*) FILTER(WHERE m.id_provincia<>h.id_provincia OR m.id_municipio<>h.id_municipio) AS geografia_enlace_distinta,
 count(*) FILTER(WHERE m.cantidad_miembros_hogar<>h.cantidad_miembros_hogar) AS tamano_hogar_distinto,
 count(*) FILTER(WHERE m.anio_archivo<>m.periodo/100 OR m.trimestre<>m.anio_archivo*10+(m.periodo%100+2)/3) AS periodo_trimestre_coherente,
 count(*) FILTER(WHERE m.pet<>CASE WHEN m.edad>=15 THEN 1 ELSE 0 END) AS pet_equivale_15_mas,
 count(*) FILTER(WHERE m.edad>=15 AND (m.pea<>m.ocupado+m.desocupado OR m.pet<>m.pea+m.inactivo)) AS pea_identidad_15_mas,
 count(*) FILTER(WHERE m.pet NOT IN(0,1) OR m.pea NOT IN(0,1) OR m.ocupado NOT IN(0,1) OR m.desocupado NOT IN(0,1) OR m.inactivo NOT IN(0,1) OR m.pet IS NULL OR m.pea IS NULL OR m.ocupado IS NULL OR m.desocupado IS NULL OR m.inactivo IS NULL) AS dominio_flags_laborales,
 count(*) FILTER(WHERE m.edad>=15 AND m.ocupado=1 AND (m.grupo_empleo NOT IN('Empleo Formal','Empleo Informal') OR m.grupo_empleo IS NULL)) AS informalidad_clasificada,
 count(*) FILTER(WHERE m.id_hogar IS DISTINCT FROM h.id_hogar) AS id_hogar_original_no_enlaza,
 count(*) FILTER(WHERE m.desocupado_ini<>m.desocupado) AS iniciadores_no_equivalen_desocupados
 FROM microdatos.miembros m LEFT JOIN microdatos.hogares h USING(periodo,upm,vivienda,hogar)")
bulk$trimestres_completos <- sum(q$meses_disponibles==3L)
check <- function(name,sql,expected=0,severity="error",note="") {
  # Bulk checks use the same predicates above to avoid rescanning the wide table.
  value <- if(name %in% names(bulk)) as.numeric(bulk[[name]][[1]]) else as.numeric(dbGetQuery(con,sql)[[1]][[1]])
  passed <- isTRUE(all.equal(value,expected))
  results[[length(results)+1L]] <<- data.frame(control=name,observado=value,esperado=expected,estado=if(passed) "ok" else if(severity=="warning") "advertencia" else "error",nota=note)
  cat(if(passed) "OK" else toupper(severity),name,":",value,"\n")
  if(!passed && severity=="error") stop("Validation failed: ",name)
}
expected <- c(miembros=790993,hogares=258764,viviendas=258607,diccionario=115178,tasas_monedas=399)
for(mod in names(expected)) check(paste0("filas_",mod),paste("SELECT count(*) FROM",if(mod %in% c("diccionario","tasas_monedas")) "meta" else "microdatos",".",mod),expected[[mod]])
check("trimestres_completos","SELECT count(*) FROM analitica.indicadores_trimestrales WHERE meses_disponibles=3",41)
check("periodos_mensuales","SELECT count(DISTINCT periodo) FROM microdatos.miembros",123)
check("sin_duplicados_persona_trimestre","SELECT count(*) FROM (SELECT trimestre,id_persona FROM microdatos.miembros GROUP BY trimestre,id_persona HAVING count(*)>1) q")
check("miembros_sin_hogar_clave_estructural","SELECT count(*) FROM microdatos.miembros m LEFT JOIN microdatos.hogares h USING(periodo,upm,vivienda,hogar) WHERE h.fila_origen IS NULL")
check("hogares_sin_vivienda","SELECT count(*) FROM microdatos.hogares h LEFT JOIN microdatos.viviendas v USING(periodo,upm,vivienda) WHERE v.fila_origen IS NULL")
check("miembros_peso_invalido","SELECT count(*) FROM microdatos.miembros WHERE factor_expansion IS NULL OR factor_expansion<=0 OR factor_expansion IN ('NaN'::float8,'Infinity'::float8,'-Infinity'::float8)")
check("hogares_peso_invalido","SELECT count(*) FROM microdatos.hogares WHERE factor_expansion IS NULL OR factor_expansion<=0")
check("peso_miembro_hogar_distinto","",0,"warning","2627 registros de 2023-T4 presentan pesos distintos entre modulos en los propios archivos fuente. Una muestra de claves/pesos se contrasto con el Excel original. Conservar y usar el factor del modulo correspondiente; no sustituirlo al unir.")
check("geografia_enlace_distinta","",0)
check("tamano_hogar_distinto","",0,"warning","7 registros de miembros de noviembre de 2023 difieren del tamano declarado en Hogar. No corregir ni inferir composicion del hogar sin revisar estos casos.")
check("periodo_trimestre_coherente","SELECT count(*) FROM microdatos.miembros WHERE anio_archivo<>periodo/100 OR trimestre<>anio_archivo*10+(periodo%100+2)/3")
check("pet_equivale_15_mas","SELECT count(*) FROM microdatos.miembros WHERE pet<>CASE WHEN edad>=15 THEN 1 ELSE 0 END")
check("pea_identidad_15_mas","SELECT count(*) FROM microdatos.miembros WHERE edad>=15 AND (pea<>ocupado+desocupado OR pet<>pea+inactivo)")
check("dominio_flags_laborales","SELECT count(*) FROM microdatos.miembros WHERE pet NOT IN(0,1) OR pea NOT IN(0,1) OR ocupado NOT IN(0,1) OR desocupado NOT IN(0,1) OR inactivo NOT IN(0,1) OR pet IS NULL OR pea IS NULL OR ocupado IS NULL OR desocupado IS NULL OR inactivo IS NULL")
check("informalidad_clasificada","SELECT count(*) FROM microdatos.miembros WHERE edad>=15 AND ocupado=1 AND (grupo_empleo NOT IN('Empleo Formal','Empleo Informal') OR grupo_empleo IS NULL)")
check("2026_sin_promedio_anual_incompleto","SELECT count(*) FROM analitica.promedios_anuales WHERE anio=2026 AND NOT anio_completo AND trimestres_disponibles=1 AND ocupados_promedio IS NULL",1)
check("viviendas_factor_nulo_conservado","SELECT count(*) FROM microdatos.viviendas WHERE factor_expansion IS NULL",0,"warning","Una observacion original conservada; no imputar factor ni usarla en sumas ponderadas sin tratamiento explicito.")
check("id_hogar_original_no_enlaza","SELECT count(*) FROM microdatos.miembros m LEFT JOIN microdatos.hogares h USING(periodo,id_hogar) WHERE h.fila_origen IS NULL",0,"warning","73840 registros de 2023; usar las claves estructurales verificadas, no ID_HOGAR solo.")
check("iniciadores_no_equivalen_desocupados","SELECT count(*) FROM microdatos.miembros WHERE desocupado_ini<>desocupado",0,"warning","Conservar ambas variables; la tasa abierta usa DESOCUPADO/PEA en personas de 15 anos y mas.")
check("ruptura_ids_panel_2023_2024","SELECT CASE WHEN EXISTS(SELECT 1 FROM microdatos.miembros a JOIN microdatos.miembros b USING(id_persona) WHERE a.trimestre=20234 AND b.trimestre=20241) THEN 0 ELSE 1 END",0,"warning","No hay IDs_PERSONA comunes entre 2023-T4 y 2024-T1. No inferir transiciones ni una sustitucion total real del panel sin validar los identificadores.")
ref <- fread(file.path(qa,"source-quarterly-15plus.csv"))
stopifnot(identical(as.integer(q$trimestre),as.integer(ref$TRIMESTRE)))
mapping <- c(poblacion_15_mas="poblacion15",ocupados="OCUPADO",fuerza_trabajo="PEA",desocupados="DESOCUPADO",fuera_fuerza_trabajo="INACTIVO",tasa_desocupacion_abierta="tasa_desocupacion",tasa_ocupacion="tasa_ocupacion",tasa_participacion="tasa_participacion")
diffs <- rbindlist(lapply(names(mapping),function(nm) {
  d <- abs(q[[nm]]-ref[[mapping[[nm]]]])
  stopifnot(all(d<=1e-7*pmax(1,abs(ref[[mapping[[nm]]]]))))
  data.table(metrica=nm,diferencia_maxima=max(d),trimestres=length(d),estado="ok")
}))
fwrite(diffs,file.path(qa,"sql-versus-r.csv"))
fwrite(q,file.path(qa,"database-quarterly-controls.csv"))
bulk$bcrd_ocupados_2025t4_redondeado <- round(q[trimestre==20254,ocupados])
bulk$bcrd_desocupacion_2025t4_decima <- round(q[trimestre==20254,tasa_desocupacion_abierta],1)
bulk$bcrd_ocupados_2016t4_redondeado <- round(q[trimestre==20164,ocupados])
bulk$bcrd_ocupados_2017t4_redondeado <- round(q[trimestre==20174,ocupados])
check("bcrd_ocupados_2025t4_redondeado","",5168878,note="BCRD, Panorama Economico Region Norte 2026-I, pagina 23; referencia independiente publicada.")
check("bcrd_desocupacion_2025t4_decima","",5.0,note="BCRD, PERN 2026-I, pagina 23. Esta comprobacion no certifica toda la serie como publicacion oficial.")
check("bcrd_ocupados_2016t4_redondeado","",4310845,note="BCRD, Cronica Central mayo 2018, pagina 6: ocupados de 15 anos y mas, oct-dic 2016.")
check("bcrd_ocupados_2017t4_redondeado","",4431912,note="BCRD, Cronica Central mayo 2018, pagina 6: ocupados de 15 anos y mas, oct-dic 2017.")
# Full-column round trip at deterministic positions, including null and empty strings.
files <- c(miembros="ENCFT_Miembros_2016_2026.parquet",hogares="ENCFT_Hogar_2016_2026.parquet",viviendas="ENCFT_Vivienda_2016_2026.parquet",diccionario="ENCFT_Diccionario_2016_2026.parquet",tasas_monedas="tasas-monedas-completas.parquet")
roundtrip <- list()
for(mod in names(files)) {
  path <- if(mod=="tasas_monedas") file.path(qa,files[[mod]]) else file.path(root,"procesado",files[[mod]])
  positions <- unique(as.integer(c(1,2,ceiling(expected[[mod]]/2),expected[[mod]])))
  reader <- Scanner$create(open_dataset(path),batch_size=20000L,use_threads=FALSE)$ToRecordBatchReader()
  offset <- 0L
  samples <- list()
  repeat {
    batch <- reader$read_next_batch()
    if(is.null(batch)) break
    idx <- positions[positions>offset & positions<=offset+batch$num_rows]-offset
    if(length(idx)) {
      df <- as.data.frame(batch)[idx,,drop=FALSE]
      names(df) <- tolower(names(df))
      df <- cbind(data.frame(fila_origen=offset+idx),df)
      samples[[length(samples)+1L]] <- df
    }
    offset <- offset+batch$num_rows
  }
  source <- do.call(rbind,samples)
  schema <- if(mod %in% c("diccionario","tasas_monedas")) "meta" else "microdatos"
  actual <- dbGetQuery(con,paste0("SELECT * FROM ",schema,".",mod," WHERE fila_origen IN (",paste(positions,collapse=","),") ORDER BY fila_origen"))
  stopifnot(identical(names(actual),names(source)))
  for(nm in names(source)) {
    x <- actual[[nm]]
    y <- source[[nm]]
    if(inherits(x,"integer64")) x <- as.numeric(x)
    if(!isTRUE(all.equal(x,y,tolerance=1e-12,check.attributes=FALSE))) stop("Roundtrip mismatch: ",mod,".",nm)
  }
  roundtrip[[mod]] <- data.table(modulo=mod,filas_comparadas=nrow(source),columnas_comparadas=ncol(source),estado="ok")
  cat("OK full-column roundtrip:",mod,"\n")
}
fwrite(rbindlist(roundtrip),file.path(qa,"roundtrip-checks.csv"))
controls <- as.data.frame(rbindlist(results))
fwrite(controls,file.path(qa,"database-quality.csv"))
# Only validation metadata is replaced on repeat runs; no microdata are changed.
dbWithTransaction(con,{
  dbExecute(con,"CREATE TABLE IF NOT EXISTS meta.controles_calidad (control text PRIMARY KEY,observado double precision,esperado double precision,estado text,nota text,verificado_en timestamptz NOT NULL DEFAULT now())")
  for(i in seq_len(nrow(controls))) dbExecute(con,"INSERT INTO meta.controles_calidad(control,observado,esperado,estado,nota) VALUES($1,$2,$3,$4,$5) ON CONFLICT(control) DO UPDATE SET observado=EXCLUDED.observado,esperado=EXCLUDED.esperado,estado=EXCLUDED.estado,nota=EXCLUDED.nota,verificado_en=now()",params=unname(as.list(controls[i,])))
})
summary <- list(database=Sys.getenv("ENCFT_DATABASE","encft"),validated_at=format(Sys.time(),"%Y-%m-%dT%H:%M:%S%z"),data_start="2016-01-01",data_end="2026-03-31",quarters=nrow(q),rows=as.list(expected),size=dbGetQuery(con,"SELECT pg_database_size(current_database()) AS bytes,pg_size_pretty(pg_database_size(current_database())) AS pretty"),checks=nrow(controls),warnings=controls[controls$estado=="advertencia",],official_reference="https://cdn.bancentral.gov.do/documents/publicaciones-economicas/panorama-economico-de-la-region-norte/documents/PERN_2026-I.pdf",sql_vs_r=diffs,roundtrip=rbindlist(roundtrip))
write_json(summary,file.path(qa,"validation-summary.json"),auto_unbox=TRUE,pretty=TRUE,na="null")
print(summary$size)
cat("Validation completed; known source warnings are documented.\n")
dbDisconnect(con)
