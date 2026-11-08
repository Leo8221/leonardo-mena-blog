#!/usr/bin/env Rscript
stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(arrow); library(data.table); library(jsonlite)})
args <- commandArgs(trailingOnly=TRUE)
root <- if(length(args)) args[[1]] else Sys.getenv("ENCFT_SOURCE_ROOT","C:/Users/leona/Desktop/encuesta_trabajo_rd")
out <- "research/mercado-laboral-dominicano/postgresql-recovery/encft/qa"
dir.create(out, recursive = TRUE, showWarnings = FALSE)
rd <- function(module, columns = NULL) {
  path <- file.path(root, "procesado", paste0("ENCFT_", module, "_2016_2026.parquet"))
  if (is.null(columns)) return(as.data.table(read_parquet(path)))
  as.data.table(read_parquet(path, col_select = tidyselect::all_of(columns)))
}
common <- c("ANIO_ARCHIVO","TRIMESTRE","PERIODO","ANO","MES","UPM","VIVIENDA","FACTOR_EXPANSION")
flags <- c("PET","PET10","OCUPADO","SUBOCUPADO","PEA","DESOCUPADO_INI","DESOCUPADO","CESANTE","NUEVO","AMPLIADO","INACTIVO")
m <- rd("Miembros", c(common,"HOGAR","ID_HOGAR","MIEMBRO","ID_PERSONA","SEXO","EDAD", flags, "GRUPO_SECTOR","GRUPO_EMPLEO","GRUPO_EDUCACION"))
h <- rd("Hogar",c(common,"HOGAR","ID_HOGAR","CANTIDAD_MIEMBROS_HOGAR"))
v <- rd("Vivienda",common)
dup <- function(x,k) x[, .N, by=k][N>1, .(groups=.N, extra_rows=sum(N-1))]
checks <- list(
  miembros_persona_mes=dup(m,c("PERIODO","ID_PERSONA")),
  miembros_persona_trimestre=dup(m,c("TRIMESTRE","ID_PERSONA")),
  hogares_hogar_mes=dup(h,c("PERIODO","ID_HOGAR")),
  hogares_hogar_trimestre=dup(h,c("TRIMESTRE","ID_HOGAR")),
  viviendas_clave=dup(v,c("PERIODO","UPM","VIVIENDA")),
  miembros_sin_hogar=nrow(m[!h,on=.(PERIODO,ID_HOGAR)]),
  miembros_sin_hogar_clave_estructural=nrow(m[!h,on=.(PERIODO,UPM,VIVIENDA,HOGAR)]),
  hogares_clave_estructural=dup(h,c("PERIODO","UPM","VIVIENDA","HOGAR")),
  miembros_clave_estructural=dup(m,c("PERIODO","UPM","VIVIENDA","HOGAR","MIEMBRO")),
  hogares_sin_vivienda=nrow(h[!v,on=.(PERIODO,UPM,VIVIENDA)]),
  miembros_fecha_inconsistente=m[ANIO_ARCHIVO!=PERIODO%/%100 | TRIMESTRE!=ANIO_ARCHIVO*10+ceiling((PERIODO%%100)/3),.N],
  miembros_peso_invalido=m[is.na(FACTOR_EXPANSION)|!is.finite(FACTOR_EXPANSION)|FACTOR_EXPANSION<=0,.N],
  hogares_peso_invalido=h[is.na(FACTOR_EXPANSION)|!is.finite(FACTOR_EXPANSION)|FACTOR_EXPANSION<=0,.N],
  viviendas_peso_invalido=v[is.na(FACTOR_EXPANSION)|!is.finite(FACTOR_EXPANSION)|FACTOR_EXPANSION<=0,.N],
  pet_distinto_edad15=m[PET!=as.numeric(EDAD>=15),.N],
  pet10_distinto_edad10=m[PET10!=as.numeric(EDAD>=10),.N],
  pea_menos_ocupado_desocupado=m[PEA!=OCUPADO+DESOCUPADO,.N],
  pea_menos_ocupado_desocupado_ini=m[PEA!=OCUPADO+DESOCUPADO_INI,.N],
  pet_menos_pea_inactivo=m[PET!=PEA+INACTIVO,.N],
  pet10_menos_pea_inactivo=m[PET10!=PEA+INACTIVO,.N],
  pet_menos_pea_inactivo_solo15=m[EDAD>=15 & PET!=PEA+INACTIVO,.N]
)
print(checks)
print(m[!h,on=.(PERIODO,ID_HOGAR)][,.N,by=ANIO_ARCHIVO])
by15 <- m[EDAD>=15, c(list(muestra=.N,poblacion15=sum(FACTOR_EXPANSION)),lapply(.SD,function(x) sum(x*FACTOR_EXPANSION))),by=TRIMESTRE,.SDcols=flags][order(TRIMESTRE)]
by15[, tasa_desocupacion :=100*DESOCUPADO/PEA]
by15[, tasa_ocupacion :=100*OCUPADO/PET]
by15[, tasa_participacion :=100*PEA/PET]
print(tail(by15,3))
fwrite(by15,file.path(out,"source-quarterly-15plus.csv"))
quarters <- m[, c(list(muestra=.N,meses=uniqueN(PERIODO),poblacion=sum(FACTOR_EXPANSION)),lapply(.SD,function(x) sum(x*FACTOR_EXPANSION))), by=.(TRIMESTRE),.SDcols=flags][order(TRIMESTRE)]
quarters[, tasa_desocupado :=100*DESOCUPADO/PEA]
quarters[, tasa_desocupado_ini :=100*DESOCUPADO_INI/PEA]
print(quarters)
fwrite(quarters,file.path(out,"source-quarterly-controls.csv"))
fwrite(m[,.(filas=.N,personas_id=uniqueN(ID_PERSONA)),by=ANIO_ARCHIVO],file.path(out,"source-yearly-counts.csv"))
panel <- rbindlist(lapply(2016:2025,function(y) {
  before <- m[TRIMESTRE==y*10+4,ID_PERSONA]
  after <- m[TRIMESTRE==(y+1)*10+1,ID_PERSONA]
  data.table(anio_origen=y,anio_destino=y+1,personas_t4=length(before),ids_en_comun=length(intersect(before,after)),nota="Coincidencia literal de ID; no valida identidad longitudinal ni pesos de panel.")
}))
fwrite(panel,file.path(out,"panel-boundary-audit.csv"))
schemas <- rbindlist(lapply(c("Miembros","Hogar","Vivienda","Diccionario","Tasa_Monedas"),function(mod) {
  paths <- list.files(file.path(root,"procesado","parquet_por_anio",mod),full.names=TRUE,pattern="[.]parquet$")
  rbindlist(lapply(paths,function(p) {
    r <- ParquetFileReader$create(p)
    data.table(module=mod,year=as.integer(sub("[.]parquet$","",basename(p))),rows=r$num_rows,columns=length(r$GetSchema()$names))
  }))
}))
fwrite(schemas,file.path(out,"yearly-parquet-inventory.csv"))
print(schemas[module=="Miembros"])
d <- rd("Diccionario")
relevant <- unique(d[CAMPO %chin% c(flags,"FACTOR_EXPANSION","ID_HOGAR","ID_PERSONA","TRIMESTRE","PERIODO","SEXO","ZONA","GRUPO_EMPLEO","GRUPO_SECTOR"),.(ANIO_ARCHIVO,PERIODO,SECCION,CAMPO,TEXTO,VALOR,DESCRIPCION)])
fwrite(relevant,file.path(out,"dictionary-core.csv"))
print(unique(relevant[,.(SECCION,CAMPO,TEXTO,VALOR,DESCRIPCION)]))
print(m[,.(n=.N),by=.(GRUPO_SECTOR,GRUPO_EMPLEO)])
write_json(checks,file.path(out,"source-quality.json"),pretty=TRUE,auto_unbox=TRUE,na="null")
