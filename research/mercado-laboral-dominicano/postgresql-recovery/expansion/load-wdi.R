source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
root<-file.path(base,'raw/wdi');con<-init_database();catalog<-fromJSON(file.path(root,'indicators.json'))[[2]]
ind<-data.table(indicador=catalog$id,nombre=catalog$name,unidad=catalog$unit,definicion=catalog$sourceNote,organismo=catalog$sourceOrganization,temas=vapply(catalog$topics,function(t)paste(t$value,collapse=' | '),character(1)),fila_origen=seq_len(nrow(catalog)))
esfile<-file.path(root,'indicators-es.json')
if(file.exists(esfile)){
 es<-fromJSON(esfile)[[2]];ix<-match(ind$indicador,es$id);translated<-!is.na(ix)&nzchar(es$name[ix]);ind[translated,nombre:=es$name[ix[translated]]]
 desc<-!is.na(ix)&nzchar(es$sourceNote[ix]);ind[desc,definicion:=es$sourceNote[ix[desc]]]
}
load_table(con,ind,'banco_mundial','indicadores','Banco Mundial WDI','Diccionario de indicadores WDI',file.path(root,'indicators.json'),url='https://api.worldbank.org/v2/indicator?source=2&format=json&per_page=20000',grain='código de indicador; definición y unidad propias')
dbExecute(con,'CREATE TABLE IF NOT EXISTS banco_mundial.observaciones(pais_iso3 text NOT NULL,pais text NOT NULL,indicador text NOT NULL,anio integer NOT NULL,valor double precision NOT NULL,unidad_api text,estado text,decimales integer,actualizacion_fuente date,lote integer NOT NULL,PRIMARY KEY(pais_iso3,indicador,anio))')
dbExecute(con,'CREATE TABLE IF NOT EXISTS meta.wdi_cargas(lote integer PRIMARY KEY,archivo text,sha256 text,filas_api integer,filas_con_datos integer,actualizacion_fuente date)')
files<-sort(list.files(root,pattern='^batch_[0-9]+[.]json$',full.names=TRUE))
for(f in files){
 batch<-as.integer(sub('^batch_([0-9]+)[.]json$','\\1',basename(f)));sha<-digest(file=f,algo='sha256')
 done<-dbGetQuery(con,'SELECT sha256 FROM meta.wdi_cargas WHERE lote=$1',params=list(batch));if(nrow(done)){stopifnot(done$sha256==sha);next}
 content<-fromJSON(f);stopifnot(content[[1]]$pages==1L);d<-content[[2]];dated<-as.Date(content[[1]]$lastupdated)
 x<-data.table(pais_iso3=d$countryiso3code,pais=d$country$value,indicador=d$indicator$id,anio=as.integer(d$date),valor=as.numeric(d$value),unidad_api=d$unit,estado=d$obs_status,decimales=as.integer(d$decimal),actualizacion_fuente=dated,lote=batch)
 x<-x[!is.na(valor)];stopifnot(!anyDuplicated(x[,.(pais_iso3,indicador,anio)]),all(is.finite(x$valor)),all(x$anio<=2026L),all(x$indicador %in% ind$indicador))
 dbWithTransaction(con,{
  if(nrow(x))dbAppendTable(con,Id(schema='banco_mundial',table='observaciones'),as.data.frame(x))
  dbExecute(con,'INSERT INTO meta.wdi_cargas VALUES($1,$2,$3,$4,$5,$6)',params=list(batch,normalizePath(f,winslash='/'),sha,nrow(d),nrow(x),dated))
 })
 cat('Loaded WDI',batch,nrow(x),'observations\n')
}
dbExecute(con,'CREATE INDEX IF NOT EXISTS wdi_indicador_anio ON banco_mundial.observaciones(indicador,anio,pais_iso3)')
dbExecute(con,"CREATE OR REPLACE VIEW analitica.indicadores_internacionales AS SELECT o.pais_iso3,o.pais,o.anio,o.indicador,i.nombre AS indicador_nombre,i.temas,o.valor,coalesce(nullif(o.unidad_api,''),nullif(i.unidad,''),'Ver nombre y definición del indicador') AS unidad,i.definicion,o.estado,o.actualizacion_fuente FROM banco_mundial.observaciones o JOIN banco_mundial.indicadores i USING(indicador)")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.indicadores_rd AS SELECT * FROM analitica.indicadores_internacionales WHERE pais_iso3='DOM'")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.educacion_internacional AS SELECT * FROM analitica.indicadores_internacionales WHERE indicador LIKE 'SE.%'")
summary<-dbGetQuery(con,"SELECT count(*) AS observaciones,count(distinct pais_iso3) AS paises_y_agregados,count(distinct indicador) AS indicadores,min(anio) AS desde,max(anio) AS hasta,count(*) FILTER(WHERE pais_iso3='DOM') AS observaciones_rd,count(distinct indicador) FILTER(WHERE pais_iso3='DOM') AS indicadores_rd,count(*) FILTER(WHERE indicador LIKE 'SE.%') AS observaciones_educacion FROM banco_mundial.observaciones")
dbExecute(con,"INSERT INTO meta.tablas(tabla,fuente,titulo,archivo,hoja,url,sha256,filas,columnas,periodo,grano,notas) VALUES('banco_mundial.observaciones','Banco Mundial WDI','Series internacionales anuales',$1,'','https://data.worldbank.org/country/dominican-republic',$2,$3,10,'1960-2026 según indicador','país/agregado, indicador y año','Solo valores efectivamente publicados, sin interpolación. Cobertura y definiciones variables. Agregados LAC y WLD no son países. No mezclar monedas corrientes, constantes o PPP.') ON CONFLICT(tabla) DO UPDATE SET filas=EXCLUDED.filas,sha256=EXCLUDED.sha256",params=list(normalizePath(file.path(base,'qa/wdi-download.json'),winslash='/'),digest(file=file.path(root,'indicators.json'),algo='sha256'),as.numeric(summary$observaciones)))
write_json(summary,file.path(base,'qa/wdi.json'),auto_unbox=TRUE,pretty=TRUE);print(summary)
dbDisconnect(con)
