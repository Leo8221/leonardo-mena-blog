source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
source(file.path(base,'time-helpers.R'))
m<-fromJSON(file.path(base,'qa/download-manifest.json'))
con<-init_database()
checks<-list()
for(i in which(m$group %in% c('tss','dga'))){
 f<-m[i,]
 txt<-readLines(f$path,encoding=if(f$group=='tss')'latin1'else'UTF-8',warn=FALSE)
 if(f$group=='tss')txt<-iconv(txt,from='latin1',to='UTF-8')
 x<-fread(text=paste(txt,collapse='\n'),encoding='UTF-8',colClasses='character',check.names=FALSE)
 x<-x[,which(vapply(x,function(z)any(!is.na(z)&nzchar(z)),logical(1))),with=FALSE]
 labels<-names(x)
 setnames(x,clean_names(labels))
 x<-typed_text(x)
 yr<-x[[grep('^(ano|a_o)$',names(x))[1]]]
 mo<-month_num(x$mes)
 stopifnot(length(yr)==nrow(x),!anyNA(mo),!anyNA(yr))
 x$periodo<-as.Date(sprintf('%04d-%02d-01',as.integer(yr),mo))
 x$fila_origen<-seq_len(nrow(x))+1L
 if(f$group=='tss'){
  table<-if(grepl('empleadores',f$path))'empleadores'else'empleos'
  stopifnot(!anyDuplicated(x$periodo))
  if(table=='empleos')checks$tss_total<-sum(x$total!=x$empresa_privada+x$publica_centralizada+x$publica_descentralizada)
 }else{
  table<-if(grepl('importaciones',f$path))'importaciones_capitulo'else'exportaciones_capitulo'
  stopifnot(!anyDuplicated(x[,c('codigo_capitulo','periodo')]))
 }
 load_table(con,x,f$group,table,toupper(f$group),table,f$path,url=f$url,period=paste(min(x$periodo),max(x$periodo),sep=' / '),grain=if(f$group=='tss')'mes'else'capítulo arancelario y mes',notes=if(f$group=='tss')'Empleos cotizantes no son personas únicas. Empleadores cotizantes es otro universo. No sumar meses para contar personas.'else'Importe numérico original sin cambio de escala. La unidad monetaria se mantiene pendiente de cotejo metodológico; no usar como millones por el título genérico del portal.',original_names=c(labels,'Primer día del mes','Fila CSV'))
 checks[[table]]<-list(rows=nrow(x),from=min(x$periodo),to=max(x$periodo))
}
dbExecute(con,"CREATE OR REPLACE VIEW analitica.empleo_formal AS SELECT e.periodo,e.empresa_privada,e.publica_centralizada,e.publica_descentralizada,e.total AS empleos_cotizantes,p.empleadores_cotizantes,100.0*(e.total/NULLIF(lag(e.total,12) OVER(ORDER BY e.periodo),0)::numeric-1) AS variacion_interanual_empleos FROM tss.empleos e LEFT JOIN tss.empleadores p USING(periodo)")
# Unit validation is reproducible in verify-expansion.R, against the official 2018 report.
dbExecute(con,"CREATE OR REPLACE VIEW analitica.comercio_capitulo AS SELECT periodo,'importaciones'::text AS flujo,codigo_capitulo::text,capitulo_descripcion AS capitulo,importaciones AS valor_original,'USD FOB; escala contrastada'::text AS unidad,importaciones AS valor_usd,importaciones/1e6 AS millones_usd FROM dga.importaciones_capitulo UNION ALL SELECT periodo,'exportaciones'::text,codigo_capitulo::text,capitulo_descripcion,exportaciones,'USD FOB; escala contrastada'::text,exportaciones,exportaciones/1e6 FROM dga.exportaciones_capitulo")
write_json(checks,file.path(base,'qa/trade-tss.json'),auto_unbox=TRUE,pretty=TRUE)
print(checks)
dbDisconnect(con)
