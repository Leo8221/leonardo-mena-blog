source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
source(file.path(base,'time-helpers.R'))
f<-fromJSON(file.path(base,'qa/fx-download.json'))
out<-list()
for(s in excel_sheets(f$path)){
 x<-grid_read(f$path,s)
 yr<-year_num(x[,1])
 annual<-grepl('Anual',s)
 daily<-s=='Diaria'
 quarter<-grepl('Trimestral',s)
 mo<-if(annual)rep(1L,nrow(x))else month_num(x[,2])
 day<-if(daily)number(x[,3])else rep(1L,nrow(x))
 valid<-!is.na(yr)&!is.na(mo)&!is.na(day)
 rr<-which(valid)
 dt<-as.Date(sprintf('%04d-%02d-%02d',yr[rr],mo[rr],as.integer(day[rr])))
 cc<-if(daily)4:5 else if(annual)2:3 else 3:4
 for(j in seq_along(cc)){
  v<-number(x[rr,cc[j]])
  keep<-!is.na(v)
  out[[length(out)+1L]]<-data.table(hoja=s,periodo=dt[keep],frecuencia=if(daily)'diaria'else if(annual)'anual'else if(quarter)'trimestral'else'mensual',estadistica=if(startsWith(s,'FP'))'fin_periodo'else if(daily)'observacion_diaria'else'promedio_periodo',operacion=c('compra','venta')[j],valor_dop_por_usd=v[keep],fila_excel=rr[keep],columna_excel=cc[j])
 }
}
z<-rbindlist(out)
z[,fila_origen:=.I]
stopifnot(!anyDuplicated(z[,.(hoja,periodo,operacion)]),all(z$valor_dop_por_usd>0),max(z$periodo)<=Sys.Date())
con<-init_database()
load_table(con,z,'bcrd','tipo_cambio','BCRD','Dólar de referencia del mercado spot: histórico diario y agregaciones oficiales',f$path,url=f$url,period=paste(min(z$periodo),max(z$periodo),sep=' / '),grain='frecuencia, estadística, fecha y operación',notes='Compra y venta en DOP por USD. Promedio y fin de período son series distintas. La fecha de cada agregado es el inicio del período. No se interpolan días sin publicación.')
dbExecute(con,'CREATE INDEX IF NOT EXISTS tipo_cambio_periodo ON bcrd.tipo_cambio(frecuencia,estadistica,periodo,operacion)')
dbExecute(con,'CREATE OR REPLACE VIEW analitica.tipo_cambio AS SELECT periodo,frecuencia,estadistica,operacion,valor_dop_por_usd FROM bcrd.tipo_cambio')
write_json(dbGetQuery(con,'SELECT frecuencia,estadistica,count(*) observaciones,min(periodo) desde,max(periodo) hasta FROM bcrd.tipo_cambio GROUP BY 1,2 ORDER BY 1,2'),file.path(base,'qa/fx.json'),auto_unbox=TRUE,pretty=TRUE)
dbDisconnect(con)
