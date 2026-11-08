source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
source(file.path(base,'time-helpers.R'))
m<-fromJSON(file.path(base,'qa/download-manifest.json'))
out<-list()
used<-list()
getfile<-function(id){f<-m[m$id==id,];stopifnot(nrow(f)==1,f$status!='failed');f}
add<-function(f,s,ds,label,date,value,unit,r,c,scale=1,frequency='mensual',note=''){
 ok<-!is.na(date)&!is.na(value)
 if(!any(ok))return(invisible(NULL))
 n<-length(date)
 label<-rep(label,length.out=n)
 unit<-rep(unit,length.out=n)
 r<-rep(r,length.out=n)
 c<-rep(c,length.out=n)
 note<-rep(note,length.out=n)
 out[[length(out)+1L]]<<-data.table(dataset=ds,serie=label[ok],periodo=date[ok],frecuencia=frequency,valor=value[ok]*scale,unidad=unit[ok],valor_fuente=value[ok],fuente_id=f$id,hoja=s,fila_excel=r[ok],columna_excel=c[ok],nota_periodo=note[ok])
 used[[f$id]]<<-f
}
vertical<-function(id,ds,cols,labels,units,y=1,mo=2,start=1L,scales=1){
 f<-getfile(id)
 s<-excel_sheets(f$path)[1]
 x<-grid_read(f$path,s)
 dt<-vertical_dates(x,y,mo,start)
 summary_rows<-if(id=='a8b898c70528')which(grepl('^Dic[.] ',x[,1]))else integer()
 for(j in seq_along(cols)){
  monthly<-dt
  if(length(summary_rows))monthly[summary_rows]<-as.Date(NA)
  add(f,s,ds,labels[j],monthly,number(x[,cols[j]]),rep(units,length.out=length(cols))[j],seq_len(nrow(x)),cols[j],rep(scales,length.out=length(cols))[j])
  if(length(summary_rows))add(f,s,paste0(ds,'_cierre_anual'),labels[j],dt[summary_rows],number(x[summary_rows,cols[j]]),rep(units,length.out=length(cols))[j],summary_rows,cols[j],frequency='anual',note='Resumen de diciembre publicado aparte de la serie mensual; no concatenar ambos.')
 }
}
vertical('0d521ff39a46','imae_2018',3:16,c('Índice original','Variación interanual original','Variación acumulada original','Variación promedio 12 meses original','Índice desestacionalizado','Variación mensual desestacionalizada','Variación interanual desestacionalizada','Variación acumulada desestacionalizada','Variación promedio 12 meses desestacionalizada','Índice tendencia ciclo','Variación mensual tendencia ciclo','Variación interanual tendencia ciclo','Variación acumulada tendencia ciclo','Variación promedio 12 meses tendencia ciclo'),c('indice_2018_100',rep('porcentaje',3),'indice_2018_100',rep('porcentaje',4),'indice_2018_100',rep('porcentaje',4)),start=9L)
vertical('bc4fa06686d7','ipc_nacional',3:7,c('Índice general','Inflación mensual','Inflación acumulada diciembre','Inflación interanual','Inflación promedio 12 meses'),c('indice_oct2019_sep2020_100',rep('porcentaje',4)),start=7L)
vertical('a8b898c70528','ipc_subyacente',3:6,c('Índice subyacente','Inflación mensual subyacente','Inflación acumulada subyacente','Inflación interanual subyacente'),c('indice_oct2019_sep2020_100',rep('porcentaje',3)),start=5L)
vertical('ccf12eb9c121','canasta_quintiles',3:8,c(paste('Quintil',1:5),'Nacional'),'DOP',start=5L)
vertical('4b17eb9473e7','canasta_regiones',3:6,c('Ozama','Norte o Cibao','Este','Sur'),'DOP',start=4L)
vertical('613d318a6a55','politica_monetaria',3:6,c('Tasa de política monetaria','Facilidad de depósito','Facilidad de préstamo','Lombarda'),'porcentaje_anual',start=7L,scales=100)
vertical('3f00431056f1','tasas_activas_mn',3:15,c('Agricultura ganadería silvicultura y pesca','Minas y canteras','Manufacturas','Construcción','Tarjeta de crédito','Otros consumos','Comercio','Transporte almacenamiento información y comunicaciones','Alojamiento y comidas','Vivienda','Otras actividades inmobiliarias','Otros sectores','Interbancaria ponderada'),'porcentaje_anual',start=7L,scales=100)
for(id in c('e444fec4cfeb','7e64a3000508','4e4e30dfe511')){
 f<-getfile(id)
 s<-excel_sheets(f$path)[1]
 x<-grid_read(f$path,s)
 single<-id=='4e4e30dfe511'
 dt<-vertical_dates(x,1,if(single)1 else 2,if(single)8L else 5L)
 cc<-if(single)which(!is.na(x[3,])&seq_len(ncol(x))>1)else seq(3,ncol(x),2)
 cc<-cc[cc<ncol(x)]
 for(c in cc){
  add(f,s,if(single)'ipc_grupos' else if(id=='e444fec4cfeb')'ipc_quintiles' else 'ipc_regiones',paste(x[3,c],'Índice'),dt,number(x[,c]),'indice_oct2019_sep2020_100',seq_len(nrow(x)),c)
  add(f,s,if(single)'ipc_grupos' else if(id=='e444fec4cfeb')'ipc_quintiles' else 'ipc_regiones',paste(x[3,c],'Inflación mensual'),dt,number(x[,c+1]),'porcentaje',seq_len(nrow(x)),c+1)
 }
}
# Months on rows and years on columns; complete histories, never annual totals plus months.
for(id in c('31f8bc50c9de','0be52ba4d903','60fd1b3952de','900f472eee99')){
 f<-getfile(id)
 s<-excel_sheets(f$path)[1]
 x<-grid_read(f$path,s)
 yrrow<-if(id=='0be52ba4d903')6L else 8L
 yr<-fill_down(year_num(x[yrrow,]))
 mo<-month_num(x[,1])
 rr<-which(!is.na(mo))
 rr<-rr[rr>yrrow]
 for(c in which(!is.na(yr)&seq_along(yr)>1)){
  ds<-switch(id,'31f8bc50c9de'='remesas','0be52ba4d903'='reservas','60fd1b3952de'='remesas_transacciones','900f472eee99'='remesas_promedio')
  lab<-if(id=='0be52ba4d903')x[8,c]else ds
  unit<-switch(id,'31f8bc50c9de'='USD','0be52ba4d903'='millones_USD','60fd1b3952de'='transacciones','900f472eee99'='USD_por_transaccion')
  add(f,s,ds,lab,as.Date(sprintf('%04d-%02d-01',yr[c],mo[rr])),number(x[rr,c]),unit,rr,c)
 }
}
# Territorial banking stocks by quarter. Source columns identify quarter-end month.
for(id in c('ba8081747640','8ccdfd0126ec')){
 f<-getfile(id)
 s<-excel_sheets(f$path)[1]
 x<-grid_read(f$path,s)
 for(c in 3:ncol(x)){
  label<-x[4,c]
  mo<-month_num(label)
  yr<-suppressWarnings(as.integer(sub('^.*-','',label)))+2000L
  if(is.na(mo)||is.na(yr))next
  rr<-which(!is.na(x[,2])&seq_len(nrow(x))>4)
  labels<-x[rr,2]
  regions<-fill_down(x[,1])
  is_subtotal<-grepl('SUB-TOTAL',labels,fixed=TRUE)
  labels[is_subtotal]<-paste(regions[rr[is_subtotal]],labels[is_subtotal],sep=' / ')
  add(f,s,if(id=='ba8081747640')'prestamos_provincias' else 'depositos_provincias',labels,rep(as.Date(sprintf('%04d-%02d-01',yr,((mo-1L)%/%3L)*3L+1L)),length(rr)),number(x[rr,c]),'millones_DOP',rr,c,frequency='trimestral',note='Saldo al cierre del trimestre; fecha normalizada al inicio del trimestre. No sumar entre períodos ni subtotales con provincias.')
 }
}
# Full historical credit by destination (Excel serial dates on header).
f<-getfile('c23160200995')
s<-excel_sheets(f$path)[1]
x<-grid_read(f$path,s)
for(c in 2:ncol(x)){
 dt<-header_date(x[6,c])
 if(is.na(dt))next
 rr<-which(!is.na(x[,1])&seq_len(nrow(x))>8)
 add(f,s,'credito_destino',paste0(x[rr,1],' [fila ',rr,']'),rep(as.Date(format(dt,'%Y-%m-01')),length(rr)),number(x[rr,c]),'millones_DOP',rr,c,note='Saldos; conservar moneda y jerarquía del rótulo. No sumar totales con componentes.')
}
# GDP retrospective with a consistent 2018 reference, preserving cumulative quarters separately.
f<-getfile('e1d15befa0ba')
for(s in excel_sheets(f$path)){
 x<-grid_read(f$path,s)
 yr<-fill_down(year_num(x[7,]))
 q<-match(x[8,],c('E-M','A-J','J-S','O-D'))
 acum<-grepl('Acum',s)
 if(acum)q<-match(x[8,],c('E-M','E-J','E-S','E-D'))
 section<-x[7,1]
 for(r in 9:nrow(x)){
  lab<-x[r,1]
  if(is.na(lab))next
  if(grepl('^(INDICE|VALOR AGREGADO POR|TASAS DE CRECIMIENTO|INCIDENCIA|PARTICIPACI|PONDERACION)',toupper(lab))){section<-lab;next}
  val<-number(x[r,])
  cc<-which(!is.na(yr)&!is.na(q)&!is.na(val))
  if(!length(cc)){section<-lab;next}
  is_share<-grepl('PONDERACION',toupper(section))
  unit<-if(grepl('INCIDENCIA',toupper(section)))'puntos_porcentuales' else if(grepl('CRECIMIENTO|VARIACI|VAR[.]|PARTICIPACI|PONDERACION',toupper(section)))'porcentaje' else if(grepl('PIBK',s))'indice_volumen_2018_100' else 'millones_DOP'
  add(f,s,paste0('pib_origen_2018_',if(acum)'acumulado'else'trimestral'),paste(section,lab,sep=' / '),as.Date(sprintf('%04d-%02d-01',yr[cc],(q[cc]-1L)*3L+1L)),val[cc],unit,r,cc,scale=if(is_share)100 else 1,frequency=if(acum)'trimestral_acumulada'else'trimestral',note='Serie retrospectiva oficial. Conservar unidad, sección y frecuencia; los índices encadenados no son aditivos. Ponderación convertida de fracción a porcentaje.')
 }
}
z<-rbindlist(out,fill=TRUE)
stopifnot(nrow(z)>10000,!anyDuplicated(z[,.(fuente_id,hoja,fila_excel,columna_excel)]),all(is.finite(z$valor)),max(z$periodo)<=Sys.Date())
z[,fila_origen:=.I]
con<-init_database()
if(dbExistsTable(con,Id(schema='bcrd',table='series'))){
 old<-dbGetQuery(con,'SELECT * FROM bcrd.series')
 # Same downloaded source cells must retain exactly their original values.
 comparison<-merge(old,z,by=c('fuente_id','hoja','fila_excel','columna_excel'))
 removed<-as.data.table(old)[!z,on=.(fuente_id,hoja,fila_excel,columna_excel)]
 stopifnot(all(comparison$valor_fuente.x==comparison$valor_fuente.y),all(removed$fuente_id=='e1d15befa0ba' & removed$valor_fuente>=2007 & removed$valor_fuente<=2026))
 if(nrow(old)!=nrow(z)||any(comparison$periodo.x!=comparison$periodo.y)||any(comparison$serie.x!=comparison$serie.y)||any(comparison$dataset.x!=comparison$dataset.y)||any(comparison$unidad.x!=comparison$unidad.y))dbWithTransaction(con,{
  dbExecute(con,'DELETE FROM bcrd.series')
  for(i in seq.int(1L,nrow(z),by=10000L))dbAppendTable(con,Id(schema='bcrd',table='series'),as.data.frame(z[i:min(i+9999L,nrow(z))]))
  dbExecute(con,"UPDATE meta.tablas SET filas=$1,periodo=$2 WHERE tabla='bcrd.series'",params=list(nrow(z),paste(min(z$periodo),max(z$periodo),sep=' / ')))
 })
}
load_table(con,z,'bcrd','series','BCRD','Series económicas históricas normalizadas',file.path(base,'qa/download-manifest.json'),period=paste(min(z$periodo),max(z$periodo),sep=' / '),grain='fuente, hoja, serie y período; cada celda de datos se representa una vez',notes='Fecha=primer día del mes o trimestre representado; stocks y flujos separados por dataset; nunca sumar series o bases alternativas. Remesas en USD físicos: el Excel solo muestra millones mediante formato. TPM y tasas convertidas de fracción a porcentaje.')
sources<-rbindlist(used,fill=TRUE)
sources[,fila_origen:=.I]
load_table(con,sources,'meta','archivos_bcrd','BCRD','Archivos fuente de las series BCRD',file.path(base,'qa/download-manifest.json'),grain='archivo público descargado')
dbExecute(con,'CREATE INDEX IF NOT EXISTS bcrd_series_dataset_periodo ON bcrd.series(dataset,periodo)')
dbExecute(con,'CREATE INDEX IF NOT EXISTS bcrd_series_serie ON bcrd.series(dataset,serie,periodo)')
dbExecute(con,"CREATE OR REPLACE VIEW analitica.series_economicas AS SELECT dataset,serie,periodo,frecuencia,valor,unidad,nota_periodo,fuente_id,hoja,fila_excel,columna_excel FROM bcrd.series")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.catalogo_series AS SELECT dataset,serie,frecuencia,unidad,min(periodo) AS desde,max(periodo) AS hasta,count(*) AS observaciones FROM bcrd.series GROUP BY 1,2,3,4")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.macro_mensual AS SELECT periodo,max(valor) FILTER(WHERE dataset='ipc_nacional' AND serie='Índice general') AS ipc,max(valor) FILTER(WHERE dataset='ipc_nacional' AND serie='Inflación interanual') AS inflacion_interanual,max(valor) FILTER(WHERE dataset='imae_2018' AND serie='Variación interanual original') AS imae_interanual,max(valor) FILTER(WHERE dataset='politica_monetaria' AND serie='Tasa de política monetaria') AS tpm_porcentaje,max(valor) FILTER(WHERE dataset='remesas') AS remesas_usd,max(valor) FILTER(WHERE dataset='canasta_quintiles' AND serie='Nacional') AS canasta_nacional_dop FROM bcrd.series WHERE frecuencia='mensual' GROUP BY periodo")
coverage<-dbGetQuery(con,'SELECT dataset,count(*) AS observaciones,count(distinct serie) AS series,min(periodo) AS desde,max(periodo) AS hasta FROM bcrd.series GROUP BY dataset ORDER BY dataset')
write_json(coverage,file.path(base,'qa/time-series.json'),pretty=TRUE,auto_unbox=TRUE)
print(coverage)
dbDisconnect(con)
