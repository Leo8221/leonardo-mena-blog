source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
source(file.path(base,'time-helpers.R'))
library(xml2)
m<-fromJSON(file.path(base,'qa/download-manifest.json'))
m<-m[m$group=='digepres' & grepl('Mensual',m$path,ignore.case=TRUE),]
out<-list()
issues<-list()
used<-list()
reconciliations<-list()
rowchecks<-list()
read_xml_zip<-function(path,member){h<-unz(path,member);on.exit(close(h));read_xml(paste(readLines(h,warn=FALSE),collapse=''))}
for(i in seq_len(nrow(m))){
 f<-m[i,]
 styles<-read_xml_zip(f$path,'xl/styles.xml')
 ns<-xml_ns(styles)
 fm<-xml_find_all(styles,'.//d1:numFmt',ns)
 fmt<-setNames(xml_attr(fm,'formatCode'),xml_attr(fm,'numFmtId'))
 xf<-xml_find_all(styles,'.//d1:cellXfs/d1:xf',ns)
 stylecodes<-fmt[xml_attr(xf,'numFmtId')]
 wb<-read_xml_zip(f$path,'xl/workbook.xml')
 rels<-read_xml_zip(f$path,'xl/_rels/workbook.xml.rels')
 wn<-xml_find_all(wb,'.//*[local-name()="sheet"]')
 rn<-xml_find_all(rels,'.//*[local-name()="Relationship"]')
 targets<-setNames(xml_attr(rn,'Target'),xml_attr(rn,'Id'))
 for(s in excel_sheets(f$path)){
  yy<-year_num(s)
  if(is.na(yy))next
  x<-as.matrix(suppressMessages(read_excel(f$path,sheet=s,range=cell_limits(c(1,1),c(NA,NA)),col_names=FALSE,col_types='text',.name_repair='minimal')))
  scores<-apply(x[seq_len(min(25,nrow(x))),,drop=FALSE],1,function(z)sum(!is.na(month_num(z))))
  h<-which.max(scores);if(scores[h]<3){issues[[length(issues)+1L]]<-list(file=f$id,sheet=s,reason='No monthly header');next}
  months<-month_num(x[h,])
  cc<-which(!is.na(months))
  candidates<-seq_len(min(cc)-1L)
  labelcol<-candidates[which.max(vapply(candidates,function(k)sum(!is.na(x[(h+1L):nrow(x),k])&is.na(number(x[(h+1L):nrow(x),k]))),integer(1)))]
  rr<-which(seq_len(nrow(x))>h & !is.na(x[,labelcol]))
  rr<-rr[!grepl('^(Fuente|Nota|[*(]|[0-9]+/)',trimws(x[rr,labelcol]),ignore.case=TRUE)]
  sheetnode<-wn[xml_attr(wn,'name')==s]
  rid<-xml_text(xml_find_first(sheetnode,'./@*[local-name()="id"]'))
  target<-targets[[rid]]
  member<-if(startsWith(target,'/'))substring(target,2)else paste0('xl/',target)
  sx<-read_xml_zip(f$path,member)
  cells<-xml_find_all(sx,'.//*[local-name()="sheetData"]//*[local-name()="c"]')
  formats<-setNames(stylecodes[as.integer(xml_attr(cells,'s'))+1L],xml_attr(cells,'r'))
  title<-paste(na.omit(as.vector(x[1:(h-1L),])),collapse=' | ')
  cut<-12L
  if(yy==2026L){
   tokens<-unlist(strsplit(tolower(iconv(title,to='ASCII//TRANSLIT')),'[^a-z]+'))
   mn<-month_num(tokens)
   if(any(!is.na(mn)))cut<-max(mn,na.rm=TRUE)
   cut<-min(cut,as.integer(format(Sys.Date(),'%m')))
  }
  for(c in cc[months[cc]<=cut]){
   values<-number(x[rr,c])
   good<-!is.na(values)
   r<-rr[good]
   values<-values[good]
   if(!length(r))next
   # Source display often divides pesos by a million through the Excel format.
   letter<-function(k){z<-'';while(k>0){k<-k-1L;z<-paste0(LETTERS[k%%26L+1L],z);k<-k%/%26L};z}
   display<-formats[paste0(letter(c),r)]
   scaled<-grepl(',,',display,fixed=TRUE)
   unit<-ifelse(scaled,'DOP',if(grepl('millones',title,ignore.case=TRUE))'millones_DOP'else'por_verificar')
   unit[is.na(display)]<-'por_verificar'
   # A source formatting defect: Jan-2010 TOTAL GASTOS uses an unscaled display
   # format, while its raw amount and the published annual sum are in pesos.
   corrected<-f$id=='a1156fb8cacb' & yy==2010L & months[c]==1L & trimws(x[r,labelcol])=='TOTAL GASTOS'
   unit[corrected]<-'DOP'
   # Never infer a peso/million conversion from the magnitude alone.
   dop<-ifelse(unit=='DOP',values,ifelse(unit=='millones_DOP',values*1e6,NA_real_))
   label<-trimws(x[r,labelcol])
   code<-ifelse(grepl('^[0-9.]+ ?-',label),sub('^([0-9.]+) ?-.*$','\\1',label),NA_character_)
   out[[length(out)+1L]]<-data.table(fuente_id=f$id,archivo=basename(f$path),hoja=s,titulo=title,periodo=as.Date(sprintf('%04d-%02d-01',yy,months[c])),codigo=code,concepto=label,valor_dop=dop,valor_fuente=values,unidad_fuente=unit,formato_excel=unname(display),fila_excel=r,columna_excel=c,es_total=grepl('total',label,ignore.case=TRUE))
  }
  # Independently compare the sum of monthly raw cells with the published total.
  totals<-which(grepl('^TOTAL$',trimws(x[h,]),ignore.case=TRUE))
  if(length(totals)==1){
   monthlycols<-cc[months[cc]<=cut]
   valueset<-x[rr,monthlycols,drop=FALSE]
   mat<-matrix(number(valueset),nrow=nrow(valueset),ncol=ncol(valueset))
   to_dop<-function(vals,cols){
    for(j in seq_along(cols)){
     fm<-formats[paste0(letter(cols[j]),rr)]
     mult<-ifelse(grepl(',,',fm,fixed=TRUE),1,if(grepl('millones',title,ignore.case=TRUE))1e6 else NA_real_)
     mult[is.na(fm)]<-NA_real_
     if(f$id=='a1156fb8cacb'&&yy==2010L&&cols[j]==cc[months[cc]==1L])mult[trimws(x[rr,labelcol])=='TOTAL GASTOS']<-1
     vals[,j]<-vals[,j]*mult
    };vals
   }
   normalized<-to_dop(mat,monthlycols)
   sums<-rowSums(normalized,na.rm=TRUE)
   published<-as.vector(to_dop(matrix(number(x[rr,totals]),ncol=1),totals))
   valid<-!is.na(published)&rowSums(!is.na(normalized))>0
   differences<-abs(sums[valid]-published[valid])>pmax(1,abs(published[valid]))*1e-7
   reconciliations[[length(reconciliations)+1L]]<-data.table(fuente_id=f$id,hoja=s,filas_comparadas=sum(valid),diferencias=sum(differences),max_diferencia=if(any(valid))max(abs(sums[valid]-published[valid]))else 0)
   rowchecks[[length(rowchecks)+1L]]<-data.table(fuente_id=f$id,hoja=s,fila_excel=rr[valid],concepto=x[rr[valid],labelcol],suma_meses_dop=sums[valid],total_publicado_dop=published[valid],estado=ifelse(differences,'diferencia','conciliado'))
  }
  used[[f$id]]<-f
 }
 cat('Prepared fiscal',i,'/',nrow(m),'\n')
}
z<-rbindlist(out,fill=TRUE)
stopifnot(nrow(z)>10000,!anyDuplicated(z[,.(fuente_id,hoja,fila_excel,columna_excel)]))
z[,fila_origen:=.I]
con<-init_database()
if(dbExistsTable(con,Id(schema='digepres',table='ejecucion_mensual'))){
 dbExecute(con,"UPDATE digepres.ejecucion_mensual SET valor_dop=valor_fuente,unidad_fuente='DOP' WHERE fuente_id='a1156fb8cacb' AND hoja='2010' AND concepto='TOTAL GASTOS' AND periodo=DATE '2010-01-01'")
}
load_table(con,z,'digepres','ejecucion_mensual','DIGEPRES','Ejecución presupuestaria histórica por clasificadores',file.path(base,'qa/download-manifest.json'),grain='clasificador, concepto y mes; incluye subtotales de distintos niveles',period=paste(min(z$periodo),max(z$periodo),sep=' / '),notes='No sumar clasificadores alternativos ni padres con hijos. Valor_DOP usa el formato real de la celda para resolver pesos versus millones. No se mezclan presupuesto inicial/vigente con ejecución. Ceros de meses futuros se excluyen según fecha de corte del encabezado.')
sources<-rbindlist(used)
sources[,fila_origen:=.I]
load_table(con,sources,'meta','archivos_digepres','DIGEPRES','Archivos de ejecución fiscal',file.path(base,'qa/download-manifest.json'))
rc<-rbindlist(rowchecks)
rc[,fila_origen:=.I]
load_table(con,rc,'digepres','conciliacion_filas','DIGEPRES','Conciliación de meses y total publicado por fila',file.path(base,'qa/download-manifest.json'),grain='archivo, hoja y fila del clasificador',notes='Las diferencias pueden ser del origen o de la interpretación de unidades; se excluyen de la vista analítica predeterminada sin modificar la fuente.')
dbExecute(con,'CREATE INDEX IF NOT EXISTS fiscal_concepto_periodo ON digepres.ejecucion_mensual(fuente_id,codigo,periodo)')
dbExecute(con,"CREATE OR REPLACE VIEW analitica.presupuesto_mensual AS SELECT e.fuente_id,e.archivo,e.periodo,e.codigo,e.concepto,e.valor_dop,e.valor_dop/1e6 AS millones_dop,e.es_total,e.hoja,e.fila_excel FROM digepres.ejecucion_mensual e WHERE e.valor_dop IS NOT NULL AND NOT EXISTS(SELECT 1 FROM digepres.conciliacion_filas c WHERE c.fuente_id=e.fuente_id AND c.hoja=e.hoja AND c.fila_excel=e.fila_excel AND c.estado='diferencia')")
qa<-list(rows=nrow(z),sources=nrow(sources),unknown_unit=sum(is.na(z$valor_dop)),units=z[,.N,by=unidad_fuente],coverage=z[,.(desde=min(periodo),hasta=max(periodo),observaciones=.N),by=.(fuente_id,archivo)],reconciliation=rbindlist(reconciliations),skipped=issues)
write_json(qa,file.path(base,'qa/fiscal.json'),auto_unbox=TRUE,pretty=TRUE,na='null')
cat('Fiscal rows',nrow(z),'unknown units',sum(is.na(z$valor_dop)),'\n')
dbDisconnect(con)
