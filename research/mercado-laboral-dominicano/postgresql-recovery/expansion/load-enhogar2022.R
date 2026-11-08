source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R');library(xml2)
root<-'research/mercado-laboral-dominicano/data/raw/enhogar_2022';con<-init_database();results<-list()
for(module in c('Personas','Elegidos')){
 path<-file.path(root,paste0(module,'_ENH22.csv'));d<-fread(path,colClasses='character',encoding='Latin-1',na.strings=c('','NA'))
 labels<-names(d);names(d)<-clean_names(labels);d<-typed_text(d,keep_text=c('upm','hvivien','hhogar','hlinea','hprovi'))
 d$fila_origen<-seq_len(nrow(d))+1L
 stopifnot(!anyDuplicated(d[,c('upm','hvivien','hhogar','hlinea')]))
 factor<-if(module=='Personas')'f_expansion' else 'fexpansion10m'
 stopifnot(all(is.na(d[[factor]])|d[[factor]]>0))
 table<-tolower(module)
 results[[table]]<-load_table(con,d,'enhogar_2022',table,'ONE',paste('ENHOGAR 2022',module),path,url=paste0('https://www.one.gob.do/catalogo-datos/ENHOGAR/ENHOGAR_2022_BD_SPSS/',module,'_ENH22.csv'),period='2022',grain='persona en hogar; UPM + HVIVIEN + HHOGAR + HLINEA',notes=if(module=='Personas')'Usar factor propio F_expansión, UPM y estrato para inferencia de diseño. Encuesta independiente de ENHOGAR 2024: no enlazar personas entre años.'else'Muestra de personas elegidas: universo y factor FEXPANSION10M propios. No usar factor del módulo de personas ni convertir no respuesta en cero.',original_names=c(labels,'Fila CSV'))
 book<-file.path(root,paste0('Libro_de_codigos_ENHOGAR2022_',module,'.htm'));doc<-read_html(book,encoding='UTF-8')
 tables<-xml_find_all(doc,'//table[@border]');dictionary<-list()
 for(t in tables){
  rows<-lapply(xml_find_all(t,'./tr|./tbody/tr'),function(r)trimws(xml_text(xml_find_all(r,'./td'))))
  if(!length(rows))next
  variable<-rows[[1]][1];label<-NA_character_
  for(r in rows){if('Etiqueta' %in% r)label<-tail(r,1)}
  codes<-Filter(function(r)length(r)>=2 && grepl('^[0-9.]+$',r[length(r)-1]),rows)
  if(!length(codes))dictionary[[length(dictionary)+1L]]<-data.table(variable=variable,etiqueta=label,codigo=NA_character_,etiqueta_codigo=NA_character_)
  else for(r in codes)dictionary[[length(dictionary)+1L]]<-data.table(variable=variable,etiqueta=label,codigo=r[length(r)-1],etiqueta_codigo=tail(r,1))
 }
 dd<-rbindlist(dictionary);dd[,fila_origen:=.I]
 load_table(con,dd,'enhogar_2022',paste0('diccionario_',table),'ONE',paste('Diccionario ENHOGAR 2022',module),book,period='2022',grain='variable y código de respuesta')
 for(v in unique(dd$variable)){
  nm<-clean_names(v);lab<-dd$etiqueta[match(v,dd$variable)]
  if(nm %in% names(d)&&!is.na(lab))dbExecute(con,paste('COMMENT ON COLUMN',dbQuoteIdentifier(con,Id(schema='enhogar_2022',table=table)),'.',dbQuoteIdentifier(con,nm),'IS',dbQuoteString(con,lab)))
 }
}
dbExecute(con,"CREATE OR REPLACE VIEW analitica.enhogar_2022_personas AS SELECT 2022 AS anio,upm,hvivien AS vivienda,hhogar AS hogar,hlinea AS linea,hprovi AS codigo_provincia,hzona AS zona,p202 AS sexo,p203 AS edad,p205 AS parentesco,p401,p402,p403,p404,p405,f_expansion AS factor_expansion,estrato,fila_origen FROM enhogar_2022.personas")
write_json(results,file.path(base,'qa/enhogar2022.json'),auto_unbox=TRUE,pretty=TRUE)
dbDisconnect(con)
