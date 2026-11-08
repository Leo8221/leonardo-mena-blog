source("research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R")
con<-init_database()
specs<-list(
 list(id="mipymes_2023",path="posts/republica-en-un-grafico/2026-02-14-mipymes-rd/Encuesta-Nacional-a-las-MIPYMES-2023-Base-de-datos.xlsx",title="Encuesta Nacional a las MiPyMES 2023",period="Levantamiento 2023; variables economicas y credito referidas a 2022",rows=13130L,weight="factor",url="https://www.bancentral.gov.do/a/d/5962",grain="Establecimiento encuestado",notes="Usar FACTOR. No enlazar negocios con personas ENCFT. Ingresos no son mensuales; revisar referencia anual de cada variable. 369 casos carecen de respuestas en varios modulos; no convertir ausencias en No."),
 list(id="enief",path="posts/republica-en-un-grafico/2026-01-31-efectivo-en-la-republica-dominicana/ENIEF_Consolidado.xlsx",title="ENIEF consolidada 2019 y 2023",period="2019;2023",rows=6676L,weight="factor_exp",url="https://www.bancentral.gov.do/",grain="Persona seleccionada por ronda",notes="Archivo consolidado local. Filtrar BASE antes de expandir. Los cuestionarios de ambas rondas no son identicos; no interpretar nulos estructurales como No. Etiquetas detalladas requieren diccionario de cada ronda."),
 list(id="enae_2024",path="research/mercado-laboral-dominicano/data/raw/enae_2024/base_de_datos_general_ENAE_2024.xlsx",title="Encuesta Nacional de Actividad Economica 2024",period="Levantamiento 2024; revisar periodo fiscal de cada variable",rows=1225L,weight="factor_expansion",url="https://www.one.gob.do/media/z4ag3kyr/base-de-datos-general-encuesta-nacional-de-actividad-econ%C3%B3mica-2024.xlsx",grain="Empresa encuestada",notes="Usar factor_expansion; variables _imp incluyen imputaciones del productor. No sumar flujos monetarios de diferentes conceptos; conservar cuestionario y diccionario.")
)
results<-list()
for(s in specs){
  df<-as.data.frame(read_excel(s$path,col_types="text",.name_repair="minimal"))
  original<-names(df)
  nms<-clean_names(original)
  if(s$id=="mipymes_2023"){
    coded<-grepl("^P[0-9]+_[0-9]+",original)
    nms[coded]<-tolower(sub("^([A-Z0-9_]+).*","\\1",original[coded]))
  }
  names(df)<-nms
  stopifnot(nrow(df)==s$rows,!anyDuplicated(nms))
  df<-typed_text(df)
  stopifnot(all(is.finite(df[[s$weight]])&df[[s$weight]]>0))
  if(s$id=="mipymes_2023")stopifnot(!anyDuplicated(df$id))
  df<-cbind(fila_origen=seq_len(nrow(df)),df)
  results[[s$id]]<-load_table(con,df,s$id,"microdatos",s$id,s$title,s$path,excel_sheets(s$path)[1],s$url,s$period,s$grain,s$notes,c("Fila fisica original",original))
}
dict_path<-specs[[3]]$path
d<-as.data.frame(read_excel(dict_path,sheet="Diccionario",skip=7,col_types="text",.name_repair="minimal"))
names(d)<-c("variable","etiqueta_variable","tipo_variable","valor","etiqueta_valor")
for(nm in c("variable","etiqueta_variable","tipo_variable")){
  last<-NA_character_;for(i in seq_len(nrow(d))){if(!is.na(d[[nm]][i]))last<-d[[nm]][i] else d[[nm]][i]<-last}
}
d<-cbind(fila_origen=seq_len(nrow(d)),d)
load_table(con,d,"enae_2024","diccionario","enae_2024","Diccionario ENAE 2024",dict_path,"Diccionario",period="2024",grain="Variable y categoria de respuesta")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.mipymes AS SELECT fila_origen,id,region,provincia,rama_de_actividad,clasificacion_mipymes,formalidad,ingresos_totales,costos_y_gastos_operacionales,costo_total,empleo_total,empleo_hombre,empleo_mujer,p7_1 AS obtuvo_credito_2022,p7_6 AS cuenta_bancaria_2022,factor AS factor_expansion FROM mipymes_2023.microdatos")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.mipymes_resumen AS SELECT region,clasificacion_mipymes,formalidad,count(*) AS muestra,sum(factor_expansion) AS establecimientos_estimados,sum(empleo_total*factor_expansion) AS empleo_estimado,sum(ingresos_totales*factor_expansion) AS ingresos_estimados_2022 FROM analitica.mipymes GROUP BY region,clasificacion_mipymes,formalidad")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.enief AS SELECT substring(base from '[0-9]{4}')::integer AS anio_encuesta,m.* FROM enief.microdatos m")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.enae AS SELECT * FROM enae_2024.microdatos")
dbExecute(con,"CREATE INDEX IF NOT EXISTS enief_base_idx ON enief.microdatos(base)")
dbExecute(con,"CREATE INDEX IF NOT EXISTS mipymes_region_rama_idx ON mipymes_2023.microdatos(region,rama_de_actividad)")
print(dbGetQuery(con,"SELECT base,count(*) AS muestra,sum(factor_exp) AS expansion FROM enief.microdatos GROUP BY base ORDER BY base"))
print(dbGetQuery(con,"SELECT round(sum(factor)) AS establecimientos,round(sum(factor*empleo_total)) AS empleo FROM mipymes_2023.microdatos"))
write_json(results,file.path(base,"qa","loaded-surveys.json"),pretty=TRUE,auto_unbox=TRUE)
dbDisconnect(con)
