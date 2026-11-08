source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
m<-fromJSON(file.path(base,'qa/download-manifest.json')); con<-init_database(); results<-list(); checks<-list()
spec<-list(
 list('e9f2e8149cb1','centros_2021',0L,'2021; ciclo escolar no precisado'),
 list('45e2d34289a9','centros_2022_2024',0L,'2022-2023 y 2023-2024'),
 list('563f7ac6727d','centros_2024_2025',1L,'2024-2025'),
 list('30f99ffda639','matricula_nivel',0L,'2015-2016 a 2023-2024'),
 list('7033ed20a71b','matricula_sexo',0L,'2015-2016 a 2020-2021'),
 list('efda8ea2087a','matricula_sector',0L,'2015-2016 a 2023-2024'),
 list('69a2c4d681f0','matricula_edad_2015_2021',0L,'2015-2016 a 2020-2021'),
 list('94cb6fa664e1','matricula_edad_2021_2024',0L,'2021-2022 a 2023-2024'),
 list('eecbbc68ac0c','matricula_nacionalidad',0L,'2015-2016 a 2020-2021'),
 list('bbc38646825d','matricula_detalle_2022_2023',1L,'2022-2023'),
 list('940505a5cc73','matricula_detalle_2023_2024',1L,'2023-2024'),
 list('efa0f2e71687','pruebas_nacionales',0L,'2016-2024')
)
for(s in spec){
 f<-m[m$id==s[[1]],];stopifnot(nrow(f)==1,f$status!='failed')
 grid<-suppressMessages(read_excel(f$path,sheet=1,col_names=FALSE,col_types='text',.name_repair='minimal'))
 header<-s[[3]]+1L
 x<-as.data.frame(grid[(header+1L):nrow(grid),,drop=FALSE]);names(x)<-unlist(grid[header,],use.names=FALSE)
 labels<-names(x);names(x)<-clean_names(labels)
 x$fila_origen<-seq_len(nrow(x))+s[[3]]+1L
 keep<-rowSums(!is.na(x[,setdiff(names(x),'fila_origen'),drop=FALSE]))>0
 # Repeated table headings/notes and blank formatted rows are not observations.
 pc<-intersect(c('periodo','ano_escolar','ano'),names(x))
 if(length(pc))keep<-keep & (grepl('^(19|20)[0-9]{2}([ -]?[0-9]{4})?$',x[[pc[1]]]) | x[[pc[1]]]=='202120222')
 x<-x[keep,,drop=FALSE]
 # Keep geographic and institution codes textual, including leading zeros.
 x<-typed_text(x,keep_text=c('regional','distrito','codigo_de_centro','edad','periodo','ano','ano_escolar'))
 labels<-c(labels,'Fila de la región utilizada del Excel, incluyendo encabezados')
 results[[s[[2]]]]<-load_table(con,x,'minerd',s[[2]],'MINERD',s[[2]],f$path,sheet=excel_sheets(f$path)[1],url=f$url,period=s[[4]],grain=if(s[[2]]=='pruebas_nacionales')'centro de pruebas, año, convocatoria y nivel/modalidad' else if(s[[2]]=='centros_2024_2025')'centro, tanda, nivel y modalidad' else 'fila de la tabulación; conservar el nivel geográfico y las dimensiones',notes='No sumar tabulaciones alternativas. Códigos de centros de matrícula y pruebas no son equivalentes sin tabla de correspondencia. Ausencias no equivalen a cero.',original_names=labels)
 checks[[s[[2]]]]<-list(rows=nrow(x),excluded_nonobservations=sum(!keep),columns=names(x))
}
dbExecute(con,paste(readLines(file.path(base,'education-views.sql'),encoding='UTF-8'),collapse='\n'),immediate=TRUE)
dbExecute(con,"CREATE OR REPLACE VIEW analitica.educacion_oferta_2024_2025 AS SELECT '2024-2025'::text AS periodo_escolar,substring(regional from '^[0-9]+') AS codigo_regional,regional,substring(distrito from '^[0-9]+') AS codigo_distrito,distrito,sector,substring(planta_fisica from '^[0-9]+') AS codigo_planta,planta_fisica,substring(centro_educativo from '^[0-9]+') AS codigo_centro,centro_educativo,tanda,nivel,modalidad,matricula,fila_origen FROM minerd.centros_2024_2025")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.educacion_centros_2024_2025 AS SELECT periodo_escolar,codigo_regional,regional,codigo_distrito,distrito,sector,codigo_centro,centro_educativo,sum(matricula) AS matricula,count(*) AS combinaciones_oferta FROM analitica.educacion_oferta_2024_2025 GROUP BY 1,2,3,4,5,6,7,8")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.educacion_matricula_nivel AS SELECT substring(periodo from 1 for 4)::integer AS anio_inicio,CASE WHEN periodo='202120222' THEN '2021-2022' ELSE substring(periodo from 1 for 4)||'-'||right(periodo,4) END AS periodo_escolar,substring(regional_distrito from '^[0-9]+') AS codigo_territorio,CASE WHEN length(substring(regional_distrito from '^[0-9]+'))=2 THEN 'regional' ELSE 'distrito' END AS nivel_geografico,regional_distrito AS territorio,v.nivel,v.estudiantes,fila_origen FROM minerd.matricula_nivel CROSS JOIN LATERAL (VALUES ('Inicial',nivel_inicial),('Primario',nivel_primario),('Secundario',nivel_secundario),('Adultos',educacion_de_adultos)) v(nivel,estudiantes)")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.educacion_matricula_sector AS SELECT substring(periodo from 1 for 4)::integer AS anio_inicio,substring(periodo from 1 for 4)||'-'||right(periodo,4) AS periodo_escolar,substring(regional from '^[0-9]+') AS codigo_territorio,CASE WHEN length(substring(regional from '^[0-9]+'))=2 THEN 'regional' ELSE 'distrito' END AS nivel_geografico,regional AS territorio,v.sector,v.estudiantes,fila_origen FROM minerd.matricula_sector CROSS JOIN LATERAL (VALUES ('Público',sector_publico),('Privado',sector_privado),('Semioficial',sector_semioficial)) v(sector,estudiantes)")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.educacion_matricula_sexo AS SELECT substring(periodo from 1 for 4)::integer AS anio_inicio,substring(periodo from 1 for 4)||'-'||right(periodo,4) AS periodo_escolar,substring(regional_distrito from '^[0-9]+') AS codigo_territorio,CASE WHEN length(substring(regional_distrito from '^[0-9]+'))=2 THEN 'regional' ELSE 'distrito' END AS nivel_geografico,regional_distrito AS territorio,v.sexo,v.estudiantes,fila_origen FROM minerd.matricula_sexo CROSS JOIN LATERAL (VALUES ('Masculino',masculino),('Femenino',femenino)) v(sexo,estudiantes)")
dbExecute(con,"CREATE OR REPLACE VIEW analitica.educacion_matricula_detalle AS SELECT '2022-2023'::text AS periodo_escolar,* FROM minerd.matricula_detalle_2022_2023 UNION ALL SELECT '2023-2024'::text,* FROM minerd.matricula_detalle_2023_2024")
checks$pruebas<-dbGetQuery(con,"SELECT anio,count(*) AS registros,sum(estudiantes) AS participaciones,sum((estudiantes<>mujeres+hombres)::int) AS diferencia_sexo,sum((estudiantes<>promovidos+aplazados)::int) AS diferencia_estado FROM analitica.educacion_pruebas GROUP BY anio ORDER BY anio")
checks$matricula<-dbGetQuery(con,"SELECT periodo_escolar,nivel_geografico,count(*) registros,sum(estudiantes) AS estudiantes FROM analitica.educacion_matricula_nivel GROUP BY 1,2 ORDER BY 1,2")
checks$oferta<-dbGetQuery(con,"SELECT count(*) AS registros,count(distinct codigo_centro) AS centros,sum(matricula) AS matricula FROM analitica.educacion_oferta_2024_2025")
write_json(list(loaded=results,checks=checks),file.path(base,'qa/education.json'),auto_unbox=TRUE,pretty=TRUE,na='null')
print(checks$pruebas);print(checks$matricula);print(checks$oferta)
dbDisconnect(con)
