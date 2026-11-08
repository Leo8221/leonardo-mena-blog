source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
m<-fromJSON(file.path(base,'qa/download-manifest.json'))
f<-m$path[m$id=='30f99ffda639'];x<-read_excel(f,col_types='text');print(table(x[[6]],useNA='always'))
cat('xml2 available:',requireNamespace('xml2',quietly=TRUE),'\n')
for(id in c('a1156fb8cacb','31f8bc50c9de','613d318a6a55')){
 f<-m$path[m$id==id];z<-paste(readLines(unz(f,'xl/styles.xml'),warn=FALSE),collapse='')
 cat(id,'\n');cat(regmatches(z,gregexpr('<numFmt numFmtId[^>]+',z))[[1]],sep='\n')
}
