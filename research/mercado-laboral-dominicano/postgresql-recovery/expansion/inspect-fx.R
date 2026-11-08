source('research/mercado-laboral-dominicano/postgresql-recovery/expansion/common.R')
source(file.path(base,'time-helpers.R'))
f<-file.path(base,'raw/bcrd-mercado-cambiario/TASA_DOLAR_REFERENCIA_MC.xlsx')
for(s in excel_sheets(f)){x<-grid_read(f,s);cat(s,nrow(x),ncol(x),'\n');for(r in seq_len(min(8,nrow(x))))cat(r,paste(x[r,],collapse=' | '),'\n')}
