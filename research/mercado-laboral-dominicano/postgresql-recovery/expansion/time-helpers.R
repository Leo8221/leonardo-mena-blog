month_num<-function(z){match(substr(tolower(iconv(trimws(z),to='ASCII//TRANSLIT')),1,3),c('ene','feb','mar','abr','may','jun','jul','ago','sep','oct','nov','dic'))}
year_num<-function(z){v<-suppressWarnings(as.integer(sub('^((19|20)[0-9]{2}).*$','\\1',trimws(z))));v[is.na(v)|v<1900|v>2099]<-NA_integer_;v}
fill_down<-function(x){last<-NA;for(i in seq_along(x)){if(!is.na(x[i]))last<-x[i] else x[i]<-last};x}
number<-function(z){suppressWarnings(as.numeric(gsub(',','',trimws(z),fixed=TRUE)))}
grid_read<-function(f,s=1)as.matrix(suppressMessages(read_excel(f,sheet=s,col_names=FALSE,col_types='text',.name_repair='minimal')))
vertical_dates<-function(x,ycol=1,mcol=2,start=1L){
 mo<-month_num(x[,mcol]);mo[seq_len(start-1L)]<-NA_integer_;yy<-year_num(x[,ycol]);rr<-which(!is.na(mo));out<-as.Date(rep(NA_character_,nrow(x)))
 if(!length(rr))return(out)
 gg<-cumsum(c(TRUE,diff(mo[rr])<=0));previous<-0L;lastyear<-NA_integer_
 for(g in unique(gg)){
  rows<-rr[gg==g];yr<-unique(yy[(previous+1L):max(rows)]);yr<-yr[!is.na(yr)]
  after<-min(max(rows)+1L,nrow(x))
  if(grepl('^Promedio (19|20)[0-9]{2}$',x[after,ycol]))yr<-as.integer(sub('^Promedio ','',x[after,ycol]))
  if(length(yr)>1L)stop('Ambiguous year block')
  if(length(yr))lastyear<-yr
  if(!is.na(lastyear))out[rows]<-as.Date(sprintf('%04d-%02d-01',lastyear,mo[rows]))
  previous<-max(rows)
 }
 # Isolated December observations preceding a complete monthly series.
 for(c in unique(c(ycol,mcol))){
  z<-x[,c];r<-which(grepl('^(Dic|DIC|dic)[.]? ?(19|20)[0-9]{2}$',z))
  if(length(r))out[r]<-as.Date(paste0(sub('^.*((19|20)[0-9]{2})$','\\1',z[r]),'-12-01'))
 }
 out
}
header_date<-function(z){
 d<-number(z)
 if(!is.na(d)&&d>25000&&d<50000)return(as.Date(d,origin='1899-12-30'))
 key<-tolower(trimws(z));mo<-month_num(key)
 if(!is.na(mo)&&grepl('^[a-z]{3}-[0-9]{2}',key))return(as.Date(sprintf('%04d-%02d-01',2000+as.integer(substr(key,5,6)),mo)))
 as.Date(NA)
}
