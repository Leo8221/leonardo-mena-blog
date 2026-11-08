stopifnot(isTRUE(l10n_info()[["UTF-8"]]))
suppressPackageStartupMessages({library(readxl);library(data.table);library(jsonlite)})
make_clean_names <- function(x) make.unique(gsub("^_+|_+$","",gsub("[^a-z0-9]+","_",tolower(iconv(x,to="ASCII//TRANSLIT")))) ,sep="_")
base <- "research/mercado-laboral-dominicano/postgresql-recovery/expansion"
paths <- c(mipymes="posts/republica-en-un-grafico/2026-02-14-mipymes-rd/Encuesta-Nacional-a-las-MIPYMES-2023-Base-de-datos.xlsx",enief="posts/republica-en-un-grafico/2026-01-31-efectivo-en-la-republica-dominicana/ENIEF_Consolidado.xlsx",enae="research/mercado-laboral-dominicano/data/raw/enae_2024/base_de_datos_general_ENAE_2024.xlsx")
for(id in names(paths)) {
 x <- as.data.table(read_excel(paths[[id]],col_types="text",.name_repair="minimal"))
 nms <- make_clean_names(names(x))
 fwrite(data.table(posicion=seq_along(nms),nombre=names(x),sql=nms,nulos=vapply(x,function(z)sum(is.na(z)),integer(1))),file.path(base,"qa",paste0(id,"-columns.csv")))
 cat(id,nrow(x),ncol(x),"\n")
 cols <- grep("(^id$|^base$|factor|peso|expan|clasificacion|formalidad|^region$|_edad$|ingresos_totales)",nms)
 for(j in cols){cat(names(x)[j],": "); y<-x[[j]]; if(length(unique(y))<20) print(table(y,useNA="ifany")) else print(summary(suppressWarnings(as.numeric(y))))}
}
d <- read_excel(paths[["enae"]],sheet="Diccionario",col_names=FALSE,col_types="text",n_max=15,.name_repair="minimal")
write_json(as.matrix(d),file.path(base,"qa","enae-dictionary-preview.json"),pretty=TRUE,na="null")
