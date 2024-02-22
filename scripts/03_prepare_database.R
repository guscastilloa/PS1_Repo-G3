##############################################################################-
# DATE:
#   2024/feb/10
# AUTHOR:
#  Jaime Buitrago
# DESCRIPTION:
#   Preparation of database for Problem Set 1
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Merge GEIH database and Web scarped database ----
############################################################################-

geih <- arrow::read_parquet("stores/geih18.parquet")
geih <- geih%>%filter(DPTO==11 & OCI==1 & P6040>=18)%>%
        select("DIRECTORIO","SECUENCIA_P", "ORDEN","P6430","ESC", 
                      "OCI", "RAMA4D")
names(geih) <- tolower(names(geih))

db <- arrow::read_parquet("stores/geih.parquet")
db <- db%>%filter(ocu==1 & age>=18)

db <- merge(db,geih, 
            by=c("directorio","secuencia_p","orden"),
            no.dups = TRUE,
            all = TRUE,
            suffixes = "")

db <- db %>% drop_na(dominio)

############################################################################-
# 1. Create variables ----
############################################################################-

db$exp <- db$age-db$esc-6

db$sector <- ifelse(

arrow::write_parquet(b, sink = "stores/db.parquet")