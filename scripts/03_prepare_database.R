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
                      "OCI", "RAMA2D","MES")
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
# 1. Create and label variables ----
############################################################################-

db$exp <- db$age-db$esc-6

db <- db %>%mutate(sector=case_when(rama2d<10 ~1, 
               rama2d>=10 & rama2d<15 ~2,
               rama2d>=15 & rama2d<40 ~3,
               rama2d>=40 & rama2d<45 ~4,
               rama2d>=45 & rama2d<50 ~5,
               rama2d>=50 & rama2d<60 ~6,
               rama2d>=60 & rama2d<65 ~7,
               rama2d>=65 & rama2d<70 ~8,
               rama2d>=70 & rama2d<75 ~9,
               rama2d>=75 ~10
)
)

db$sector <- factor(db$sector,
                    levels = c(1,2,3,4,5,6,7,8,9,10),
                    labels = c("Agricultura, ganadería, caza, silvicultura y pesca",
                               "Explotación de Minas y Canteras",
                               "Industria manufacturera",
                               "Suministro de Electricidad Gas y Agua",
                               "Construcción",
                               "Comercio, hoteles y restaurantes",
                               "Transporte, almacenamiento y comunicaciones",
                               "Intermediación financiera",
                               "Actividades inmobiliarias, empresariales y de alquiler",
                               "Servicios comunales, sociales y personales")
                    )

db$p6430 <- factor(db$p6430,
                    levels = c(1,2,3,4,5,6,7,8,9),
                    labels = c("Obrero, empleado particular",
                               "Obrero, empleado del gobierno",
                               "Empleado doméstico", 
                               "Trabajador por cuenta propia", 
                               "Patrón o empleador",
                               "Trabajador familiar sin remuneración",
                               "Trabajador sin remuneración en empresas de otros hogares",
                               "Jornalero o Peón",
                               "Otro")
                    )

arrow::write_parquet(db, sink = "stores/db.parquet")