##############################################################################-
# DATE:
#   2024/feb/10
# AUTHOR:
#  Jaime Buitrago
# DESCRIPTION:
#   Merge GEIH 2018 downloaded from the ANDA
##############################################################################-

# Prepare workspace

rm (list=ls())
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Append house and household databases ----
############################################################################-

for (j in 1:12){
  if (j==1) {
  #if (exists("viviendas")==FALSE) {
  viviendas <- read.csv(paste0("stores/",j,"/Vivienda y Hogares.CSV"))
  }
  else{
  modulo <- read.csv(paste0("stores/",j,"/Vivienda y Hogares.CSV"))
  viviendas <-rbind(viviendas, modulo)
  }
}

############################################################################-
# 2. Merge and append people databases ----
############################################################################-

persona <- c("Desocupados",
              "Fuerza de trabajo",
              "Inactivos",
              "Ocupados",
              "Otras actividades y ayudas en la semana",
              "Otros ingresos"
             )

for (j in 1:12){  
  personas <- read.csv(paste0("stores/",j,"/Caracteristicas generales (personas).CSV"))
  personas <- personas%>%select(-c("OCI","DSI","INI"))

for ( i in persona) {
  modulo <- read.csv(paste0("stores/",j,"/",i,".CSV"))
  modulo <- modulo%>%select(-c("HOGAR","AREA","CLASE","FEX_C","DPTO",
                             "ANO","MES","PERIODO","REGIS"))
  personas <- merge(personas,modulo, 
              by=c("DIRECTORIO","SECUENCIA_P","ORDEN"),
              no.dups = TRUE,
              all = TRUE,
              suffixes = "")
}
  if(j==1){
    geih <- personas
    common <- colnames(personas)
  }  
  else{
    geih <- rbind(geih[common],personas[common])
  }
}

############################################################################-
# 3. One database ----
############################################################################-

viviendas <- viviendas%>%select(-c("HOGAR","AREA","CLASE","FEX_C","DPTO",
                             "ANO","MES","PERIODO","REGIS","ORDEN"))
geih <- merge(viviendas,geih, 
                    by=c("DIRECTORIO","SECUENCIA_P"),
                    no.dups = TRUE,
                    all = TRUE,
                    suffixes = "")  

arrow::write_parquet(geih, sink = "stores/geih18.parquet")