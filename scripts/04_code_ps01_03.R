################################################################################
# Problem Set 1 - Question 3
# Authors: Alexander Almeida-Ramírez
# Descripción: Este archivo lee los datos y estima la relación entre salarios y 
#edad. 
################################################################################

# Prepare workspace
rm (list=ls())
source("scripts/00_packages.R")
gc()

#install.packages("arrow") ya está este paquete instalado en el 00 de paquetes? 
#library(arrow)

#Cargar la base de datos 
datos_geih<-read_parquet("stores/geih.parquet")
View(datos_geih)

#install.packages("skimr") Igual con estos paquetes que detectan valores missings
#library(skimr)
#Verificar la información de edad 
db_miss <- skim(datos_geih) %>% select( skim_variable, n_missing)
Nobs= nrow(datos_geih)
db_miss<- db_miss%>% mutate(p_missing= n_missing/Nobs)

View()
