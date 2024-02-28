################################################################################
# Problem Set 1 - Question 3
# Authors: Jorge Luis Congacha Yunda
# Descripción: Brecha salarial
################################################################################

# Prepare workspace
rm (list=ls())
source("scripts/00_packages.R")
gc()

#install.packages("arrow") ya está este paquete instalado en el 00 de paquetes? 
#library(arrow)

#Cargar la base de datos 
datos_geih<-read_parquet("stores/geih18.parquet")
