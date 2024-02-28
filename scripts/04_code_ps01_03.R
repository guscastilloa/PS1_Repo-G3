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

#Definir los posibles predictores de la base de datos: 

geih_select <- datos_geih  %>% select(y_total_m_ha, 
                                       hoursWorkUsual,
                                       age,
                                       sex,
                                       oficio,
                                       relab,
                                       college,
                                       ocu,
                                       maxEducLevel)

#install.packages("skimr") Igual con estos paquetes que detectan valores missings
#library(skimr)
#Verificar la información de edad 
db_miss <- skim(geih_select) %>% select( skim_variable, n_missing)
Nobs= nrow(geih_select)
db_miss<- db_miss%>% mutate(p_missing= n_missing/Nobs)


#Gráfico con los missings 
ggplot(tail(db_miss, 40), aes(x = reorder(skim_variable, +p_missing) , y =  p_missing)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  coord_flip() +
  labs(title = "N Missing Per Variable", x = "Var Name", y = "Missings") +
  theme(axis.text = element_text(size = 8))

#Tenemos que hay un porcentaje de 48% de valores missings en horas trabajadas y 
#en el ingreso por hora. 
install.packages("visdat")
library(visdat)
vis_miss(geih_select) #pregunta, cómo podemos hacer para imputar estos missings. 
vis_miss() #


#Quiero saber si hay correlación entre las variables missings 
db_missing <- geih_select %>% mutate_all(~ifelse(!is.na(.), 1, 0))
## drop  variables with not missing or  with all missing.

db_missing <-  db_missing %>%  select(which(apply(db_missing, 2, sd) > 0))
M <- cor(db_missing)
install.packages("corrplot")
library(corrplot)
corrplot(M) 