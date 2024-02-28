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

#Vamos a hacer lo mismo con los ocupados para ver si podemos reemplazar algunos 
#valores que son missings values. 
db_missing <-  db_missing %>%  select(which(apply(db_missing, 2, sd) > 0))

#Vamos a lidiar con los valores faltantes de horas trabajadas aplicándole un 
#cero a esta variable. 
geih_select <- geih_select  %>%
  mutate(totalHoursWorked = ifelse(ocu == 0, 0, hoursWorkUsual))

#Cómo vamos a lidiar con los missings que existen en los salarios? 
dummy<- geih_select %>% filter(ocu== 1)
vis_miss(geih_select) #Todavía existen un 11% de valores que son missing y que 
                #tenemos que solucionar 

#Revisando, podríamos reemplazar por ceros a las personas que no son ocupadas. 
#y eliminar los ingresos de aquellos que están ocupados. Finalmente, eliminar 
#a todos aquellos que tienen missings values mayores a cero. 
geih_select <- geih_select  %>%
  mutate(y_total_m_ha = ifelse(ocu == 0, 0, hoursWorkUsual))

#Eliminar aquellos que tienen ingresos superiores al 99% (por salarios)
geih_select<-geih_select %>% 
  mutate(y_total_m_ha= ifelse(y_total_m_ha> quantile(y_total_m_ha, .99), 
                              NA, y_total_m_ha))

#Los eliminamos debido a que hay individuos que tienen salarios muy altos, lo cual 
#puede afectar nuestras estimaciones. 

#Generar nuevas variables (edad al cuadrado) y el los salarios en logaritmo. 
geih_select<- geih_select  %>% mutate(age2=age^2)
geih_select <- geih_select  %>% mutate(ln_wage = log(y_total_m_ha))
