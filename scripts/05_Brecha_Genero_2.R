###############################################################################-
# Problem Set 1 - Question 3
# Authors: Jorge Luis Congacha Yunda
# Descripción: Brecha salarial
###############################################################################-
###############################################################################
# Prepare workspace
rm (list=ls())
source("scripts/00_packages.R")
gc()

#Cargar la base de datos 
datos_geih<-read_parquet("stores/geih.parquet")
db <- read_parquet("stores/db.parquet")
#View(datos_geih)

###############################################################################-
# 1. Preparar base y tratar valores faltantes ------
###############################################################################-

# 1.1 Selección de observaciones 
#Definir los posibles predictores de la base de datos: 

geih_select <- db %>% select(ln_wage, y_total_m_ha,
                                      y_ingLab_m_ha,
                                      hoursWorkUsual,
                                      age,
                                      agesqr,
                                      sex,
                                      female,
                                      oficio,
                                      relab,
                                      college,
                                      ocu,
                                      esc,
                                      formal,
                                      microEmpresa,
                                      p6050,
                                      p6210)


#install.packages("skimr") Igual con estos paquetes que detectan valores missings
library(skimr)
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
#install.packages("visdat")
library(visdat)
vis_miss(geih_select) #pregunta, cómo podemos hacer para imputar estos missings. 
#vis_miss() #


#Quiero saber si hay correlación entre las variables missings 
db_missing <- geih_select %>% mutate_all(~ifelse(!is.na(.), 1, 0))
## drop  variables with not missing or  with all missing.

db_missing <-  db_missing %>%  select(which(apply(db_missing, 2, sd) > 0))
M <- cor(db_missing)
#install.packages("corrplot")
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
  mutate(y_ingLab_m_ha= ifelse(y_ingLab_m_ha> quantile(y_ingLab_m_ha, 
                                                       .99, na.rm = TRUE), 
                               NA, y_ingLab_m_ha))

#Los eliminamos debido a que hay individuos que tienen salarios muy altos, lo cual 
#puede afectar nuestras estimaciones. 

##############################################################################-
# 2. Estimaciones ------------------
##############################################################################-


stargazer(data.frame(datos_geih), header=FALSE, type='text', title="Variables incluidas")

# Por ahora eliminé filas con valores faltantes o no finitos en ln_wage
geih_select <- geih_select[complete.cases(geih_select$ln_wage) & 
                             is.finite(geih_select$ln_wage), ]

#vamos a codificar nuestra variable sexo. Vamos a dejar a las mujeres con 1.
geih_select$sex <- ifelse(geih_select$sex == 1, 0, 1)
table(geih_select$sex)

## A) Estimación de la brecha salarial incondicional -----
# Log(w)= β1 + β2Female + u
reg1 <- lm(ln_wage ~ sex, data = geih_select)
summary(reg1)

stargazer(reg1, digits=3, align=TRUE, type="latex", out="views/4reg1.tex" ,
          omit.stat = c("adj.rsq", "f", "ser"))

## B) Estimación de brecha salarial condicional  ---------
# incorporando variables de control como características similares de trabajadores y puestos de trabajo.

reg2<-lm(ln_wage ~ sex+age+posicion+oficio+formal+microEmpresa+
           esc, data = geih_select)
stargazer(reg2,type="text",digits=3 , omit.stat = c("adj.rsq", "f", "ser")) 
summary(reg2)

# Estimamos la brecha salarial usando:

# Eliminación de una fila con valores faltantes en todas las variables utilizadas en el modelo
v <- c("sex", "posicion", "oficio", "formal", 
       "microEmpresa", "esc", "age")
geih_select <- geih_select[complete.cases(geih_select[, v]), ]

### i) Usamos FWL -----------------------------------------------

geih_select<- geih_select %>% 
  mutate(woman_res=
           lm(as.numeric(sex) ~ posicion+oficio+formal+microEmpresa+
                                esc+age, geih_select)$residuals) #obtenemos los residuales de sex~x
geih_select<- geih_select %>% 
  mutate(log_wage_res=
           lm(ln_wage ~ posicion+oficio+formal+microEmpresa+
                        esc+age, geih_select)$residuals) #obtenemos los residuales de logy~x

# Corremos una regresión con las anteriores dos regresiones

reg3<- lm(log_wage_res~woman_res, geih_select)
stargazer(reg3,type="text",digits=3 ) 
stargazer(reg3, reg1 , digits=3, align=TRUE, type="latex",
          out="views/4reg3.tex" , omit.stat = c("adj.rsq", "f", "ser"))

### ii) FWL con boostrap ---------------------------------------

btrap<-function(data,index){
  data<-data %>% 
    mutate(woman_res=lm(as.numeric(sex)~posicion+oficio+formal+
                          microEmpresa+esc+age, geih_select)$residuals, #obtenemos los residuales de sex~x
           log_wage_res=lm(ln_wage~posicion+oficio+formal+
                             microEmpresa+esc+age, geih_select)$residuals) #obtenemos los residuales de logy~x
  coef(lm(log_wage_res~woman_res, data = data, subset = index))[2]  # nos suministra el segundo coeficiente de la regresión lineal 
}

btrap(geih_select,1:nrow(geih_select))

boot(geih_select, btrap, R = 2000) 

## C) Gráficamos la predición salario-edad, y estimación de las edades donde se
# logra el mayor slario con los respectivos intervalaos de confianza por género. 
#- estimacion de los coeficientes 

mod0 <- lm(ln_wage ~ age + I(age^2), data = geih_select, subset = (sex == 0))
mod1 <- lm(ln_wage ~ age + I(age^2), data = geih_select, subset = (sex == 1))

## Realizamos el gráfico usando los coeficientes estimados. 
# Con esto generamos un gráfico de ingresos en funcion de la edad. 

age_seq <- seq(min(geih_select$age), max(geih_select$age), by = 1)  # edades

predic0 <- predict(mod0, newdata = data.frame(edad=age_seq))  # Predicciones
predic1 <- predict(mod1, newdata = data.frame(edad=age_seq))  # Predicciones

geih_select_0 <- geih_select[geih_select$sex == 0,]
geih_select_1 <- geih_select[geih_select$sex == 1,]

peak_ages<-function(data,index){
  coefficients <- coef(lm(ln_wage ~ age + I(age^2), data = data, subset = index))
  coef2 <- coefficients[2]
  coef3 <- coefficients[3]
  coef_peak <- -(coef2/(2*coef3))
  return(coef_peak)
}

#Para los hombres
peak_ages(geih_select_0,1:nrow(geih_select_0))
val0 <- boot(geih_select_0, peak_ages, R = 2000) 
val0
quantile(val0$t[,1], 0.025)
quantile(val0$t[,1], 0.975)

#Para las mujeres
peak_ages(geih_select_1,1:nrow(geih_select_1))
val1 <- boot(geih_select_1, peak_ages, R = 2000) 
val1
quantile(val1$t[,1], 0.025)
quantile(val1$t[,1], 0.975)

# Definimos la ruta para gurdar nuestro gráfico
graph_peak <- file.path("../views", "earnings_peak_ages.png")
# gráfico
png(filename = graph_peak, width = 800, height = 600 )

# Plot del perfil de ingresos
plot(geih_select$age, (geih_select$ln_wage), 
     xlab = "Edad", ylab = "log(Ingresos)", 
     col = alpha("grey", 0.8), main = "Ingresos vs. Edad por sexo", 
     xlim = c(10, 80), ylim = c(5, 13),  # Establecer rango y frecuencia en ejes X e Y
     xaxp = c(10, 80, 7), yaxp = c(5, 13, 8))  # Establecer frecuencia en ejes X e Y
lines(age_seq, predic0, col = "red", lwd = 1)  # Línea de predicciones para Hombres
lines(age_seq, predic1, col = "blue", lwd = 1)  # Línea de predicciones para Mujeres
# Colocamos líneas verticales
abline(v = 42.64015, col = "red", lwd = 2, lty = 1)  # Línea discontinua para el valor central Hombres
abline(v = quantile(val0$t[,1], 0.025), col = "red", lwd = 2, lty = 3) 
abline(v = quantile(val0$t[,1], 0.975), col = "red", lwd = 2, lty = 3) 
abline(v = 47.96018, col = "blue", lwd = 2, lty = 1)  # Línea discontinua para el valor central Mujeres
abline(v = quantile(val1$t[,1], 0.025), col = "blue", lwd = 2, lty = 3) 
abline(v = quantile(val1$t[,1], 0.975), col = "blue", lwd = 2, lty = 3) 
# Colocamos leyenda
legend("topright",                    # posición de la leyenda
       legend = c("Mujer", "Hombre"), # etiquetas
       col = c("blue", "red"),        # colores de las líneas en la leyenda
       lwd = 1,                       # grosor de las líneas en la leyenda
       bg = "white")                  # color de fondo de la leyenda
# Agregar pie de página
#mtext("Lineas verticales continuas: Peak ages para cada sexo. Lineas verticales punteadas: Intervalos de confianza.", side = 1, line = 3.8, adj = 0, cex = 0.6)

# Agregar etiquetas para las líneas rojas y azules
text(42.64015, 5, "42,6", pos = 3)  # Etiqueta para línea roja
text(47.96018, 5, "47,9", pos = 3)  # Etiqueta para línea azul

# cerrar el dispositivo gráfico PNG
dev.off()

colnames(geih_select)
# summarytools::freq(geih_select$oficio, cumul=FALSE)

arrow::write_parquet(geih_select, sink = "stores/geih_select.parquet")

