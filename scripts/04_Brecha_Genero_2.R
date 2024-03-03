################################################################################
# Problem Set 1 - Question 4
# Authors: Jorge Luis Congacha Yunda
# Descripción: Brecha salarial
################################################################################

# Prepare workspace
rm (list=ls())
source("scripts/00_packages.R")
gc()

#Cargar la base de datos 
datos_geih<-read_parquet("stores/geih.parquet")
View(datos_geih)

################################################################################
# 1. limpieza de base de datos.
# 1.1 Selección de observaciones 

#################################################################################
#Definir los posibles predictores de la base de datos: 

geih_select <- datos_geih %>% select(y_total_m_ha, y_ingLab_m_ha,
                                      hoursWorkUsual,age,
                                      sex,oficio,
                                      relab,college,
                                      ocu,maxEducLevel,
                                      formal,microEmpresa,
                                      p6050,p6210)

#Renombrar variables 
geih_select <- geih_select %>% 
  rename(ocupacion=relab,
         educacion=p6210,
         edad=age,
         posicion= p6050) 


# Por ahora eliminé filas con valores faltantes o no finitos en ln_wage
geih_select <- geih_select[complete.cases(geih_select$ln_wage) & 
                             is.finite(geih_select$ln_wage), ]

#vamos a codificar nuestra variable sexo. Vamos a dejar a las mujeres con 1.
geih_select$sex <- ifelse(geih_select$sex == 1, 0, 1)
table(geih_select$sex)

# A) Estimación de la brecha salarial incondicional. Log(w)= β1 + β2Female + u
reg1 <- lm(ln_wage ~ sex, data = geih_select)
summary(reg1)

stargazer(reg1, digits=3, align=TRUE, type="latex", out="views/4reg1.tex" , omit.stat = c("adj.rsq", "f", "ser"))

#B) Estimación de brecha salarial condicional incorporando variables de control
#como características similares de trabajadores y puestos de trabajo.

reg2<-lm(ln_wage ~ sex+posicion+oficio+ocupacion+formal+microEmpresa+
           maxEducLevel+edad, data = geih_select)
stargazer(reg2,type="text",digits=3 , omit.stat = c("adj.rsq", "f", "ser")) 
summary(reg2)

# Estimamos la brecha salarial usando:

# Eliminación de una fila con valores faltantes en todas las variables utilizadas en el modelo
geih_select <- geih_select[complete.cases(geih_select[, 
                                                      c("sex", "posicion", "oficio", 
                                                        "ocupacion", "formal", 
                                                        "microEmpresa", "maxEducLevel",
                                                        "edad")]), ]

## i) Usamos FWL

geih_select<- geih_select %>% mutate(woman_res=lm(as.numeric(sex) ~ posicion+oficio+
                                                    ocupacion+formal+microEmpresa+
                                                    maxEducLevel+edad, geih_select)$residuals) #obtenemos los residuales de sex~x

geih_select<- geih_select %>% mutate(log_wage_res=lm(ln_wage ~ posicion+oficio+
                                                       ocupacion+formal+microEmpresa+
                                                       maxEducLevel+edad, geih_select)$residuals) #obtenemos los residuales de logy~x

# Corremos una regresión con las anteriores dos regresiones

reg3<- lm(log_wage_res~woman_res, geih_select)
stargazer(reg3,type="text",digits=3 ) 
stargazer(reg3, reg1 , digits=3, align=TRUE, type="latex", out="views/4reg3.tex" ,
          omit.stat = c("adj.rsq", "f", "ser"))

## ii) FWL con boostrap

btrap<-function(data,index){
  data<-data %>% mutate(woman_res=lm(as.numeric(sex)~posicion+oficio+ocupacion+
                                       formal+microEmpresa+maxEducLevel+edad,
                                     geih_select)$residuals) #obtenemos los residuales de sex~x
  data<-data %>% mutate(log_wage_res=lm(ln_wage~posicion+oficio+ocupacion+
                                          formal+microEmpresa+maxEducLevel+edad, 
                                        geih_select)$residuals) #obtenemos los residuales de logy~x
  coef(lm(log_wage_res~woman_res, data = data, subset = index))[2]  # nos suministra el segundo coeficiente de la regresión lineal 
}

btrap(geih_select,1:nrow(geih_select))

boot(geih_select, btrap, R = 2000) 

# C) Gráficamos la predición salario-edad, y estimación de las edades donde se 
#logra el mayor slario con los respectivos intervalaos de confianza por género. 
## - estimacion de los coeficientes 

mod0 <- lm(ln_wage ~ edad + I(edad^2), data = geih_select, subset = (sex == 0))
mod1 <- lm(ln_wage ~ edad + I(edad^2), data = geih_select, subset = (sex == 1))

## Realizamos el gráfico usando los coeficientes estimados. Con esto generamos un
#gráfico de ingresos en funcion de la edad. 

age <- seq(min(geih_select$edad), max(geih_select$edad), by = 1)  # edades

predic0 <- predict(mod0, newdata = data.frame(edad=age))  # Predicciones
predic1 <- predict(mod1, newdata = data.frame(edad=age))  # Predicciones

geih_select_0 <- geih_select[geih_select$sex == 0,]
geih_select_1 <- geih_select[geih_select$sex == 1,]

peak_ages<-function(data,index){
  coefficients <- coef(lm(ln_wage ~ edad + I(edad^2), data = data, subset = index))
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
graph_peak <- file.path("views", "earnings_peak_ages.png")
# gráfico
png(filename = graph_peak, width = 800, height = 600 )

# Plot del perfil de ingresos
plot(geih_select$edad, (geih_select$ln_wage), 
     xlab = "Edad", ylab = "log(Ingresos)", 
     col = alpha("grey", 0.8), main = "Ingresos vs. Edad por sexo", 
     xlim = c(10, 80), ylim = c(5, 13),  # Establecer rango y frecuencia en ejes X e Y
     xaxp = c(10, 80, 7), yaxp = c(5, 13, 8))  # Establecer frecuencia en ejes X e Y
lines(age, predic0, col = "red", lwd = 1)  # Línea de predicciones para Hombres
lines(age, predic1, col = "blue", lwd = 1)  # Línea de predicciones para Mujeres
# Colocamos líneas verticales
abline(v = 42.64015, col = "red", lwd = 2, lty = 1)  # Línea discontinua para el valor central Hombres
abline(v = quantile(val0$t[,1], 0.025), col = "red", lwd = 2, lty = 3) 
abline(v = quantile(val0$t[,1], 0.975), col = "red", lwd = 2, lty = 3) 
abline(v = 47.96018, col = "blue", lwd = 2, lty = 1)  # Línea discontinua para el valor central Mujeres
abline(v = quantile(val1$t[,1], 0.025), col = "blue", lwd = 2, lty = 3) 
abline(v = quantile(val1$t[,1], 0.975), col = "blue", lwd = 2, lty = 3) 
# Colocamos leyenda
legend("topright",                    # posición de la leyenda
       legend = c("Hombre", "Mujer"), # etiquetas
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


install.packages("dcolumn")


colnames(geih_select)
summarytools::freq(geih_select$oficio, cumul=FALSE)


