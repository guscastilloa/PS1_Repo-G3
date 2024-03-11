################################################################################
# Problem Set 1 - Question 3
# Authors: Alexander Almeida-Ramírez
# Descripción: Este archivo lee los datos y estima la relación entre salarios y 
#edad. 
################################################################################


# Preparar el análisis 
rm (list=ls())
source("scripts/00_packages.R")
gc()

#Cargar la base de datos 
db<-arrow::read_parquet("stores/db.parquet")


#Definir los posibles predictores de la base de datos: 
geih_select <- db  %>% select(ln_wage, y_total_m_ha, y_ingLab_m_ha,
                                      hoursWorkUsual,age,female,oficio,
                                      college,ocu,maxEducLevel,
                                      formal,microEmpresa,p6050,p6210,
                                      posicion, sector,exp, age, agesqr, esc)

#Eliminar las observaciones que tienen ingresos laborales cero. 
geih_select<-na.omit(geih_select)

#1 - REGRESIÓN : log(wage) = b1 + b2(age) + b3(age)^2 + u (también le vamos a agregar
#algunas variables explicativas adicionales)
reg_age_c <- lm(ln_wage ~ age + agesqr+female+hoursWorkUsual+sector+
                  posicion+exp+esc, geih_select)
summary(reg_age_c)

#2 - Regresión: Log(wage)=b1 + b2(age) + b3(age)^2 + u (sin controles)
reg_age_sc <- lm(ln_wage ~ age + agesqr, geih_select)
summary(reg_age_sc)

#Generar la tabla de regresión con Stargazer 
stargazer(reg_age_sc,reg_age_c,
          title = "Resultados de la regresion", 
          align = TRUE, 
          keep.stat = c("n", "rsq", "adj.rsq"),
          keep=c("age","agesqr"),
          dep.var.labels.include = FALSE,
          dep.var.caption  = "Logaritmo del salario",
          column.labels   = c("Con controles", "Sin controles"),
          covariate.labels = c("Edad", "Edad al cuadrado"),
          type="latex",out = "views/fit.tex")

# Leer el contenido del archivo
regression_table <- readLines("views/fit.tex")

# Reemplazar "Observations" con "Observaciones"
regression_table <- gsub("Observations", "Observaciones", regression_table)
regression_table <- gsub("Adjusted R$^{2}$ ", "R$^{2}$ ajustado", regression_table)


# Escribir el contenido modificado de nuevo al archivo
writeLines(regression_table, "views/fit.tex")

#Obtener la edad máxima en la que el incremento en el salario empieza a disminuir(SC)
matrix_coef <- summary(reg_age_sc)$coefficients
matrix_coef
coeficiente_edad<-my_estimates <- matrix_coef[2, 1] 
coeficiente_edad2<-my_estimates <- matrix_coef[3, 1] 
coeficiente_edad2


#Obtener la edad máxima en la que el incremento en el salario empieza a disminuir(C)
matrix_coef <- summary(reg_age_c)$coefficients
matrix_coef
coeficiente_edad_c<-my_estimates <- matrix_coef[2, 1] 
coeficiente_edad_c2<-my_estimates <- matrix_coef[3, 1] 
coeficiente_edad_c2

#Aplico la fórmula para obtener el "peak" de la edad 
edad_peak<-(-coeficiente_edad/(2*coeficiente_edad2))
edad_peak

#Calcular el promedio de la edad 
mean(db$age)
#Con controles
media_edad_c<-(coeficiente_edad_c+(2*coeficiente_edad_c2*39))*100
media_edad_c

#Sin controles
media_edad<-(coeficiente_edad+(2*coeficiente_edad2*39))*100
media_edad

#Encontramos que el peak del salario está en los 49.65 años. 

#BOOTSTRAP para construir los intervalos de confianza---------------------------
#Definir la función que espero extraer los coeficientes del modelo basado en 
#muestras de bootstrap
set.seed(1453)
coeficientes <- function(geih_select, indices) {
  fit <- lm(ln_wage ~ age + agesqr, data = geih_select[indices, ])
  return(coef(fit))
}

#Usar la función boot para calcular los coeficientes de confianza 
boot_resultados <- boot(data = geih_select, statistic = coeficientes, R = 1000)
coef_boot <- boot_resultados$t0
SE <- apply(boot_resultados$t,2,sd)

# Crear una secuencia de edades para predecir los ingresos
edades_pred <- seq(min(geih_select$age,na.rm=TRUE), max(geih_select$age,na.rm=TRUE),length=50)
edades_pred

#Ahora hacerlo para los intervalos de confianza 
yhat <- coef_boot[1] + coef_boot[2] * edades_pred + coef_boot[3] * edades_pred^2
y_inferior <- (coef_boot[1]-1.96*SE[1]) + (coef_boot[2]-1.96*SE[2])*edades_pred + 
  (coef_boot[3]-1.96*SE[3])*edades_pred^2
y_superior <- (coef_boot[1]+1.96*SE[1]) + (coef_boot[2]+1.96*SE[2])*edades_pred + 
  (coef_boot[3]+1.96*SE[3])*edades_pred^2

#Crear la base de datos con los estimados 
df <- data.frame(edades_pred, yhat, y_inferior, y_superior)

#Realizar la gráfica 
age_earnings_plot<- ggplot(df, aes(x = edades_pred, y = yhat)) +
  geom_line(aes(color = "Salario estimado"), size = 1) +
  geom_line(aes(x = edades_pred, y = y_inferior, color = "Límite inferior"), 
            linetype = "dotted", linewidth = 1) +
  geom_line(aes(x = edades_pred, y = y_superior, color = "Límite superior"), 
            linetype = "dotted", linewidth = 1) +
  geom_ribbon(aes(ymin = y_inferior, ymax = y_superior), fill = "gray80", alpha = 0.5) +
  scale_color_manual(name = "", values = c("Estimado" = "purple", "Límite inferior" = "blue", "Límite superior" = "blue")) +
  labs(x = "Edad", y = "Log(Salario)") +
  theme_classic() +
  scale_x_continuous(limits = c(18, 90)) +
  geom_vline(xintercept = 45.30, linetype = "dotted") +
  theme(legend.position = "bottom")

# Exportar la gráfica.
ggsave("views/age_earnings_plot.png", plot = age_earnings_plot, width = 6, 
       height = 4, dpi = 300)

