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

#Cargar la base de datos 
datos_geih<-read_parquet("stores/db.parquet")
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

geih_select<-na.omit(geih_select)
#Tenemos que un 11% de valores missings en horas trabajadas y en el ingreso por hora. 
vis_miss(geih_select)  

geih_select<- geih_select  %>% mutate(agesqr=age*age)
geih_select <- geih_select  %>% mutate(ln_wage = log(y_total_m_ha))

#Eliminar los valores missings en el ingroso por hora (para poder ejecutar 
#correctamente el bootstrap)


#Estadísticas descriptivas 
#Estadisticas descriptivas de las variables que vamos a analizar
summary_table <- stargazer(data.frame(geih_select), exclude = c("oficio", "relab"), 
                           title = "Variables incluidas en nuestra muestra seleccionada", 
                           align = TRUE, omit.stat = c("n"))

#Export descriptive analysis of selected variables in latex
writeLines(summary_table, "views/summary_table.tex")

#Estadísticas descriptivas de las variables categóricas 
maxEducLevel<-ggplot(geih_select, aes(x = `maxEducLevel`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Nivel de educación") + ylab("Frecuencia")
  ggtitle("Nivel educativo máximo alcanzado")

# Exportar la gráfica 
ggsave("views/maxEducLevel.png", plot = maxEducLevel, width = 6, height = 4,
       dpi = 300)

#Exportar gráfica de análisis de oficio 
oficio<-ggplot(geih_select, aes(x = `oficio`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Frecuencia de ocupación")
# Export ggplot as PNG
ggsave("views/oficio.png", plot = oficio, width = 6, height = 4, dpi = 300)

#Análisis del tipo de ocupación 
relab<-ggplot(geih_select, aes(x = `relab`)) +
  geom_bar() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Análisis del tipo de ocupación")
# Export ggplot as PNG
ggsave("views/relab.png", plot = relab, width = 6, height = 4, dpi = 300)


#REGRESIÓN : log(wage) = b1 + b2(age) + b3(age)^2 + u (también le vamos a agregar
#algunas variables explicativas adicionales)
reg_age_c <- lm(ln_wage ~ age + agesqr+ hoursWorkUsual+sex+oficio+relab+
                college+ maxEducLevel, geih_select)
summary(reg_age_c)

reg_age_sc <- lm(ln_wage ~ age + agesqr, geih_select)
summary(reg_age_sc)

#Generate the LaTeX code using the stargazer function and store it in a variable
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
regression_table <- gsub("Adjusted R", "R ajustado", regression_table)


# Escribir el contenido modificado de nuevo al archivo
writeLines(regression_table, "views/fit.tex")

#Obtener la edad máxima en la que el incremento en el salario empieza a disminuir 
matrix_coef <- summary(reg_age)$coefficients
matrix_coef
coeficiente_edad<-my_estimates <- matrix_coef[2, 1] 
coeficiente_edad2<-my_estimates <- matrix_coef[3, 1] 
coeficiente_edad2

#Aplico la fórmula para obtener el "peak" de la edad 
edad_peak<-(-coeficiente_edad/(2*coeficiente_edad2))
edad_peak
#Encontramos que el peak del salario está en los 49.65 años. 

#BOOTSTRAP para construir los intervalos de confianza---------------------------
#Definir la función que espero extraer los coeficientes del modelo basado en 
#muestras de bootstrap

# Calcular intervalos de confianza bootstrap
datos_completos <- na.omit(geih_select[c("age","ln_wage")])

set.seed(1453)
coeficientes <- function(datos_completos, indices) {
  fit <- lm(ln_wage ~ age + agesqr, data = geih_selected[indices, ])
  return(coef(fit))
}

#Usar la función boot para calcular los coeficientes de confianza 
boot_resultados <- boot(data = datos_completos, statistic = coeficientes, R = 1000)
coef_boot <- boot_resultados$t0
SE <- apply(boot_resultados$t,2,sd)

# Crear una secuencia de edades para predecir los ingresos
edades_pred <- seq(min(datos_completos$age,na.rm=TRUE), max(datos_completos$age,na.rm=TRUE),length=50)
edades_pred

#Ahora hacerlo para los intervalos de confianza 
y <- coef_boot[1] + coef_boot[2] * edades_pred + coef_boot[3] * edades_pred^2
y_i <- (coef_boot[1]-1.96*SE[1]) + (coef_boot[2]-1.96*SE[2])*edades_pred + 
  (coef_boot[3]-1.96*SE[3])*edades_pred^2
y_s <- (coef_boot[1]+1.96*SE[1]) + (coef_boot[2]+1.96*SE[2])*edades_pred + 
  (coef_boot[3]+1.96*SE[3])*edades_pred^2

df <- data.frame(edades_pred, y, y_i, y_s)

age_earnings_plot<- ggplot(df, aes(x = edades_pred, y = y)) +
  geom_line(aes(color = "Salario estimado"), size = 1) +
  geom_line(aes(x = edades_pred, y = y_i, color = "Límite inferior"), linetype = "dotted", linewidth = 1) +
  geom_line(aes(x = edades_pred, y = y_s, color = "Límite superior"), linetype = "dotted", linewidth = 1) +
  geom_ribbon(aes(ymin = y_i, ymax = y_s), fill = "gray80", alpha = 0.5) +
  scale_color_manual(name = "", values = c("Estimado" = "purple", "Límite inferior" = "blue", "Límite superior" = "blue")) +
  labs(x = "Edad", y = "Log(Salario)") +
  theme_classic() +
  scale_x_continuous(limits = c(18, 90)) +
  geom_vline(xintercept = 49.65, linetype = "dotted") +
  theme(legend.position = "bottom")



# Export ggplot as PNG
ggsave("views/age_earnings_plot.png", plot = age_earnings_plot, width = 6, 
       height = 4, dpi = 300)

