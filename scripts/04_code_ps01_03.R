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

#Tenemos que un 11% de valores missings en horas trabajadas y en el ingreso por hora. 
#install.packages("visdat")
library(visdat)
vis_miss(geih_select)  

geih_select<- geih_select  %>% mutate(agesqr=age^2)
geih_select <- geih_select  %>% mutate(ln_wage = log(y_total_m_ha))

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
reg_age <- lm(ln_wage ~ age + agesqr+ hoursWorkUsual+age+sex+oficio+relab+
                college+ ocu+ maxEducLevel, geih_select)
summary(reg_age)

#Generate the LaTeX code using the stargazer function and store it in a variable
regression_table<- stargazer(reg_age,
           title = "Resultados de la regresion", 
                              align = TRUE, 
                              keep.stat = c("n", "rsq", "adj.rsq"),
                              keep=c("age","agesqr"),
                              dep.var.labels = "Logaritmo del Salario",
                              covariate.labels = c("Edad", "Edad al cuadrado")
                              ,out = "views/fit.tex")

# Reemplazar "Observations" con "Observaciones"
regression_table<- gsub("Observations", "Observaciones", regression_table)


#BOOTSTRAP to construct the confidence intervals -------------------------------
#Define a function that will extract the coefficients from the model based on bootstrap samples
get_coefficients <- function(geih_selected, indices) {
  fit <- lm(ln_wage ~ age + age2, data = geih_selected[indices, ])
  return(coef(fit))
}

#Use the boot function to perform the bootstrap procedure and calculate the confidence intervals
##Set stype = "i" to obtain the percentile intervals
boot_results <- boot(data = geih_selected, statistic = get_coefficients, R = 1000)
confidence_intervals <- boot.ci(boot_results, type = "perc")

#Obtain the confidence intervals
confidence_intervals_95 <- confidence_intervals$percent[, 4]

#Plot of the estimated age-earnings profile
geih_selected <- geih_selected  %>% mutate(yhat=predict(reg_age))
summ = geih_selected %>%  
  group_by(
    age, age2
  ) %>%  
  summarize(
    mean_y = mean(ln_wage),
    yhat_reg = mean(yhat), .groups="drop"
  ) 


age_earnings_plot <- ggplot(summ) + 
  geom_line(
    aes(x = age, y = yhat_reg), 
    color = "green", size = 1.5
  ) + 
  labs(
    title = "log Wages by Age in the GEIH 2018",
    x = "Age",
    y = "log Wages"
  ) +
  theme_bw() 

age_earnings_plot <- age_earnings_plot +
  geom_vline(xintercept = c(34.58, 49.45), linetype = "dashed", color = "red")

# Export ggplot as PNG
ggsave("stores/age_earnings_plot.png", plot = age_earnings_plot, width = 6, height = 4, dpi = 300)

