################################################################################
# Problem Set 1 - Question 3
# Authors: Jorge Luis Congacha Yunda
# Descripción: Brecha salarial
#Última actualización: Alexander Almeida -Ramírez 
################################################################################

# Prepare workspace
rm (list=ls())
source("scripts/00_packages.R")
gc()

#install.packages("arrow") ya está este paquete instalado en el 00 de paquetes? 
#library(arrow)

#Cargar la base de datos 

#Perfil edad-salario por género. 
datos_geih<-read_parquet("stores/db.parquet")
datos_geih<- datos_geih %>% mutate(agesqr=age*age)
datos_geih <- datos_geih %>% mutate(ln_wage = log(y_total_m_ha))


datos_hombre<-subset(datos_geih,datos_geih$sex==1)
datos_mujeres<-subset(datos_geih,datos_geih$sex==0)

# Definir géneros
generos <- unique(datos_geih$sex)

# Inicializar lista para almacenar los gráficos
plots_list <- list()

# Configurar la semilla para reproducibilidad
set.seed(1453)

# Inicializar lista para almacenar los gráficos
plots_list <- list()

# Iterar sobre cada género
for (genero in generos) {
  # Filtrar datos por género
  datos_completos <- na.omit(datos_geih[c("age","ln_wage","sex","agesqr")])
  datos_genero <- subset(datos_completos, sex == genero)
  
  set.seed(1453)
  coeficientes <- function(datos_genero, indices) {
    fit <- lm(ln_wage ~ age + agesqr, data = datos_genero[indices, ])
    return(coef(fit))
  }
  
  # Ajustar modelo y calcular intervalos de confianza
  boot_resultados <- boot(data = datos_genero, statistic = coeficientes, R = 1000)
  coef_boot <- boot_resultados$t0
  SE <- apply(boot_resultados$t,2,sd)
  
  # Crear una secuencia de edades para predecir los ingresos
  edades_pred <- seq(min(datos_genero$age,na.rm=TRUE), max(datos_genero$age,na.rm=TRUE),length=50)
  
  # Calcular los valores de predicción y los límites inferiores y superiores
  y <- coef_boot[1] + coef_boot[2] * edades_pred + coef_boot[3] * edades_pred^2
  y_i <- (coef_boot[1]-1.96*SE[1]) + (coef_boot[2]-1.96*SE[2])*edades_pred + 
    (coef_boot[3]-1.96*SE[3])*edades_pred^2
  y_s <- (coef_boot[1]+1.96*SE[1]) + (coef_boot[2]+1.96*SE[2])*edades_pred + 
    (coef_boot[3]+1.96*SE[3])*edades_pred^2
  
  # Crear un data frame con los resultados
  df <- data.frame(edades_pred, y, y_i, y_s)
  
  # Crear el gráfico y guardarlo en la lista de gráficos
  plots_list[[genero]] <- ggplot(df, aes(x = edades_pred, y = y)) +
    geom_line(aes(color = "Salario estimado"), size = 1) +
    geom_line(aes(x = edades_pred, y = y_i, color = "Límite inferior"), linetype = "dotted", linewidth = 1) +
    geom_line(aes(x = edades_pred, y = y_s, color = "Límite superior"), linetype = "dotted", linewidth = 1) +
    geom_ribbon(aes(ymin = y_i, ymax = y_s), fill = "gray80", alpha = 0.5) +
    scale_color_manual(name = "", values = c("Estimado" = "purple", "Límite inferior" = "blue", "Límite superior" = "blue")) +
    labs(x = "Edad", y = "Log(Salario)") +
    theme_classic() +
    scale_x_continuous(limits = c(18, 90)) +
    geom_vline(xintercept = 49.65, linetype = "dotted") +
    theme(legend.position = "bottom") +
    ggtitle(paste("Perfil Estimado de Ingresos por Edad - Género:", genero))
  
  # Guardar la gráfica como un archivo PNG
  ggsave(paste("views/age_earnings_plot_", tolower(genero), ".png", sep = ""), plot = plots_list[[genero]], width = 6, height = 4, dpi = 300)
}