##############################################################################-
# DATE:
#   2024/mar/7
# AUTHOR:
#  Gustavo Castillo
# DESCRIPTION:
#   Evaluate incidence of predictive power of including sampling weights
##############################################################################-

# Prepare workspace
rm (list=ls())
gc()
source("scripts/00_packages.R")

############################################################################-
# 1. Prepare dataset ----
############################################################################-

dbf <- arrow::read_parquet("stores/db.parquet")
db <- dbf[complete.cases(dbf$y_ingLab_m_ha, dbf$maxEducLevel),] %>% 
  mutate(ln_wage = log(1+y_ingLab_m_ha)) %>% 
  rename(ocupacion=relab) %>% 
  mutate(p6050=as.factor(p6050), 
         sector=as.factor(sector),
         p6430=as.factor(p6430),
         oficio= as.factor(oficio),
         ocupacion=as.factor(ocupacion))

############################################################################-
# 2. Estimate  ----
############################################################################-

# Sampling Weght: fex_c
f <- as.formula(ln_wage ~ sex+age + I(age^2))
glm.fit <- glm(f, data=db)
glm.w.fit <- glm(f, data=db, weights = fex_c)
dat.weighted<- svydesign(ids = ~1, data = db, weights = db$fex_c)
svy.1 <- svyglm(f, design=dat.weighted)

stargazer::stargazer(glm.fit, glm.w.fit, svy.1, dep.var.caption = "Variable Dependiente",
                     dep.var.labels = "Log Salario", align = T, 
                     column.labels = c(" glm", "SW glm", "SW svyglm"),
                     no.space = T, keep.stat="n",
                     covariate.labels = c("Sexo", "Edad", "Edad²", "Intercepto"),
                     type="latex",out = "views/a1-fit.tex", header = F)
# Replace "Observations" with "Observaciones"
regression_table <- readLines("views/a1-fit.tex")
regression_table <- gsub("Observations", "Observaciones", regression_table)
regression_table <- gsub(" glm", " \\\\texttt{glm}", regression_table)
regression_table <- gsub(" svyglm", " \\\\texttt{svyglm}", regression_table)
writeLines(regression_table[-12], "views/a1-fit.tex")

set.seed(1989)

cv.err <- boot::cv.glm(data = db, glm.fit, K=20); 
cv.svyerr <- boot::cv.glm(data = db, svy.1, K=20);
cv.err.w <- boot::cv.glm(data = db, glm.w.fit, K=20); 

############################################################################-
# 3. Export  ----
############################################################################-

t <- tibble("Estimación"= c("svy","glm sin SW","glm con SW"),
     "Error Estimado"=c(sprintf("%.7f", cv.svyerr$delta[2]),
                    sprintf("%.7f", cv.err$delta[2]),
                    sprintf("%.7f", cv.err.w$delta[2])))
t['Diferencia Porcentual'] <- (as.numeric(t$`Error Estimado`)/lag(as.numeric(t$`Error Estimado`))-1)*100
# xt <- xtable(t, caption = "Error de Predicción Estimado con 20 pliegues")
t_list <- list(t)
attr(t_list, "message") = c("SW=Factor de Expansión")
xt <- xtableList(t_list, caption = "Error de Predicción Estimado con 20 pliegues")
p <- print.xtableList(xt, booktabs = T, file = "views/a1-cverror.tex",
             floating = getOption("xtable.floating", TRUE))
writeLines(p, "views/a1-cverror.tex")
t2 <- readLines("views/a1-cverror.tex")
t2 <- gsub("\\[ht\\]", "[H]", t2[-c(1,2)])
writeLines(t2, "views/a1-cverror.tex")

geih <- arrow::read_parquet("stores/geih18.parquet")
sw_tbl <- tibble("Muestra"=c("Tota GEIH", "Submuestra"),
       "N"=c(nrow(geih), nrow(db)),
       "Media"=c(mean(geih$FEX_C), mean(db$fex_c)),
       "Desv. Est"=c(sd(geih$FEX_C), sd(db$fex_c)),
       "Asimetría"= c(moments::skewness(geih$FEX_C), moments::skewness(db$fex_c)),
       "Curtosis" = c(moments::kurtosis(geih$FEX_C), moments::kurtosis(db$fex_c)))
xt2 <- xtable(sw_tbl, caption = "Momentos de la distribución del factor de expansión")
print.xtable(xt2, booktabs = T, file = "views/a1-moments.tex",
                      floating = getOption("xtable.floating", TRUE))
t3 <- readLines("views/a1-moments.tex")
t3 <- gsub("\\[ht\\]", "[H]", t3[-c(1,2)])
writeLines(t3, "views/a1-moments.tex")
