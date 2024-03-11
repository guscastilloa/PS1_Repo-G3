##############################################################################-
# DATE:
#   2024/feb/28
# AUTHOR:
#  Gustavo Castillo
# DESCRIPTION:
#   Evaluate predictive power of previous econometric specifications
##############################################################################-

# Prepare workspace

rm (list=ls())
gc()
source("scripts/00_packages.R")
p_load(leaps, stats, MASS, boot, caret, rio, dplyr, arrow, doSNOW, 
       foreach, doParallel)
############################################################################-
# 1. Prepare dataset ----
############################################################################-
db <- read_parquet("https://github.com/guscastilloa/PS1_Repo-G3/blob/135ee86dcd92c1f0ef52a9fb73c214a1f3e92338/stores/db.parquet")
db <- read_parquet("stores/db.parquet")
db <- db %>% select(ln_wage, y_total_m_ha, y_ingLab_m_ha,
                    hoursWorkUsual,age,female,oficio,ocu,maxEducLevel,
                    formal,microEmpresa,p6050,p6210,
                    posicion, sector,exp, age, agesqr, esc)
db<-na.omit(db)

# List models to evaluate
f1 <- as.formula(ln_wage ~ female)
f2 <- as.formula(ln_wage ~ age + I(age^2))
f3 <- as.formula(ln_wage ~ female+age + I(age^2))
f4 <- as.formula(ln_wage~age+agesqr+female+esc+hoursWorkUsual+formal+
                              sector+oficio+microEmpresa)

# Variations
f2v <- as.formula(ln_wage ~ poly(age, 15, raw=TRUE))
f3v <- as.formula(ln_wage ~ female+age+agesqr+female:poly(age,15,raw=TRUE)+poly(esc,3))
f4v1 <- as.formula(ln_wage~poly(age,6, raw=TRUE)+female+female:age+esc+
                    hoursWorkUsual+formal+sector+oficio+microEmpresa+female:formal+
                    female:microEmpresa)
f4v2 <- as.formula(ln_wage~poly(age,6, raw=TRUE)+female+female:age+poly(esc,5)+
                    poly(hoursWorkUsual,3)+formal+sector+oficio+microEmpresa+
                    female:formal+female:microEmpresa+esc:sector)
f4v3 <- as.formula(ln_wage~poly(age,6, raw=TRUE)+female+female:age+poly(esc,6)+
                    poly(hoursWorkUsual,7)+formal+sector+oficio+microEmpresa+
                    female:formal+female:microEmpresa+ esc:sector+poly(age,4):poly(esc,5))
#..................


############################################################################-
# 2. Validation set approach ----
############################################################################-
set.seed(1984)
train <- caret::createDataPartition(y = db$ln_wage, p = .70, list = F)

# train <- sample(nrow(db),size = 0.7*nrow(db),replace = F)
db.train <- db[train,]
db.test <- db[-train,]

  # fix_lm.fit3 <- caret::train(form_controls, data = db, method='lm', subset = train)

lm.fit1 <- caret::train(f1,data = db, subset = train, method = 'lm')
lm.fit2 <- caret::train(f2,data = db, subset = train, method = 'lm')
lm.fit3 <- caret::train(f3,data = db, subset = train, method='lm')
lm.fit4 <- caret::train(f4,data = db, subset = train, method = 'lm')

lm.fit1v <- caret::train(f2v,data = db, subset = train, method = 'lm')
lm.fit2v <- caret::train(f3v,data = db, subset = train, method = 'lm')
lm.fit3v <- caret::train(f4v1,data = db, subset = train, method = 'lm')
lm.fit4v <- caret::train(f4v2,data = db, subset = train, method = 'lm')
lm.fit5v <- caret::train(f4v3,data = db, subset = train, method = 'lm')

attach(db)
mse.fit1 <- mean((db$ln_wage-predict(lm.fit1, db))[-train]^2)
mse.fit2 <- mean((ln_wage-predict(lm.fit2, db))[-train]^2)
mse.fit3 <- mean((ln_wage-predict(lm.fit3, db))[-train]^2)
mse.fit4 <- mean((ln_wage-predict(lm.fit4, db))[-train]^2)

mse.fit1v <- mean((ln_wage-predict(lm.fit1v, db))[-train]^2)
mse.fit2v <- mean((ln_wage-predict(lm.fit2v, db))[-train]^2)
mse.fit3v <- mean((ln_wage-predict(lm.fit3v, db))[-train]^2)
mse.fit4v <- mean((ln_wage-predict(lm.fit4v, db))[-train]^2)
mse.fit5v <- mean((ln_wage-predict(lm.fit5v, db))[-train]^2)

tbl1 <- data.frame("Especificación"=c("Solo Sexo", "Edad + Edad²", "Sexo,Edad,Edad²",
                              "Con Controles", "Edad v1", "Sexo,Edad v1", 
                              "Controles v1", "Contorles v2", "Controles v3"),
           "Test RMSE"= c(round(sqrt(mse.fit1), 3), round(sqrt(mse.fit2), 3), 
                          round(sqrt(mse.fit3), 3), round(sqrt(mse.fit4), 3),
                          round(sqrt(mse.fit1v), 3), round(sqrt(mse.fit2v), 3),
                          round(sqrt(mse.fit3v), 3),round(sqrt(mse.fit4v), 3), 
                          round(sqrt(mse.fit5v), 3)
                          ),
           "Grados de Libertad"=c(summary(lm.fit1)$df[2], summary(lm.fit2)$df[2],
                                  summary(lm.fit3)$df[2], summary(lm.fit4)$df[2],
                                  summary(lm.fit1v)$df[2], summary(lm.fit2v)$df[2],
                                  summary(lm.fit3v)$df[2],summary(lm.fit4v)$df[2],
                                  summary(lm.fit5v)$df[2]
                                  ),
           "Número de Predictores"=c(length(lm.fit1$coefnames),length(lm.fit2$coefnames),
                                     length(lm.fit3$coefnames), length(lm.fit4$coefnames),
                                     length(lm.fit1v$coefnames),length(lm.fit2v$coefnames),
                                     length(lm.fit3v$coefnames),length(lm.fit4v$coefnames),
                                     length(lm.fit5v$coefnames)
                                     )
           ); tbl1

xtbl1 <- xtable(tbl1, caption = "Comparación RMSE con 70% muestra de entrenamiento")
names(xtbl1) <- c("Especificación", "Test RMSE", "Grados de Libertado", "Número de Predictores")
print.xtable(xtbl1, booktabs = TRUE, file = "views/validation_set_rmse.tex",
             include.rownames = FALSE, include.colnames = T)

############################################################################-
# 2. LOOCV ----
############################################################################-

## For second best model: f4v2 -----------

cluster <- parallel::makeCluster(parallel::detectCores()-1, type = "SOCK")
doParallel::registerDoParallel(cluster)

#################################################-
ctrl <- caret::trainControl(method = 'LOOCV', allowParallel = TRUE)

start_time <- Sys.time()
small.crt.mse <- caret::train(f4, data = db, method = 'lm', trControl=ctrl)
end_time <- Sys.time()

crt.rmse <- RMSE(small.crt.mse$pred$pred, db$ln_wage)

time.small.crt <- end_time - start_time
print(time.small.crt)

#################################################-

parallel::stopCluster(cluster)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}
unregister_dopar()




## For best model: f4v3 -----------
cluster <- parallel::makeCluster(parallel::detectCores()-1, type = "SOCK")
doParallel::registerDoParallel(cluster)

#################################################-
ctrl <- caret::trainControl(method = 'LOOCV', allowParallel = TRUE)

start_time <- Sys.time()
small.crt.mse <- train(f4v3, data = db, method = 'lm', trControl=ctrl)
end_time <- Sys.time()

crt.rmse <- RMSE(small.crt.mse$pred$pred, db$ln_wage)

time.small.crt <- end_time - start_time
print(time.small.crt)

#################################################-

parallel::stopCluster(cluster)
unregister_dopar()