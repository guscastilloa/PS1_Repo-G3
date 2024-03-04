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

# List models to evaluate
form_sex <- as.formula(ln_wage ~ sex)
form_age2 <- as.formula(ln_wage ~ age + I(age^2))
form_controls <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                              microEmpresa+maxEducLevel+age)
form_controls2 <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                               microEmpresa+maxEducLevel+age+I(age^2))
form_controls3 <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                               microEmpresa+maxEducLevel+age+I(age^2)+
                               as.factor(sector)+hoursWorkUsual+as.factor(p6430)+
                               exp+esc
                               )

# Variations
form_age2v1 <- as.formula(ln_wage ~ poly(age, 5))
form_controlsv1 <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                              microEmpresa+age:sex+poly(maxEducLevel,3))
form_controls2v1 <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                               microEmpresa+maxEducLevel+age:sex+poly(age,5))
form_controls3v1 <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                               microEmpresa+maxEducLevel+as.factor(sector)+hoursWorkUsual+as.factor(p6430)+
                               exp+esc+poly(age,8))
form_controls3v2 <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                                 microEmpresa+maxEducLevel+as.factor(sector)+hoursWorkUsual+as.factor(p6430)+
                                 exp+esc+poly(age,15))                             

############################################################################-
# 2. Validation set approach ----
############################################################################-
set.seed(1984)
train <- caret::createDataPartition(y = db$ln_wage, p = .70, list = F)

# train <- sample(nrow(db),size = 0.7*nrow(db),replace = F)
db.train <- db[train,]
db.test <- db[-train,]

  # fix_lm.fit3 <- caret::train(form_controls, data = db, method='lm', subset = train)

lm.fit1 <- caret::train(form_sex,data = db, subset = train, method = 'lm')
lm.fit2 <- caret::train(form_age2,data = db, subset = train, method = 'lm')
lm.fit3 <- caret::train(form_controls,data = db, subset = train, method='lm')
lm.fit4 <- caret::train(form_controls2,data = db, subset = train, method = 'lm')
lm.fit5 <- caret::train(form_controls3,data = db, subset = train, method = 'lm')

lm.fit1v <- caret::train(form_age2v1,data = db, subset = train, method = 'lm')
lm.fit2v <- caret::train(form_controlsv1,data = db, subset = train, method = 'lm')
lm.fit3v <- caret::train(form_controls2v1,data = db, subset = train, method = 'lm')
lm.fit4v <- caret::train(form_controls3v1,data = db, subset = train, method = 'lm')
lm.fit5v <- caret::train(form_controls3v2,data = db, subset = train, method = 'lm')

attach(db)
mse.fit1 <- mean((db$ln_wage-predict(lm.fit1, db))[-train]^2)
mse.fit2 <- mean((ln_wage-predict(lm.fit2, db))[-train]^2)
mse.fit3 <- mean((ln_wage-predict(lm.fit3, db))[-train]^2)
mse.fit4 <- mean((ln_wage-predict(lm.fit4, db))[-train]^2)
mse.fit5 <- mean((ln_wage-predict(lm.fit5, db))[-train]^2)

mse.fit1v <- mean((ln_wage-predict(lm.fit1v, db))[-train]^2)
mse.fit2v <- mean((ln_wage-predict(lm.fit2v, db))[-train]^2)
mse.fit3v <- mean((ln_wage-predict(lm.fit3v, db))[-train]^2)
mse.fit4v <- mean((ln_wage-predict(lm.fit4v, db))[-train]^2)
mse.fit5v <- mean((ln_wage-predict(lm.fit5v, db))[-train]^2)

tbl1 <- data.frame("Especificación"=c("Solo Sexo", "Edad + Edad²", "Controles 1",
                              "Controles 2", "Controles 3", "Edad v1", 
                              "Controles 1 v1", "Controles 2 v1", "Contorles 3 v1",
                              "Controles 3 v2"),
           "Test RMSE"= c(round(sqrt(mse.fit1), 3), round(sqrt(mse.fit2), 3), 
                          round(sqrt(mse.fit3), 3), round(sqrt(mse.fit4), 3),
                          round(sqrt(mse.fit5), 3), round(sqrt(mse.fit1v), 3),
                          round(sqrt(mse.fit2v), 3), round(sqrt(mse.fit3v), 3),
                          round(sqrt(mse.fit4v), 3), round(sqrt(mse.fit5v), 3)
                          ),
           "Grados de Libertad"=c(summary(lm.fit1)$df[2], summary(lm.fit2)$df[2],
                                  summary(lm.fit3)$df[2], summary(lm.fit4)$df[2],
                                  summary(lm.fit5)$df[2], summary(lm.fit1v)$df[2],
                                  summary(lm.fit2v)$df[2], summary(lm.fit3v)$df[2],
                                  summary(lm.fit4v)$df[2], summary(lm.fit5v)$df[2]
                                  ),
           "Número de Predictores"=c(length(lm.fit1$coefnames),length(lm.fit2$coefnames),
                                     length(lm.fit3$coefnames), length(lm.fit4$coefnames),
                                     length(lm.fit5$coefnames), length(lm.fit1v$coefnames),
                                     length(lm.fit2v$coefnames), length(lm.fit3v$coefnames),
                                     length(lm.fit4v$coefnames), length(lm.fit5v$coefnames)
                                     )
           )

xtbl1 <- xtable(tbl1, caption = "Comparación RMSE con 70% muestra de entrenamiento")
names(xtbl1) <- c("Especificación", "Test RMSE", "Grados de Libertado", "Número de Predictores")
print.xtable(xtbl1, booktabs = TRUE, file = "views/validation_set_rmse.tex",
             include.rownames = FALSE, include.colnames = T)

############################################################################-
# 2. LOOCV ----
############################################################################-

cluster <- parallel::makeCluster(parallel::detectCores()-1, type = "SOCK")
doParallel::registerDoParallel(cluster)

#################################################-
ctrl <- caret::trainControl(method = 'LOOCV', allowParallel = TRUE)

start_time <- Sys.time()
small.crt.mse <- train(form_1, data = db, method = 'lm', trControl=ctrl)
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
