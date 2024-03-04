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
source("scripts/00_packages.R")
gc()

############################################################################-
# 1. Prepare dataset ----
############################################################################-

db <- arrow::read_parquet("stores/db.parquet") %>% 
  mutate(ln_wage = log(1+y_ingLab_m_ha)) %>% 
  rename(ocupacion=relab)

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
                              microEmpresa+maxEducLevel+age:sex)
form_controls2v1 <- as.formula(ln_wage ~ sex+p6050+oficio+ocupacion+formal+
                               microEmpresa+maxEducLevel+age+age:sex+poly(age,5))
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
train <- sample(nrow(db),size = 0.7*nrow(db),replace = F)
db.train <- db[train,]
db.test <- db[-train,]


lm.fit1 <- lm(form_sex,data = db, subset = train)
lm.fit2 <- lm(form_age2,data = db, subset = train)
lm.fit3 <- lm(form_controls,data = db, subset = train)
lm.fit4 <- lm(form_controls2,data = db, subset = train)
lm.fit5 <- lm(form_controls3,data = db, subset = train)

lm.fit1v <- lm(form_age2v1,data = db, subset = train)
lm.fit2v <- lm(form_controlsv1,data = db, subset = train)
lm.fit3v <- lm(form_controls2v1,data = db, subset = train)
lm.fit4v <- lm(form_controls3v1,data = db, subset = train)
lm.fit5v <- lm(form_controls3v2,data = db, subset = train)

mse.fit1 <- mean((mpg-predict(lm.fit2, Auto))[-train]^2)
mse.fit2 <- mean((mpg-predict(lm.fit2, Auto))[-train]^2)
mse.fit3 <- mean((mpg-predict(lm.fit3, Auto))[-train]^2)

data.frame("Model"= c("First degree", "Second Degree", "Third Degree"),
           "MSE"= c(mse.fit1, mse.fit2, mse.fit3))


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