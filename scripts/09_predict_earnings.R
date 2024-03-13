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
       foreach, doParallel, moments)
############################################################################-
# 1. Prepare dataset ----
############################################################################-
# db <- read_parquet("https://github.com/guscastilloa/PS1_Repo-G3/blob/135ee86dcd92c1f0ef52a9fb73c214a1f3e92338/stores/db.parquet")
db_f <- read_parquet("stores/db.parquet")
percentile_function<- ecdf(db_f$y_ingLab_m_ha)
db_f$y_ingLab_m_ha.percentile <- percentile_function(db_f$y_ingLab_m_ha)*100
db_f <- db_f %>% unite(unique_id,c(directorio, secuencia_p, orden))
db <- db_f %>% select(unique_id,ln_wage, y_total_m_ha, y_ingLab_m_ha,
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
f2v <- as.formula(ln_wage ~ age+agesqr+esc)
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

xtbl1 <- xtable(tbl1, caption = "Comparación RMSE", digits=3)
names(xtbl1) <- c("Especificación", "Test RMSE", "Grados de Libertado", "Número de Predictores")
print.xtable(xtbl1, booktabs = TRUE, file = "views/validation_set_rmse.tex",
             include.rownames = FALSE, include.colnames = T)


## Prediction error distribution -------

# Take best model and calculate leverage statistic
lm.fit.1 <- lm(f4v3, data = db)
db['hatvalues'] <- hatvalues(lm.fit.1)
db['residuals'] <- lm.fit.1$residuals
db['std_residuals'] <- studres
db['cooks_distance'] <- cooks.distance(lm.fit.1)

pred_errors <- (db.test$ln_wage-predict(lm.fit5v, db.test))
squared_errors <- (db.test$ln_wage-predict(lm.fit5v, db.test))^2

# Define outlier criteria
# 1. More than |2σ| from the mean
ul <- mean(db.test$sq_errors)+2*sd(db.test$sq_errors)
ll <- mean(db.test$sq_errors)-2*sd(db.test$sq_errors)
# 2. Leverage limit
leverage_limit <- (length(lm.fit.1$coefficients)+1)/nrow(db)
# 3. Cook's Distance greater than 1


db.test <-  db.test %>% mutate(
  y_hat=predict(lm.fit5v, db.test),
  pred_errors=pred_errors,
  sq_errors=squared_errors,
  type_pred_error=ifelse(pred_errors>0,"Subestimado",
                         ifelse(pred_errors<0,"Sobreestimado","Perfecto")),
  hatvalues=db[-train,]$hatvalues,
  residuals=db[-train,]$residuals,
  std_residuals=db[-train,]$std_residuals,
  outlier_2sigma = ifelse(sq_errors<ll | sq_errors>ul ,
                           "Sí",'No'),
  out_lvg_and_stdres = ifelse( (std_residuals>3 | std_residuals< -3) &
                                 (hatvalues>leverage_limit),
                               "Sí","No"),
  cooks_dist = db[-train,]$cooks_distance,
  cooks_gt1 = ifelse(cooks_dist>1,"Sí","No"),
  cooks_gt4n = ifelse(cooks_dist>4/(nrow(db)),"Sí","No"),
  cooks_gt4nk1 = ifelse(cooks_dist>4/(nrow(db)-length(coefficients(lm.fit.1))-1),
                        "Sí","No"),
  )
write_parquet(db.test %>% select(unique_id,y_hat,ln_wage, pred_errors, sq_errors,type_pred_error),
              sink = "stores/db_30_test.parquet")

o <- db.test %>% filter(unique_id %in% check_ids) %>% select(unique_id,esc, age, female,sq_errors, pred_errors)
o <- inner_join(x=db_f %>% 
                  filter(unique_id %in% check_ids) %>% 
                  select(unique_id,y_ingLab_m_ha.percentile, y_ingLab_m_ha),
                y=o, by='unique_id') %>% 
  select(unique_id, age, esc, female,y_ingLab_m_ha,y_ingLab_m_ha.percentile,
         pred_errors, sq_errors)

out_table <- tibble(o[,-1])
names(out_table) <-  c("Edad", "Años Escolaridad", 'Sexo',"Ing. Lab. por hora",
                       'Percentil Ing.','Error Predicción','Error²')
outxtable <- xtable(out_table, caption = "Características de los 8 Outliers")
print.xtable(outxtable, comment = FALSE, booktabs = TRUE, file = "views/Xoutliers.tex")

# Income Distribucion by error type
d1 <- ggplot(data =filter(db.test, squared_errors>1.17) ,
       aes(x=y_ingLab_m_ha))+
  geom_density()+
  theme_bw()+
  geom_vline(data= data.frame(xint=quantile(db$y_ingLab_m_ha, probs = c(0.05)),
                              type_pred_error='Sobreestimado'),
             aes(xintercept = xint),
             linetype='dotted',color='red')+
  geom_vline(data= data.frame(xint=quantile(db$y_ingLab_m_ha, probs = c(0.98)),
                              type_pred_error='Subestimado'),
             aes(xintercept = xint),
             linetype='dotted',color='blue')+
  
  facet_wrap(~type_pred_error, scales = 'free')+
  labs(x='Ingreso laboral por hora',
       y = 'Densidad')
ggsave("views/y_density_by_sqerrorsgt1p17sigma.pdf",
       plot = d1, width= 5, height = 3)


d2 <- ggplot(data =filter(db.test, squared_errors>1.17) ,
       aes(x=y_ingLab_m_ha))+
  stat_ecdf()+
  theme_bw()+
  geom_vline(data= data.frame(xint=quantile(db$y_ingLab_m_ha, probs = c(0.05)),
                              type_pred_error='Sobreestimado'),
             aes(xintercept = xint),
             linetype='dotted',color='red')+
  geom_hline(data= data.frame(yint=.60,
                              type_pred_error='Sobreestimado'),
             aes(yintercept = yint),
             linetype='dotted',color='green')+
  geom_vline(data= data.frame(xint=quantile(db$y_ingLab_m_ha, probs = c(0.98)),
                              type_pred_error='Subestimado'),
             aes(xintercept = xint),
             linetype='dotted',color='blue')+
  
  facet_wrap(~type_pred_error, scales = 'free')+
  labs(x='Ingreso laboral por hora',
       y = "CDF Empírica",
       caption = "Solo se toman observaciones cuyos errores al cuadrado superan mu+2sigma~1.17")

ggsave("views/y_ecdf_by_sqerrorsgt1p17sigma.pdf",
       plot = d2, width= 5, height = 3)

d1n2 <- ggarrange(d1, d2, nrow=2)
ggsave("views/density_and_ecdf.pdf",
       plot = d1n2, width= 5, height = 5)

ggplot(data =filter(db.test, squared_errors>1) ,
       aes(x=hatvalues))+
  geom_density()+
  theme_bw()+
  facet_grid(~type_pred_error, scales = 'free')

## Leverage vs studentized residuals
p<- ggplot(db.test, aes(x=hatvalues, y=std_residuals, 
                        color=out_lvg_and_stdres, shape=type_pred_error))+
  geom_point()+
  theme_bw()+
  geom_hline(yintercept = 3, linetype='dotted', color='gray')+
  geom_hline(yintercept = -3, linetype='dotted', color='gray')+
  geom_vline(xintercept = leverage_limit, linetype='dotted', color='black')+
  labs(x='Leverage',
       y='Residuales Student',
       color="Potencial Outlier",
       shape="Error")+
  theme(legend.position='bottom',legend.box="vertical",legend.margin=margin())+
  scale_color_manual(values =c("gray", 'blue'))
ggsave(filename= "views/bestpred-resVleverage.pdf", p,
       width = 4, height = 5)  

############################################################################-
# 2. LOOCV ----
############################################################################-

## For second best model: f4v2 -----------

cluster <- parallel::makeCluster(parallel::detectCores()-1, type = "SOCK")
doParallel::registerDoParallel(cluster)

#################################################-
ctrl <- caret::trainControl(method = 'LOOCV', allowParallel = TRUE)

start_time <- Sys.time()
small.crt.mse <- caret::train(f4v2, data = db, method = 'lm', trControl=ctrl)
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

print("Logró guardar f4v2!!! *******************************************")
print(crt.rmse)
cat("---------------------------------------------------------\n\n\n\n")




## For best model: f4v3 -----------
cluster <- parallel::makeCluster(parallel::detectCores()-1, type = "SOCK")
doParallel::registerDoParallel(cluster)

#################################################-
ctrl <- caret::trainControl(method = 'LOOCV', allowParallel = TRUE)

start_time <- Sys.time()
small.crt.mse <- train(f4v3, data = db, method = 'lm', trControl=ctrl)
end_time <- Sys.time()

crt.rmse_f4v3 <- RMSE(small.crt.mse$pred$pred, db$ln_wage)

time.small.crt <- end_time - start_time
print(time.small.crt)

#################################################-

parallel::stopCluster(cluster)
unregister_dopar()

print("************ successs ***********************************")

# Export 
saveRDS(loocv,"stores/loocv.rds")


loocv <- tibble(
  "Modelo"=c("Contorles v2", "Controles v3"),
  "Test Error LOOCV"=c(crt.rmse, crt.rmse_f4v3)
)