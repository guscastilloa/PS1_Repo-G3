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

db <- arrow::read_parquet("stores/db.parquet")

# List models to evaluate
form_sex <- as.formula(ln_wage ~ sex)
form_controls <- as.formula(ln_wage ~ sex+posicion+oficio+ocupacion+formal+
                              microEmpresa+maxEducLevel+edad)
form_controls2 <- as.formula(ln_wage ~ sex+posicion+oficio+ocupacion+formal+
                               microEmpresa+maxEducLevel+edad+I(edad^2))
form_controls3 <- as.formula(ln_wage ~ sex+posicion+oficio+ocupacion+formal+
                               microEmpresa+maxEducLevel+edad+I(edad^2)+hoursWorkUsual)
form_age2 <- as.formula(ln_wage ~ edad + I(edad^2))

############################################################################-
# 2. Sample split approach ----
############################################################################-
set.seed(1984)
train <- sample(nrow(db),size = 0.7*nrow(db),replace = F)
db.train <- db[train,]
db.test <- db[-train,]



############################################################################-
# 2. LOOCV ----
############################################################################-