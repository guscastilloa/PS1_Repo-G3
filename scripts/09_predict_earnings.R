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


set.seed(1984)
train <- sample(nrow(db),size = 0.7*nrow(db),replace = F)
db.train <- db[train,]
db.test <- db[-train,]

############################################################################-
# 2. LOOCV ----
############################################################################-