# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(dplyr) ; library(mice)

set.seed(666)

source("data_manipulation_functions.R")

train <- fread("data/train.csv") ; train$rand <- round(runif(nrow(train)) * 100)
test <- fread("data/test.csv") ; test$target <- 0 ; test$rand <- 101

data <- rbind(train, test, fill = T) ; rm(train, test) ; gc()

data <-
data %>% clust_pca(idvar = "ID",
                   ignore = c("rand", "target"),
                   n_clust = 40,
                   clust_method = "hclust")

save(data, file = "data/featureData2.Rdata")