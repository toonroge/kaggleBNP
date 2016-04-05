# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(dplyr) ; library(mice)

set.seed(666)

source("data_manipulation_functions.R")

train <- fread("data/train.csv") ; train$rand <- round(runif(nrow(train)) * 100)
test <- fread("data/test.csv") ; test$target <- 0 ; test$rand <- 101

data <- rbind(train, test, fill = T) ; rm(train, test) ; gc()

numeric_vars <- names(data)[sapply(data, is.numeric)]
vars <- setdiff(numeric_vars, c("ID", "target", "rand"))

get_rank <- function(x){
      rank <- round(1000 * rank(x) / length(x))
      rank <- ifelse(is.na(x), 500, rank)
}

neutralize_v50 <- function(x, y){
      out <- get_rank(x + 1) / get_rank(y + 1)
      get_rank(out)
}

db <- sapply(data[, vars, with = F], neutralize_v50, data$v50) %>% as.data.frame()

names(db) <- paste(names(db), "RANK_DIVIDE_V50", sep = "_")

db$v50_RANK_DIVIDE_V50 <- NULL

data <- cbind(data[, "ID", with = F], db)

save(data, file = "featureData4.Rdata")
