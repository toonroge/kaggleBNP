# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(dplyr) ; library(mice)
library(plotly) ; library(ggplot2)

set.seed(666)

source("data_manipulation_functions.R")

train <- fread("data/train.csv") ; train$rand <- round(runif(nrow(train)) * 100)
test <- fread("data/test.csv") ; test$target <- 0 ; test$rand <- 101

data <- rbind(train, test, fill = T) %>% transform_catvars_to_rank()

keep_pattern <-
      c("CATRANK") # to keep
vars_to_keep <- c()
for (pattern in keep_pattern) {
      vars_to_keep <-
            c(vars_to_keep, grep(pattern, names(data), value = TRUE))
}

data$CATRANK_q000 <- round((apply(data[, vars_to_keep, with = F], 1, min) / 10))
data$CATRANK_q025 <- round((apply(data[, vars_to_keep, with = F], 1, quantile, probs = 0.25) / 10))
data$CATRANK_q050 <- round((apply(data[, vars_to_keep, with = F], 1, median) / 10))
data$CATRANK_avg <- round((apply(data[, vars_to_keep, with = F], 1, mean) / 10))
data$CATRANK_q075 <- round((apply(data[, vars_to_keep, with = F], 1, quantile, probs = 0.75) / 10))
data$CATRANK_q100 <- round((apply(data[, vars_to_keep, with = F], 1, max) / 10))

data <- data[, c(vars_to_keep, "ID"), with = F]

save(data, file = "data/featureData3.Rdata")