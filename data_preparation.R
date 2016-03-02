# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(dplyr) ; library(mice)

set.seed(666)

source("data_manipulation_functions.R")

train <- fread("data/train.csv") ; train$rand <- round(runif(nrow(train)) * 100)
test <- fread("data/test.csv") ; test$target <- 0 ; test$rand <- 101

data <- rbind(train, test, fill = T)

# replace blank categorical levels by unknown
# remove some variables
# flag others

data <- data %>%
            replace_blank_categorical() %>%
            manual_cleaning(data) %>%
            flag_original_variables(ignore = c("ID", "target", "rand")) %>%
            std_normal_transform(ignore = c("ID", "target", "rand")) %>%
            group_low_categorical(n = 750, max_length = 15) %>%
            group_low_categorical(n = 500, max_length = 50, ignore_pattern = "GROUP_LOW_CATEG") %>%
            impute_customrf(keep_pattern = c("GROUP_LOW_CATEG", "STD_NORM")) %>%
            missings_per_row(ignore_pattern = c("ID", "target", "GROUP_LOW_CATEG", "STD_NORM"))

save(data, file = "data/featureData.Rdata")





