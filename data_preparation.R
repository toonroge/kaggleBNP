# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(dplyr) ; library(mice)

set.seed(666)

source("data_manipulation_functions.R")

train <- fread("data/train.csv") ; train$rand <- round(runif(nrow(train)) * 100)
test <- fread("data/test.csv") ; test$target <- 0 ; test$rand <- 101

data <- rbind(train, test, fill = T)

# PCA might be more efficient
# For categorical variables --> MCA
# You can also find clusters and then do PCA for clusters
#
# Printing still to be improved
#
# add still the code from test_glmnet.R in function for transforming categorical in numerical

data <- data %>%
            replace_blank_categorical() %>%
            do_data_cleaning(cor_value = 0.8) %>%
            flag_original_variables(ignore = c("ID", "target", "rand")) %>%
            std_normal_transform(ignore = c("ID", "target", "rand")) %>%
            group_low_categorical(n = 750, max_length = 15) %>%
            group_low_categorical(n = 500, max_length = 50, ignore_pattern = "GROUP_LOW_CATEG") %>%
            impute_customrf(keep_pattern = c("GROUP_LOW_CATEG", "STD_NORM")) %>%
            missings_per_row(ignore_pattern = c("ID", "target", "GROUP_LOW_CATEG", "STD_NORM")) %>%
            transform_cat_to_ridge_coefs(cores = 4, alpha = 0.25,
                                          keep_pattern = c("STD_NORM_IMP_customRF", "GROUP_LOW_CATEG_n_500_max_50",
                                                          "MISSING_LABEL"),
                                          ignore_pattern = c("GROUP_LOW_CATEG_n_500_max_50_IMP"),
                                          ID = "ID",
                                          random = "rand", # need to be 1-100 for training and > 100 for test
                                          target = "target")

save(data, file = "data/featureData.Rdata")