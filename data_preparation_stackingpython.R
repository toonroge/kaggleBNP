# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(xgboost) ; library(dplyr) ; library(mice)
library(Matrix) ; library(matrixStats) ; library(caret) ; library(doParallel)
library(Metrics)

source("data_manipulation_functions.R")
load("data/featureData.RData")
keep <- data ; rm(data)
load("data/featureData2.RData") ; gc()
data <- data[, -c("rand", "target"), with = F]
keep <- merge(keep, data, "ID", sort = F) ; rm(data)
load("data/featureData3.RData") ; gc()
data <- data[, -c("rand", "target"), with = F]
data <- merge(keep, data, "ID", sort = F) ; rm(keep) ; gc()

names <- c("toon_elasticnet_1",
           "toon_xgb_02",
           "toon_xgb_03",
           "toon_xgb_04",
           "toon_baggingofGAMS_02",
           "toon_extc_01",
           "toon_extc_02",
           "toon_xgb_05")
stacking <- stacking_features(names)
data <- merge(data, stacking, "ID") ; rm(stacking) ; gc()

# 2. Model specific features ----------------------------------------------

cat_vars <- c("v3", "v22", "v24",
              "v30", "v31", "v47", "v52", "v56", "v66", "v71", "v74", "v75",
              "v79", "v91", "v107", "v110", "v112", "v113", "v125")
cat_vars <- paste(cat_vars, "ORIGINAL", sep = "_")

# select model variables based on the suffix
keep_pattern <-
      c(cat_vars, "hclust", "MISSING_COUNT", "toon_", "stacker_pctgsd", "stacker_mean", "_CATRANK", "pgca_dist") # to keep
ignore_pattern <-
      c("RIDGE_COEF_FOLD", "GROUP_LOW_CATEG_n_500_max_50_IMP_customRF") # if matching these strings don't keep
vars_to_keep <- c()
for (pattern in keep_pattern) {
      vars_to_keep <-
            c(vars_to_keep, grep(pattern, names(data), value = TRUE))
}
ignore_vars <- c()
for (pattern in ignore_pattern) {
      ignore_vars <-
            c(ignore_vars, grep(pattern, names(data), value = TRUE))
}
vars_to_keep <- setdiff(vars_to_keep, ignore_vars) %>% c("ID", "rand", "target", "cv_fold")
data <- data[, vars_to_keep, with = F]

# Convert v22 to numeric (copy fuction from forum)
data$v22_ORIGINAL <- sapply(data$v22_ORIGINAL, az_to_int)

# apply kaggle forum roughfix (median imputatian/ highest cardinality)
data <- na.roughfix2(data) %>% as.data.table()

write.csv(subset(data, rand < 100), file = "data/train_stacking.csv", row.names = F)
write.csv(subset(data, rand > 100), file = "data/test_stacking.csv", row.names = F)




