# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(xgboost) ; library(dplyr) ; library(mice)
library(Matrix) ; library(matrixStats) ; library(caret) ; library(doParallel)
library(Metrics)

source("data_manipulation_functions.R")
load("data/featureData.RData")

names <- c("toon_elasticnet_1",
           "toon_xgb_02",
           "toon_xgb_03",
           "toon_xgb_04",
           "toon_baggingofGAMS_02",
           "toon_extc_01")
stacking <- stacking_features(names)
data <- merge(data, stacking, "ID") ; rm(stacking) ; gc()

### create random variable for cv ###
set.seed(555)
data$cv_fold <- ceiling(0.1 * data$rand)


# 2. Model specific features ----------------------------------------------


# select model variables based on the suffix
keep_pattern <-
      c("ORIGINAL", "MISSING_COUNT", "toon_", "stacker_pctgsd", "stacker_mean") # to keep
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

# Convert to dummyvars
dummies <- dummyVars(target ~ ., data = data[, vars_to_keep, with = F])
data <- predict(dummies, newdata = data) %>%  # this converts to matrix & removes target
      as.data.table() %>%
      cbind(data[,"target", with = F])


# apply kaggle forum roughfix (median imputatian/ highest cardinality)
data <- na.roughfix2(data) %>% as.data.table()


# 3. Apply model ----------------------------------------------------------


param <- list(  objective           = "binary:logistic",
                booster             = "gbtree",
                eta                 = 0.005,
                max_depth           = 11,
                subsample           = 0.8,
                colsample_bytree    = 0.8,
                min_child_weight    = 1
)

no_vars <- c("rand", "target", "ID", "cv_fold")

dtrain <- xgb.DMatrix(data = data[cv_fold != 1 & rand < 100, -no_vars, with = F] %>%
                             data.matrix() %>%
                             Matrix(sparse = T),
                      label = data[cv_fold != 1 & rand < 100]$target)

dval <- xgb.DMatrix(data = data[cv_fold == 1 & rand < 100, -no_vars, with = F] %>%
                                data.matrix() %>%
                                Matrix(sparse = T),
                    label = data[cv_fold == 1 & rand < 100]$target)

watchlist <- list(val=dval, train=dtrain)

model <- xgb.train(params              = param,
                   data                = dtrain,
                   nrounds             = 5000,
                   watchlist           = watchlist,
                   print.every.n       = 15,
                   early.stop.round    = 150,
                   eval_metric         = "logloss",
                   maximize            = FALSE
                   )

data$pred <-
      predict(model, newdata = data[,-no_vars, with = F] %>% data.matrix() %>% Matrix(sparse = T))

# 4. Test model performance -----------------------------------------------

score <- logLoss(data[cv_fold == 1 & rand < 100]$target,
                 data[cv_fold == 1 & rand < 100]$pred)
score

# 5. Output model ---------------------------------------------------------

modeler_name <- "toon"
model_type <- "xgb_stacking"
index <- "01" # improved the lay-out of my script

model_name <- paste(modeler_name, model_type, index, sep = "_")


### a. CSV file for public leaderboard

ID_test <- data[rand > 100, "ID", with = F]
pred_test <- data[rand > 100, "pred", with = F]
submission_data <- data.frame(ID = ID_test, PredictedProb = pred_test)
names(submission_data)[2] <- "PredictedProb"
write.csv(submission_data, file =
                paste("finished_models_output/", model_name, "_submission", ".csv", sep = ""),
          row.names = F)


### b. A list with info about the model

# name output list = name_type of model_number
# save the script under the same name
model <-
      list(
            # name
            modeler_name = modeler_name,

            # What kind of model is fit?
            model_type = model_type,

            # some details about the model (Example tuning parameters)
            description = "XGB eta 0.005, depth 11, ntrees via validation, min_child 1, colsample 0.8,
            subsample 0.8, original vars from featurdata, missing count, v22 transformed
            via copy-paste transformation from forum and median imputation for
            missing values + first layer stacker models (toon_ & stacker_ models)",

            # variables used in the model
            variables = setdiff(names(data), c(no_vars, "pred", "pred_1",
                                               "pred_2", "pred_3", "pred_4", "pred_5")),

            # logLoss, based on your CV
            logLoss_estimation = score,

            # predictions --> ALL predictions with cv and rand
            predictions = data[,c("ID", "pred"), with = F],

            # other useful info, can be different for each model
            other = list(model = NULL,

                         id_data = NULL,

                         remarks = "")


      )

# save
save(model, file = paste("finished_models_output/", model_name, ".Rdata"))