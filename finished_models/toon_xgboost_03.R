# 1. Importing info - organizing data -------------------------------------


rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(xgboost)
library(Metrics) ; library(dplyr) ; library(doParallel)
library(Matrix) ; library(parallel)

source("data_manipulation_functions.R")
source("modeling_functions.R")

load("data/featureData2.RData")
### create random variable for cv ###
set.seed(555)
data$cv_fold <- ceiling(5 * runif(nrow(data)))


# 2. Model specific features ----------------------------------------------

cat_vars <- c("v3", "v22", "v24",
"v30", "v31", "v47", "v52", "v56", "v66", "v71", "v74", "v75",
"v79", "v91", "v107", "v110", "v112", "v113", "v125")

# select model variables based on the suffix
keep_pattern <-
      c(cat_vars, "hclust") # to keep
ignore_pattern <-
      c() # if matching these strings don't keep
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
data$v22 <- sapply(data$v22, az_to_int)

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
                eta                 = 0.01,
                max_depth           = 11,
                subsample           = 0.8,
                colsample_bytree    = 0.8,
                min_child_weight    = 1
)

no_vars <- c("rand", "target", "ID", "cv_fold")

# Remark - takes very long time to fit; on mac the parallelization doesn"t work by default
data <- fit_xgb_before_stacking(data = data,
                                param = param,
                                nrounds = 850,
                                cores = 4,
                                no_vars = no_vars,
                                cv_fold = data$cv_fold,
                                test = ifelse(data$rand < 100, 0, 1))


# 4. Test model performance -----------------------------------------------

score <- logLoss(data[rand < 100]$target,
                 data[rand < 100]$pred) # CV style - above 100 is for submission
score
# 0.4593891


# 5. Output model ---------------------------------------------------------

modeler_name <- "toon"
model_type <- "xgb"
index <- "03"

model_name <- paste(modeler_name, model_type, index, sep = "_")


### a. CSV file for public leaderboard

ID_test <- data[rand > 100, "ID", with = F]
pred_test <- data[rand > 100, "pred", with = F]
submission_data <- data.frame(ID = ID_test, pred = pred_test)
write.csv(submission_data, file =
                paste("finished_models_output/", model_name, "_submission", ".csv", sep = ""))

### b. CSV file for stacking - maybe best to include random variables as well

stacking_data <- data.frame(ID = data$ID,
                            pred = data$pred,
                            rand = data$rand,
                            fold = data$cv_fold)
write.csv(stacking_data, file =
                paste("finished_models_output/", model_name, "_stacking", ".csv", sep = ""))


### c. A list with info about the model

# name output list = name_type of model_number
# save the script under the same name
model <-
      list(
            # name
            modeler_name = modeler_name,

            # What kind of model is fit?
            model_type = model_type,

            # some details about the model (Example tuning parameters)
            description = "XGB eta 0.01, depth 11, ntrees 850, min_child 1, colsample 0.8,
            subsample 0.8, data after a clustering of correlated vars + PCA, v22 transformed
            via copy-paste transformation from forum and median imputation for
            missing values",

            # variables used in the model
            variables = setdiff(names(data), c(no_vars, "pred", "pred_1",
                                               "pred_2", "pred_3", "pred_4", "pred_5")),

            # logLoss, based on your CV
            logLoss_estimation = score,

            # predictions --> ALL predictions
            predictions = data[,c("ID", "pred"), with = F],

            # other useful info, can be different for each model
            other = list(model = NULL,

                         id_data = NULL,

                         remarks = "")


      )

# save
save(model, file = paste("finished_models_output/", model_name, ".Rdata"))
