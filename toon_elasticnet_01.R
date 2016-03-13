# 1. Importing info - organizing data -------------------------------------


rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(glmnet)
library(Metrics) ; library(dplyr) ; library(doParallel)
library(Matrix)

load("data/featureData.RData")

### create random variable for cv ###
set.seed(666)
data$cv_fold <- ceiling(5 * runif(nrow(data)))


# 2. Model specific features ----------------------------------------------


keep_pattern <-
      c("STD_NORM_IMP_customRF", "GROUP_LOW_CATEG_n_500_max_50", "MISSING_LABEL") # to keep
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

vars_to_keep <-
      vars_to_keep[!vars_to_keep %in% ignore_vars] %>% c("ID", "rand", "target", "cv_fold")

dummies <-
      dummyVars(target ~ ., data = data[, vars_to_keep, with = F])
# dummyVars converts to matrix --> reconvert to data.table
# dummyvars removes target ; add it again
dummieData <-
      predict(dummies, newdata = data) %>% as.data.table() %>% cbind(data[,"target", with = F])

# Add 2nd and 3th degree terms for important numeric features (found with dismo)
important_numeric_vars <- c(
      "v50_STD_NORM_IMP_customRF",
      "v6_STD_NORM_IMP_customRF",
      "v114_STD_NORM_IMP_customRF")

for (var in important_numeric_vars) {
      dummieData[[paste(var, "2", sep = "_")]] <- dummieData[[var]] ^ 2
      dummieData[[paste(var, "3", sep = "_")]] <- dummieData[[var]] ^ 3
}

# 66 has only three cardinalities and interacts a lot with other variables --> So add interactions
no_model_vars <- c("ID", "rand", "target", "cv_fold")
interaction_vars <- names(dummieData)[!names(dummieData) %in% no_model_vars]

for (var in interaction_vars) {
      name <- paste(var, "66A", sep = "_")
      dummieData[[name]] <-
            dummieData[[var]] * dummieData[["v66_GROUP_LOW_CATEG_n_500_max_50.A"]]
      name <- paste(var, "66B", sep = "_")
      dummieData[[name]] <-
            dummieData[[var]] * dummieData[["v66_GROUP_LOW_CATEG_n_500_max_50.B"]]
}


# 3. Apply model ----------------------------------------------------------


registerDoParallel(cores = 4)

train_control <- trainControl(method = "none",
                              classProbs = TRUE,
                              summaryFunction = multiClassSummary,
                              allowParallel = FALSE,
                              verbose = TRUE)

for (fold in 1:5){

      print(paste("Running the elastic net regression for fold", as.character(fold), sep = " "))
      # caret doesn't work with 0/1,
      # because internally converted to columns and 0/1 are no valid names
      xd <- dummieData[cv_fold != fold & rand < 100, -no_model_vars, with = F]
      yd <- dummieData[cv_fold != fold & rand < 100, c("target"), with = F]
      yd <- ifelse(yd$target == 1, "yes", "no") %>% as.factor() # change names

      model <- train(x = xd,
                     y = yd,
                     trControl = train_control,
                     method = "glmnet",
                     tuneGrid = data.frame(.alpha = 0.75, .lambda = 0.002))

      name <- paste("pred", as.character(fold), sep = "_")
      pred <- predict(model, newdata = dummieData[, -no_model_vars, with = F], type = "prob")
      pred <- pred[,which(names(pred) == "yes")] # write like this, because shouldn't return df
      dummieData[,name := ifelse(dummieData$cv_fold == fold, pred, NA), with = F]

      no_model_vars <- c(no_model_vars, name) # prevent leakage for the next fold

      # print per fold score
      score <- logLoss(dummieData[cv_fold == fold & rand < 100]$target,
                       dummieData[cv_fold == fold & rand < 100][[name]])
      print(paste("The out of fold Logloss is: ", as.character(score), sep = ""))
}

# Take mean of predictions per row ignoring NA's
keep_pattern = "pred"
var_names <- c()
for (pattern in keep_pattern) {
      var_names <- c(var_names, grep(pattern, names(dummieData), value = TRUE))
}
dummieData$pred <- rowMeans(dummieData[,var_names,with = F], na.rm = T)


# 4. Test model performance -----------------------------------------------


score <- logLoss(dummieData[rand < 100]$target,
                 dummieData[rand < 100]$pred) # CV style - above 100 is for submission
score
# 0.4702125


# 5. Output model ---------------------------------------------------------

modeler_name <- "toon"
model_type <- "elasticnet"
index <- "1" # improved the lay-out of my script

model_name <- paste(modeler_name, model_type, index, sep = "_")


### a. CSV file for public leaderboard

ID_test <- dummieData[rand > 100, "ID", with = F]
pred_test <- dummieData[rand > 100, "pred", with = F]
submission_data <- data.frame(ID = ID_test, pred = pred_test)
write.csv(submission_data, file =
                paste("finished_models_output/", model_name, "_submission", ".csv", sep = ""))

### b. CSV file for stacking - maybe best to include random variables as well

stacking_data <- data.frame(ID = dummieData$ID,
                            pred = dummieData$pred,
                            rand = dummieData$rand,
                            fold = dummieData$cv_fold)
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
            description = "I used an elasticnet. First I did an optimization via caret.
                              Parameters are alpha = 0.75 and lambda = 0.002, I tested if
                              backwards elimination of features helped, but it didn not.",

            # variables used in the model
            variables = c(),

            # logLoss, based on your CV
            logLoss_estimation = score,

            # predictions --> ALL predictions
            predictions = dummieData[,c("ID", "pred"), with = F],

            # other useful info, can be different for each model
            other = list(model = model,

                        id_data = NULL,

                        remarks = "Model is only for the 5th fold.
                                    Variable plots and coefs can be extracted if you want.")


            )

# save
save(model, file = paste("finished_models_output/", model_name, ".Rdata"))