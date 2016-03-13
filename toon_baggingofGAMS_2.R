
# 1. Importing info - organizing data -------------------------------------


rm(list = ls()) ; gc()

library(data.table) ; library(caret)
library(Metrics) ; library(dplyr)
library(Matrix) ; library(mgcv)

load("data/featureData.RData")

### create random variable for cv ###
set.seed(666)
data$cv_fold <- ceiling(5 * runif(nrow(data)))


# 2. Model specific features ----------------------------------------------


### convert variables to factor - necessary for GAM ###
data$v66_ORIGINAL <- as.factor(data$v66_ORIGINAL)
data$v110_ORIGINAL <- as.factor(data$v110_ORIGINAL)

### take some dummievariables for simplification of interactions ###
data$v110_ORIGINALB <-
      ifelse(data$v110_ORIGINAL == "B", "yes", "no") %>% as.factor()
data$v66_ORIGINALB <-
      ifelse(data$v66_ORIGINAL == "B", "yes", "no") %>% as.factor()
data$v66_ORIGINALC <-
      ifelse(data$v66_ORIGINAL == "C", "yes", "no") %>% as.factor()


# 3. Apply model ----------------------------------------------------------


### gam formula 1st splines model - notice RIDGE_COEF_FOLD_2 ###
form1 <- target ~ s(v50_STD_NORM_IMP_customRF, by = v66_ORIGINAL) +
      v66_ORIGINAL +
      v110_ORIGINAL +
      s(v79_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_2, by = v66_ORIGINALC, k = 5) +
      v56_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_2:v66_ORIGINALC +
      s(v50v12_LM_RATIO_STD_NORM_IMP_customRF) +
      s(v62v72_EX_RATIO_STD_NORM_IMP_customRF) +
      s(v114_STD_NORM_IMP_customRF) +
      s(v21v14_RESLM_STD_NORM_IMP_customRF) +
      s(v102_STD_NORM_IMP_customRF, v36_STD_NORM_IMP_customRF) +
      s(MISSING_LABEL_RIDGE_COEF_FOLD_2, v115_STD_NORM_IMP_customRF)

### gam formula 2nd splines model - notice RIDGE_COEF_FOLD_1 ###
form2 <- target ~ s(v50_STD_NORM_IMP_customRF, by = v66_ORIGINAL) +
      v66_ORIGINAL +
      v110_ORIGINAL +
      s(v79_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_1, by = v66_ORIGINALC, k = 5) +
      v56_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_1:v66_ORIGINALC +
      s(v50v12_LM_RATIO_STD_NORM_IMP_customRF) +
      s(v62v72_EX_RATIO_STD_NORM_IMP_customRF) +
      s(v114_STD_NORM_IMP_customRF) +
      s(v21v14_RESLM_STD_NORM_IMP_customRF) +
      s(v102_STD_NORM_IMP_customRF, v36_STD_NORM_IMP_customRF) +
      s(MISSING_LABEL_RIDGE_COEF_FOLD_1, v115_STD_NORM_IMP_customRF)

form <- c(form1, form2)

### do the actual fitting of the model ###

for (ridge_fold in 1:2) { # loop over both ridge_regression folds
      for (gam_cv_fold in 1:5) { # build models in CV style - useful for stacking afterwards
            print(paste(
                  "Ridge fold ", as.character(ridge_fold), "out of 2.", sep = " "
            ))
            print(paste(
                  "CV fold ", as.character(gam_cv_fold), "out of 5.", sep = " "
            ))

            train <-
                  data[RIDGE_FOLD == ridge_fold &
                             rand < 100 & cv_fold == gam_cv_fold]
            model <-
                  gam(form[[ridge_fold]], data = train, family = binomial())

            prediction_name <- paste(
                  "pred", "RIDGE_FOLD", as.character(ridge_fold),
                  "cv_fold", as.character(gam_cv_fold), sep = "_"
            )

            data[[prediction_name]] <-
                  predict.gam(model, data, type = "response")
      }
}

### make the predictions ###

# replace predictions that can't be used by NA - (only 4/(2*5) can be used)
ridge_fold_pattern <- c("RIDGE_FOLD_1", "RIDGE_FOLD_2")
for (ridge_fold in 1:2) {
      keep_pattern = ridge_fold_pattern[ridge_fold]
      var_names <- c()

      for (pattern in keep_pattern) {
            var_names <- c(var_names, grep(pattern, names(data), value = TRUE))
      }

      for (var in var_names) {
            gam_cv_fold <- substr(var, nchar(var), nchar(var)) %>% as.numeric()
            data[[var]] <-
                  ifelse(data$RIDGE_FOLD != ridge_fold |
                               data$cv_fold == gam_cv_fold, NA, data[[var]])
      }
}

# next take mean per row, ignoring NA's
keep_pattern = "pred"
var_names <- c()
for (pattern in keep_pattern) {
      var_names <- c(var_names, grep(pattern, names(data), value = TRUE))
}
data$pred <- rowMeans(data[,var_names,with = F], na.rm = T)


# 4. Test model performance -----------------------------------------------


score <- logLoss(data[rand < 100]$target, data[rand < 100]$pred) # CV style - above 100 is for submission
score
# 0.4744475


# 5. Output model ---------------------------------------------------------

modeler_name <- "toon"
model_type <- "baggingofGAMS"
index <- "02" # improved the lay-out of my script

model_name <- paste(modeler_name, model_type, index, sep = "_")


### a. CSV file for public leaderboard

ID_test <- data[rand > 100, "ID", with = F]
pred_test <- data[rand > 100, "pred", with = F]
submission_data <- data.frame(ID = ID_test, pred = pred_test)
write.csv(submission_data, file =
                  paste("finished_models_output/", model_name, "_submission", ".csv", sep = ""))

### b. CSV file for stacking - should contain the CV folds of each prediction

stacking_data <- data.frame(ID = data$ID,
                            pred = data$pred,
                            rand = data$rand,
                            fold = data$cv_fold)
write.csv(stacking_data, file =
                  paste("finished_models_output/", model_name, "_stacking", ".csv", sep = ""))


### c. A list with useful info

# name output list = name_type of model_number
# save the script under the same name
model <-
      list(
            # name
            modeler_name = modeler_name,

            # What kind of model is fit?
            model_type = model_type,

            # some details about the model (Example tuning parameters)
            description = "Interactions are picked based on suggestions dismo.
            Bagging gave a slight improvement in LogLoss",

            # variables used in the model
            variables = c(
                  "v50_STD_NORM_IMP_customRF", "v66_ORIGINAL",
                  "v110_ORIGINAL", "v79_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_1",
                  "v56_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_1",
                  "v50v12_LM_RATIO_STD_NORM_IMP_customRF",
                  "v62v72_EX_RATIO_STD_NORM_IMP_customRF",
                  "v114_STD_NORM_IMP_customRF",
                  "v21v14_RESLM_STD_NORM_IMP_customRF",
                  "v102_STD_NORM_IMP_customRF",
                  "v36_STD_NORM_IMP_customRF",
                  "MISSING_LABEL_RIDGE_COEF_FOLD_1",
                  "v115_STD_NORM_IMP_customRF"
            ),

            # logLoss, based on your CV
            logLoss_estimation = score,

            # predictions --> ALL predictions
            predictions = data[,c("ID", "pred"), with = F],

            # other useful info, can be different for each model
            other = list(
                  form = target ~ s(v50_STD_NORM_IMP_customRF, by = v66_ORIGINAL) +
                        v66_ORIGINAL +
                        v110_ORIGINAL +
                        s(
                              v79_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_1, by = v66_ORIGINALC, k = 5
                        ) +
                        v56_GROUP_LOW_CATEG_n_500_max_50_RIDGE_COEF_FOLD_1:v66_ORIGINALC +
                        s(v50v12_LM_RATIO_STD_NORM_IMP_customRF) +
                        s(v62v72_EX_RATIO_STD_NORM_IMP_customRF) +
                        s(v114_STD_NORM_IMP_customRF) +
                        s(v21v14_RESLM_STD_NORM_IMP_customRF) +
                        s(v102_STD_NORM_IMP_customRF, v36_STD_NORM_IMP_customRF) +
                        s(MISSING_LABEL_RIDGE_COEF_FOLD_1, v115_STD_NORM_IMP_customRF),

                  model = model,

                  id_data = data[,c("rand", "RIDGE_FOLD", "cv_fold"), with = F],

                  remarks = "model stored here is cv fold 5 and only on data RIDGE_FOLD 2"


            )

      )

# save
save(model, file = paste("finished_models_output/", model_name, ".Rdata"))