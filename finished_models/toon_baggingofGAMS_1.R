rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(mgcv)
library(Metrics) ; library(dplyr)

load("data/featureData.RData")

### create random variable for cv in gam ###
set.seed(666)
data$gam_fold <- ceiling(5 * runif(nrow(data)))

### convert variables to factor ###
data$v66_ORIGINAL <- as.factor(data$v66_ORIGINAL)
data$v110_ORIGINAL <- as.factor(data$v110_ORIGINAL)

### take some dummievariables for simplification of interactions ###
data$v110_ORIGINALB <-
      ifelse(data$v110_ORIGINAL == "B", "yes", "no") %>% as.factor()
data$v66_ORIGINALB <-
      ifelse(data$v66_ORIGINAL == "B", "yes", "no") %>% as.factor()
data$v66_ORIGINALC <-
      ifelse(data$v66_ORIGINAL == "C", "yes", "no") %>% as.factor()

### define the gam formula for fitting our splines model for the fold 1 ridge ###
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

### define the gam formula for fitting our splines model for the fold 2 ridge ###
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

for (ridge_fold in 1:2) {
      for (gam_cv_fold in 1:5) {
            print(paste(
                  "Ridge fold ", as.character(ridge_fold), "out of 2.", sep = " "
            ))
            print(paste(
                  "CV fold ", as.character(gam_cv_fold), "out of 5.", sep = " "
            ))

            train <-
                  data[RIDGE_FOLD == ridge_fold &
                             rand < 100 & gam_fold != gam_cv_fold]
            model <-
                  gam(form[[ridge_fold]], data = train, family = binomial())

            prediction_name <- paste(
                  "pred", "RIDGE_FOLD", as.character(ridge_fold),
                  "gam_fold", as.character(gam_cv_fold), sep = "_"
            )

            data[[prediction_name]] <-
                  predict.gam(model, data, type = "response")
      }
}

### Now put together the different folds to an average ###
ridge_fold_pattern <- c("RIDGE_FOLD_1", "RIDGE_FOLD_2")

# first replace predictions that can't be used by NA
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
                               data$gam_fold == gam_cv_fold, NA, data[[var]])
      }
}

# next take mean per row, ignoring NA's
keep_pattern = "pred"
var_names <- c()
for (pattern in keep_pattern) {
      var_names <- c(var_names, grep(pattern, names(data), value = TRUE))
}
data$pred <- rowMeans(data[,var_names,with = F], na.rm = T)

### test performance - CV style ###
logLoss(data[rand < 100]$target, data[rand < 100]$pred)


##################################
##### Create info over model #####
##################################

# name output list = name_type of model_number
# save the script under the same name
toon_baggingofGAMs_1 <-
      list(
            # name
            modeler_name = "Toon",

            # What kind of model is fit?
            model_type = "Bagging of GAM's",

            # some details about the model (Example tuning parameters)
            description = "Interactions are picked based on suggestions dismo.
            Bagging gave a 0.003 improvement in LogLoss",

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
            logLoss_estimation = 0.4726993,

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

                  id_data = data[,c("rand", "RIDGE_FOLD", "gam_fold"), with = F],

                  remarks = "model is cv fold 5 and only on data RIDGE_FOLD 2"


            )

      )

# save
save(toon_baggingofGAMs_1, file = "finished_models/toon_baggingofGAMs_1.Rdata")
