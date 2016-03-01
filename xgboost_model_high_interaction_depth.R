# xgboost model with high interaction depth 
# logistic regression out-of-sample prediction for factor variables with >= 90 levels
# one hot encoding for other factor variables  
# xgboost on resulting numeric matrix

######## TO DO #######
# try base 26 encoding of v22 
# check tree to see how xgboost handles missing values

###### Load libraries and data ###########

# Load libraries
library(caret)

# Load data
setwd("C:/Users/francis/Dropbox/Kaggle/BNP Paribas Claims")
train_df <- read.csv("train.csv", na.strings = c("NA", ""))
test_df <- read.csv("test.csv", na.strings = c("NA", ""))
str(train_df, list.len = nrow(train_df))
str(test_df, list.len = nrow(test_df))
sapply(train_df, summary)
sapply(test_df, summary)
# train and test have same structure, same amount of NAs, same range for numerical variables

# merge train_df and test set (to get same factor levels)
test_df$target <- NA
train_part <- 1 : nrow(train_df)
test_part <- (nrow(train_df) + 1) : (nrow(train_df) + nrow(test_df))
df <- rbind(train_df, test_df)
df <- df[, - 1] #remove ID
str(df, list.len = ncol(df))
rm(test_df)

######### Exploratory data analysis ########

# target variable
summary(train_df$target) # 0 or 1, mean equals 0.76. 
# no missing target values

# factor variables
is_fact <- sapply(df, is.factor)
df_fact <- df[, is_fact]
str(df_fact)
summary(df_fact) 
# v91 and v107 seem identical
# there are missing factor values

# take closer look at v91 and v107
summary(train_df$v91)
summary(train_df$v107)

# remove v107
df$v107 <- NULL


# most numerical variables range from 0 to 20 and have about the same number of Nas

# integer variables. 
summary(train_df$v38)
plot(as.factor(train_df$v38))
table(as.factor(train_df$v38))
plot( as.factor(train_df$target) ~ as.factor(train_df$v38)  )

summary(train_df$v62)
plot(as.factor(train_df$v62))
table(as.factor(train_df$v62))
plot(as.factor(train_df$target) ~ as.factor(train_df$v62))

summary(train_df$v72)
plot(as.factor(train_df$v72))
table(as.factor(train_df$v72))
plot(as.factor(train_df$target) ~ as.factor(train_df$v72))

summary(train_df$v129)
plot(as.factor(train_df$v129))
table(as.factor(train_df$v129))
plot(as.factor(train_df$target) ~ as.factor(train_df$v129))
# GBM should be able to handle nonlinearities
# No missing integer values 

####### Transform many levelled factors to numeric ################

# select factor variables with >= 90 levels
many_levels <- sapply(df, function(z) nlevels(z) >= 90)
df_many <- df[, many_levels]
str(df_many)
names(df_many)

# group low count factor levels for logistic model 
group_levels <- function(var) {
  low_count_levels <- names(table(var))[table(var) < 100]
  column <- as.character(var)
  column[column %in% low_count_levels] <- 'Other'
  column <- factor(column)
  return(column)
}

df_many <- as.data.frame(sapply(df_many, group_levels))
str(df_many)

# check missing values
library(Amelia)
missmap(
 df_many, main = "Missings Map",
 col = c("yellow", "black"), legend = T
)

# one way plots
sapply(df_many[train_part, ], function(z) plot(as.factor(train_df$target) ~ as.factor(is.na(z))))
# missingness is associated with the target value for two variables

# we use separate category 'Missing' for missing values
df_many <- as.data.frame(apply(df_many, 2, as.character), stringsAsFactors = FALSE)
df_many$v22[is.na(df_many$v22)] <- 'Missing'
df_many$v56[is.na(df_many$v56)] <- 'Missing'
df_many$v125[is.na(df_many$v125)] <- 'Missing'
df_many <- data.frame(sapply(df_many, factor))
summary(df_many)

# # split df_many into train_many and test_many 
train_many <- df_many[train_part, ]
test_many <- df_many[test_part, ]
str(train_many)
str(test_many)

# split up train_many in train_many_A and train_many_B
library(caret)
library(mlbench)
set.seed(1)
in_A <- createDataPartition(y = train_df$target,
                                       ## the outcome data are needed
                                       p = .50,
                                       ## The percentage of data in 
                                       ## train set A
                                       list = FALSE)
train_many_A <- train_many[in_A, ]
train_many_B <- train_many[- in_A, ]
str(train_many_A)
str(train_many_B)

# dummify train_many_A and train_many_B (glmnet needs dummified factor variables)
train_many_A_sparse <- sparse.model.matrix(object = ~ ., data = train_many_A)
str(train_many_A_sparse)
train_many_B_sparse <- sparse.model.matrix(object = ~ ., data = train_many_B)
str(train_many_B_sparse)
test_many_sparse <-  sparse.model.matrix(object = ~ ., data = test_many)
str(test_many_sparse)

# logistic regression: lasso (L1) regression
# Owen Zhang recommends: assume sparsity (L1), unless you know you're in a non-sparse domain
# we build two lasso models: one for train_many_A, one for train_many_B
library(glmnet)
input_logistic_train_many_A <- train_many_A_sparse
input_logistic_train_many_B <- train_many_B_sparse
response_logistic_train_many_A <- as.factor(train_df$target[in_A])
response_logistic_train_many_B <- as.factor(train_df$target[- in_A])
model_glmnet_train_many_A <- cv.glmnet(x = input_logistic_train_many_A, y = response_logistic_train_many_A, family = "binomial", alpha = 1, nfolds = 3)
model_glmnet_train_many_B <- cv.glmnet(x = input_logistic_train_many_B, y = response_logistic_train_many_B, family = "binomial", alpha = 1, nfolds = 3)
plot(model_glmnet_train_many_A)
model_glmnet_train_many_A$lambda.min
coef(model_glmnet_train_many_A)
plot(model_glmnet_train_many_B)
model_glmnet_train_many_B$lambda.min
coef(model_glmnet_train_many_B)

# out of fold predictions for model_glmnet_A and model_glmnet_B on training set
predict_train_many_A <- as.vector(predict(model_glmnet_train_many_B, newx = input_logistic_train_many_A, s = model_glmnet_train_many_B$lambda.min))
predict_train_many_B <- as.vector(predict(model_glmnet_train_many_A, newx = input_logistic_train_many_B, s = model_glmnet_train_many_A$lambda.min))
predict_train_many <- rep(NA, length.out = length(train_part))
predict_train_many[in_A] <- predict_train_many_A
predict_train_many[- in_A] <- predict_train_many_B

# out of fold predictions for model_glmnet_A and model_glmnet_B on test set
input_logistic_test_many <- test_many_sparse
predict_test_many <- 0.5 * (
  as.vector(predict(model_glmnet_train_many_A, newx = input_logistic_test_many, s = model_glmnet_train_many_A$lambda.min)) +
    as.vector(predict(model_glmnet_train_many_B, newx = input_logistic_test_many, s = model_glmnet_train_many_B$lambda.min))
)

###### Create variables ##########

# create variable sum_of_zeros (in case of numeric variable)
is_num <- sapply(df[, - 1], is.numeric) # leave out target
df_num <- df[, -1][, is_num]
df$sum_of_zeroes <- rowSums(df_num == 0, na.rm = TRUE)
summary(df$sum_of_zeroes)

# create variable sum_of_NAs
df$sum_of_NAs <- rowSums(is.na(df[, - 1])) # don't count target

############## One hot encoding ############

# select factor variables with number of levels below 90
few_levels <- sapply(df_fact, function(z) nlevels(z) < 90)
df_few <- df_fact[, few_levels]
str(df_few)

# dummify
dummies <- dummyVars(~ ., data = df_few, sep = "_", fullRank = TRUE)
df_few_dum <- predict(dummies, newdata = df_few)

# remove (near) zero variance columns
nzv <- nearZeroVar(df_few_dum, freqCut = 200000 / 200, saveMetrics= TRUE)
nzv[nzv$nzv,]
nzv <- nearZeroVar(df_few_dum, freqCut = 200000 / 200, saveMetrics= FALSE)
df_few_dum2 <- df_few_dum[, -nzv]
str(df_few_dum2)
rm(df_few_dum)

# set NA equal to -1
df_few_dum2[is.na(df_few_dum2)] <- - 1

# remove highly correlated  columns
descrCor <- cor(df_few_dum2)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .999)
colnames(df_few_dum2[, highlyCorDescr])
df_few_dum3 <- df_few_dum2[, - highlyCorDescr]
descrCor <- cor(df_few_dum3)
summary(descrCor[upper.tri(descrCor)])
rm(df_few_dum2)

##### Merge in one large numerical matrix ##############
is_num <- sapply(df[, - 1], is.numeric) # leave out target
df_num <- df[, -1][, is_num]
str(df_num, list.len = ncol(df_num))
num_matrix <- cbind(data.matrix(df_num), df_few_dum3, c(predict_train_many, predict_test_many))
colnames(num_matrix)[ncol(num_matrix)] <- 'fact_many'

# set NA equal to -1
num_matrix[is.na(num_matrix)] <- -1

# some memory cleaning
rm(df, df_fact, df_few, df_many, test_many, train_many, train_many_A, train_many_B)

# remove highly correlated columns
descrCor <- cor(num_matrix)
summary(descrCor[upper.tri(descrCor)])
highlyCorDescr <- findCorrelation(descrCor, cutoff = .999)
num_matrix <- num_matrix[, - highlyCorDescr]

# split in train and test set
train <- num_matrix[train_part, ]
test <- num_matrix[test_part, ]
str(train) 
str(test) 

# remove unnecessary objects in memory
rm(df, df_fact, df_missing, df_num, df_num2, filteredDescr, test_df, train_fact, test_fact, test_NA, df_few_dum3, num_matrix)

###############XGBoost###################
library(xgboost)

# # XGBoost parameters
# # xgboost fitting with arbitrary parameters
xgb_label <- factor(train_df$target, levels = c(0, 1), labels = c('No', 'Yes'))

# # basic xgboost model 
# xgb_params_1 = list(
#   objective = "binary:logistic",                                               # binary classification
#   eta = 0.01,                                                                  # learning rate
#   max.depth = 10,                                                               # max tree depth
#   eval_metric = "logloss",                                                      # evaluation/loss metric
#   nthread = 2,
#   base_score = 0.76
# )
# 
# # fit the model with the arbitrary parameters specified above
# xgb_1 = xgboost(data = train,
#                 label = train_df$target,
#                 params = xgb_params_1,
#                 nrounds = 600,                                                 # max number of trees to build
#                 verbose = TRUE,
#                 print.every.n = 10,
#                 #early.stop.round = 10,                                          # stop if no improvement within 10 trees
#                 nthread = 2,
#                 missing = -1,
#                 nfold = 2,                                                   # number of folds in K-fold
#                 prediction = TRUE,                                           # return the prediction using the final model
#                 stratified = TRUE
# )
# importance <- xgb.importance(feature_names = colnames(train[, -1]), model = xgb_1)
# head(importance, n = 30)

# set up the cross-validated hyper-parameter search
xgb_grid_1 = expand.grid( 
  nrounds = 500, 
  eta = c(0.005, 0.010, 0.015, 0.020),
  max_depth = 11, #11 works nicely
  gamma = 0, 
  colsample_bytree = 0.75,
  min_child_weight = 1
)

# pack the training control parameters
xgb_trcontrol_1 = trainControl(
  method = "cv",
  number = 2, #rerun with number = 5 !!!!!
  verboseIter = TRUE,
  returnData = FALSE,
  returnResamp = "all",                                                        
  classProbs = TRUE,                                                           
  summaryFunction = mnLogLoss
  #allowParallel = TRUE
)

# train the model for each parameter combination in the grid, 
#   using CV to evaluate
xgb_train_3 = train(
  x = train,
  y = xgb_label,
  trControl = xgb_trcontrol_1,
  tuneGrid = xgb_grid_1,
  method = "xgbTree",
  metric = 'logLoss',
  missing = -1
)
xgb_train_3
varImp(xgb_train_3)

### PREDICTION ####
#Make predict
print('Predict...')
  # Save results
  sample_submission <- read.csv("C:/Users/francis/Dropbox/Kaggle/BNP Paribas Claims/sample_submission.csv")
submission <- sample_submission
submission$PredictedProb <- predict(xgb_train_3, test, type = 'prob')[,2]
write.csv(submission, 'xgb_3.csv', row.names = FALSE)
