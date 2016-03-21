# XGB before stacking -----------------------------------------------------

# Takes a dataset as input,
# returns same dataset, but with out-of-fold XGBoost predictions ;
# no_vars = vars to exclude for fitting
# cv_fold = variable defining the cv folds

fit_xgb_before_stacking <-
      function(data, param, nrounds, cores = 4, no_vars, cv_fold, test) {

            all_var_names <- names(data)
            registerDoParallel(cores = cores)
            data[is.na(data)] <- -999
            n <- length(unique(cv_fold))

            for (fold in 1:n) {
                  dtrain <-
                        xgb.DMatrix(
                              data = data[cv_fold != fold & test != 1, -no_vars, with = F] %>%
                                    data.matrix() %>%
                                    Matrix(sparse = T),
                              label = data[cv_fold != fold & test != 1]$target
                        )

                  model <- xgb.train(params              = param,
                                     data                = dtrain,
                                     nrounds             = nrounds)

                  name <- paste("pred", as.character(fold), sep = "_")
                  pred <-
                        predict(model, newdata = data[,-no_vars, with = F] %>% data.matrix() %>% Matrix(sparse = T))
                  data[,name:= ifelse(data$cv_fold == fold, pred, NA), with = F]
                  no_vars <- c(no_vars, name)

            }
            pred_var_names <- setdiff(names(data), all_var_names)
            data$pred <- rowMeans(data[,pred_var_names,with = F], na.rm = T)
            return(data)
      }