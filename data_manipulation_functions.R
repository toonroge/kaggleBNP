# defines functions for data cleaning
# Toon Roge

# takes a vector and caps lowest and highest values
cap_vector <- function(x, cap = c(0.02, 0.98)) {
      if (is.numeric(x)) {
            q <- quantile(x, cap, na.rm = TRUE)
            min <- as.numeric(q[1])
            max <- as.numeric(q[2])
            x <- pmin(x, max) ; x <- pmax(x, min)
      }
      return(x)
}

# transforms towards standardnormal, but with first capping low and high levels
std_normal_transform <- function(data, ignore) {
      require(caret) ; require(magrittr)

      # get numeric vars to transform
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      numeric_vars <- numeric_vars[!numeric_vars %in% ignore]

      # cap numeric variables
      new_data <-
            lapply(data[, numeric_vars, with = FALSE], cap_vector) %>% as.data.table()

      # Standnormal transformation
      new_data$rand <- runif(nrow(new_data))
      preProcValues <-
            preProcess(new_data[rand < 0.2, numeric_vars, with = F],
                       method = c("BoxCox", "center", "scale"))
      new_data <-
            predict(preProcValues, new_data) ; gc() ; new_data$rand <-
            NULL

      # change column names + add new features to data
      colnames(new_data) <-
            gsub("_ORIGINAL", "", paste(colnames(new_data), "STD_NORM", sep = "_"))
      return(cbind(data, new_data))
}

# groups low occuring values in categorical variable - for one vector
replace_rare_levels <- function(x, n = 750, max_length = 6) {
      require(magrittr)

      # transform to character
      x <- as.character(x)

      # get max of n and max_length value
      vec <-
            summary(as.factor(x), maxsum = length(x)) %>% sort(decreasing = T)
      value <- max(vec[max_length], n, na.rm = T)

      # replace values with other
      names_to_replace <- names(which(vec < value))
      return(ifelse(x %in% names_to_replace, "Other", x) %>% as.factor())
}

# transforms all categorical variables in a data frame
group_low_categorical <-
      function(data, n = 750, max_length = 6, ignore_pattern = NULL) {
            # get categorical vars to transform
            categorical_vars <-
                  names(data)[!sapply(data, is.numeric)]

            ignore_vars <- c()
            for (pattern in ignore_pattern) {
                  ignore_vars <-
                        c(ignore_vars, grep(pattern, names(data), value = TRUE))
            }

            categorical_vars <-
                  categorical_vars[!categorical_vars %in% ignore_vars]

            # group low-occuring categorical variables in "other"
            data_cat <- data[, categorical_vars, with = F]
            new_data <-
                  lapply(data_cat, replace_rare_levels, n, max_length) %>% as.data.table()

            # change column names + add new features to data
            colnames(new_data) <-
                  gsub(
                        "_ORIGINAL", "", paste(
                              colnames(new_data), "GROUP_LOW_CATEG", "n",
                              as.character(n), "max", as.character(max_length), sep = "_"
                        )
                  )
            return(cbind(data, new_data))
      }

impute_missings_mice <-
      function(data, keep_pattern, method = 'rf', m = 1, maxit = 1, sampsize = 0.025) {
            require(mice) ; require(magrittr)

            var_names <- c()
            for (pattern in keep_pattern) {
                  var_names <- c(var_names, grep(pattern, names(data), value = TRUE))
            }

            data_imputation <- data[, var_names, with = F]

            imp <-
                  mice(
                        data_imputation, m = m, maxit = maxit, method = method, sampsize = sampsize
                  )

            new_data <-
                  complete(imp, 'long', inc = FALSE) %>% as.data.table()

            # change column names + add new features to data
            colnames(new_data) <-
                  paste(colnames(new_data), "IMP", method, sep = "_")
            return(cbind(data, new_data))
      }

impute_missings_caret <-
      function(data, keep_pattern, method = "bagImpute") {
            require(caret) ; require(magrittr)

            var_names <- c()
            for (pattern in keep_pattern) {
                  var_names <- c(var_names, grep(pattern, names(data), value = TRUE))
            }
            data_imputation <- data[, var_names, with = F]

            categorical_vars <-
                  names(data_imputation)[!sapply(data_imputation, is.numeric)]
            data_imputation[,(categorical_vars):= lapply(.SD, as.numeric), .SDcols = categorical_vars]


            # bagging trees imputation
            data_imputation$rand <- runif(nrow(data_imputation))
            preProcValues <-
                  preProcess(data_imputation[rand < 0.15],
                             method = method)
            new_data <-
                  predict(preProcValues, data_imputation) ; gc() ; new_data$rand <-
                  NULL
            new_data <- new_data[,-categorical_vars, with = F]

            # change column names + add new features to data
            colnames(new_data) <-
                  paste(colnames(new_data), "IMP", method, sep = "_")
            return(cbind(data, new_data))
      }


impute_missForest <-
      function(data, keep_pattern) {
            require(missForest) ; require(magrittr) ; require(doParallel)

            keep_pattern = c("GROUP_LOW_CATEG", "STD_NORM")

            var_names <- c()
            for (pattern in keep_pattern) {
                  var_names <- c(var_names, grep(pattern, names(data), value = TRUE))
            }
            data_imputation <- data[, var_names, with = F]

            registerDoParallel(cores = 4)
            rf_imp <- missForest(
                  data_imputation,
                  ntree = 10,
                  maxiter = 1,
                  mtry = 0.25,
                  sampsize = rep(0.025, ncol(data_imputation)),
                  verbose = TRUE,
                  parallelize = "forests"
            )

            newdata <- rf_imp$ximp

      }

impute_customrf <-
      function(data, keep_pattern, prop = 0.20, ntree = 20, colsample = 0.10) {
            require(randomForest) ; require(magrittr) ; require(doParallel)

            # extract data for imputation
            var_names <- c()
            for (pattern in keep_pattern) {
                  var_names <- c(var_names, grep(pattern, names(data), value = TRUE))
            }
            data_imputation <- data[, var_names, with = F]

            # transform categorical vars to numeric IDs
            categorical_vars <-
                  names(data_imputation)[!sapply(data_imputation, is.numeric)]
            data_imputation[,(categorical_vars):= lapply(.SD, as.numeric), .SDcols = categorical_vars]

            # put missing values as -999
            data_imputation[is.na(data_imputation)] <- -999

            # create new data table
            new_data <- data_imputation

            var_names <-
                  which(sapply(data_imputation, function(x)
                        sum(x == -999)) > 0) %>% names()

            for (var in var_names) {
                  # select columns for fitting rf
                  cond <-
                        which(data_imputation[, var, with = F] == -999)
                  cols <-
                        which(colSums(data_imputation[cond] == -999) / nrow(data_imputation) < prop) %>% names()
                  cols <- c(cols, var)
                  # select rows for fitting rf
                  rows <-
                        which(!(data_imputation[, var, with = F] == -999) &
                                    runif(nrow(data_imputation)) < 0.2)

                  rf <- randomForest(
                        x = as.data.frame(data_imputation[rows, cols, with = F]),
                        y = as.data.frame(data_imputation[rows, var, with = F])[[var]],
                        ntree = ntree,
                        mtry = round(length(cols) * colsample)
                  )

                  new_data[[var]] <- ifelse(new_data[[var]] == -999,
                                            predict(rf, new_data),
                                            new_data[[var]])

                  print(paste("variable: ", var))

            }

            # change column names + add new features to data
            colnames(new_data) <-
                  paste(colnames(new_data), "IMP_customRF", sep = "_")
            return(cbind(data, new_data))

      }

missings_per_row <- function(data, ignore_pattern) {
      ignore_vars <- c()
      for (pattern in ignore_pattern) {
            ignore_vars <-
                  c(ignore_vars, grep(pattern, names(data), value = TRUE))
      }
      missData <- data[,-ignore_vars, with = F]

      data$MISSING_COUNT <-
            rowSums(is.na(missData)) / ncol(missData)
      data$MISSING_LABEL <-
            as.factor(as.numeric(as.factor(data$MISSING_COUNT)))
      return(data)
}

flag_original_variables <- function(data, ignore) {
      names(data)[!names(data) %in% ignore] <-
            paste(names(data)[!names(data) %in% ignore], "ORIGINAL", sep = "_")
      return(data)

}

replace_blank_categorical <- function(data) {
      # get categorical vars to transform
      categorical_vars <- names(data)[!sapply(data, is.numeric)]

      for (var in categorical_vars) {
            data[[var]][data[[var]] == ""] <- "MISSING"
      }
      return(data)
}

get_best_feature <- function(target, var1, var2){
      require(randomForest)
      db <- data.frame(target = target, var1 = var1, var2 = var2) %>% na.omit()
      rf1 <- randomForest(y = as.factor(db$target),
                         x = db[,2] %>% as.data.frame(),
                         ntree = 75
                         )
      rf2 <- randomForest(y = as.factor(db$target),
                          x = db[,3] %>% as.data.frame(),
                          ntree = 75
      )
      err2 <- rf2$err.rate[,1] %>% min()
      err1 <- rf1$err.rate[,1] %>% min()
      x <- err2 < err1
      print(paste("Error rates:", as.character(round(err1, 4)), as.character(round(err2, 4))))
      return(x + 1) # returns 2 if second features is best, 1 if first feature is best
}

remove_two_correlated_vars <- function(data, var1, var2) {
      # This function takes data.table and the names of two corr. variables as input
      # next it checks if both variables are present
      # if so it will fit a linear model and remove one variables by the residuals of this linear model
      if (is.null(data[[var1]]) | is.null(data[[var2]])) {
            print("Already deleted one variable")
            print("==================================")
            return(data)
      } else{
            data$x <- data[[var1]]
            data$y <- data[[var2]]
            name <- paste(var1, var2, "_RESLM", sep = "")
            data[[name]] <-
                  predict(lm(x ~ y, data), data) - data$x
            data$x <- NULL ; data$y <- NULL
            best_feature <- get_best_feature(subset(data, rand < 20)$target, subset(data, rand < 20)[[var1]],
                                             subset(data, rand < 20)[[var2]])
            if (best_feature == 2) {
                  data[[var1]] <- NULL
                  print(paste("delete", var1))
            } else{
                  data[[var2]] <- NULL
                  print(paste("delete", var2))
            }
            print("==================================")
            return(data)
      }
}

manual_cleaning <- function(data) {
      ### FIRST: MANUALLY TREAT SOME INSIGHT FROM FORUM ###

      # duplicate variable v91 and v107
      data$v107 <- NULL #keep v91

      # v71 and v75 should be pasted together, because the one is almost perfect subset from the other
      data$v71v75_PASTE <- paste(data$v71, data$v75, sep = "")
      data$v71 <- NULL
      data$v75 <- NULL

      # v50 - 16,78 = 2,041*v10 + -2,779*v12 --> Almost perfect fit with LM
      # v10 + 18.58 = 1.571 * v34 + 0.9936 * v40 --> Almost perfect fir with LM
      # I think ratios should be much more informative in such case
      data$v50v12_LM_RATIO <-
            ((16.78 - data$v50) / (data$v12 * 2.779)) %>% pmax(0) %>% pmin(1)
      data$v34v10_LM_RATIO <-
            ((data$v34 * 1.571) / (data$v10 + 18.58)) %>% pmax(0) %>% pmin(1)
      data$v12 <- NULL ; data$v10 <-NULL
      data$v34 <- NULL ; data$v40 <- NULL

      # v72 = v129 + v38 + v62 (Exact)
      data$v72_ISZERO <- ifelse(data$v72 == 0, 1, 0)
      data$v129v72_EX_RATIO <-
            ifelse(
                  data$v72 == 0, mean(data$v129, na.rm = T) / mean(data$v72, na.rm = T),
                  data$v129 / data$v72
            )
      data$v38v72_EX_RATIO <-
            ifelse(
                  data$v72 == 0, mean(data$v38, na.rm = T) / mean(data$v72, na.rm = T),
                  data$v38 / data$v72
            )
      data$v62v72_EX_RATIO <-
            ifelse(
                  data$v72 == 0, mean(data$v162, na.rm = T) / mean(data$v72, na.rm = T),
                  data$v62 / data$v72
            )
      data$v62 <- NULL ; data$v38 <- NULL ; data$v129 <- NULL

      return(data)

}


removing_correlated_features <- function(data, cor_value = 0.85){
      # variables who are really strongly correlated --> strategy keep 1 + the noise of lm(v1 ~v2)

      # 1. extract numeric variables
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      numeric_vars <- numeric_vars[!numeric_vars %in% c("ID", "target", "rand")]

      # 2. construct vectors with correlated features
      print("Calculation of correlation Matrix")
      corm <- cor(x = data[, numeric_vars, with = F],
                  use = "pairwise.complete.obs")
      cord <-
            melt(corm) %>% dplyr::arrange(desc(abs(value))) %>% dplyr::filter(value < 1 &
                                                                                    value > cor_value)
      var1 <- as.character(cord$Var1) ; var2 <- as.character(cord$Var2)

      # 3. remove correlated features and add the noise as a variable
      if (length(var1) == 0){
            return(data)
      } else{
            for (i in 1:nrow(cord)){
                  print(paste(as.character(var1[i]), as.character(var2[i]), as.character(round(cord$value[i], 4))))
                  data <- data %>%
                        remove_two_correlated_vars(var1[i], var2[i])
            }
            return(data)
      }

}

do_data_cleaning <- function(data, cor_value = 0.8){

      # 1. Manual cleaning
      data <- manual_cleaning(data)

      #2. Automatic removal: if x and y are correlated; replace by x and residuals of lm(y ~ x)
      # --> Use loop, because noise can be explained by other features
      test <- names(data) ; i <- 1 ; print(i)
      data <- removing_correlated_features(data, cor_value = cor_value)
      while(!isTRUE(all.equal(names(data), test))){
            test <- names(data) ; i <- i + 1
            data <- removing_correlated_features(data, cor_value = cor_value) ; print(i)
      }
      return(data)
}

transform_cat_to_ridge_coefs <- function(db, cores = 4, alpha = 0.25,
                                         keep_pattern,
                                         ignore_pattern,
                                         ID = "ID",
                                         random = "rand", # need to be 1-100 for training and > 100 for test
                                         target = "target"){
      # takes a database as input
      # detects the categorical variables in the keep_pattern
      # excluding the ones in ignore_pattern
      # fits elasticnet regression on two folds of the data (75pct ridge as default)
      # returns the input data, but with elasticnet predictions for each fold added
      # + a variable indicating to which fold each observation belonged during the fitting

      require("glmnet")
      require("caret")
      require("doParallel")
      require("reshape2")
      require("data.table")
      require("dplyr")

      vars <- c()
      for (pattern in keep_pattern) {
            vars <- c(vars, grep(pattern, names(db), value = TRUE))
      }

      ignore_vars <- c()
      for (pattern in ignore_pattern) {
            ignore_vars <-
                  c(ignore_vars, grep(pattern, names(db), value = TRUE))
      }
      vars <- vars[!vars %in% ignore_vars]
      vars_to_keep <- c(ID, random, target, vars)

      data <- db[, vars_to_keep, with = F]
      dummies <- dummyVars(target ~ ., data = data)
      dummieData <-
            predict(dummies, newdata = data) %>% as.data.table() %>% cbind(data[,target, with = F])

      no_vars <- c(ID, random, target, "fold")

      dummieData$test <- ifelse(dummieData[[random]] > 100, 1, 0)
      dummieData[[random]] <- ifelse(dummieData[[random]] > 100, 100 * runif(nrow(dummieData)), dummieData[[random]])
      dummieData$fold <- ifelse(dummieData[[random]] < 50, 1, 2)

      registerDoParallel(cores = cores)

      for (fold in 1:2){

            print(paste("Running ridge regression fold:", as.character(fold), sep = " "))

            glmnet <- cv.glmnet(
                  x = as.matrix(dummieData[fold == fold & test == 0, -no_vars, with = F]),
                  y = as.matrix(dummieData[fold == fold & test == 0, c("target"), with = F]),
                  family = "binomial",
                  alpha = alpha,
                  parallel = TRUE
            )

            print("Fold has terminated.")

            coefs <- coef.cv.glmnet(glmnet, s="lambda.min") %>% as.matrix()
            ridge_data <- data.frame(var = row.names(coefs), value = as.numeric(coefs[, 1])) %>% as.data.table()

            keep <- grepl("\\.", as.character(ridge_data$var))
            ridge_data <- ridge_data[keep]
            ridge_data <- cbind(ridge_data, colsplit(as.character(ridge_data$var), "\\.", c("variable", "level")))
            ridge_data$var <- NULL

            categorical_vars <- names(data)[!sapply(data, is.numeric)]

            for (var in categorical_vars){
                  print(paste("replacing the following categorical variable:", var, sep = " "))
                  coef_table <- ridge_data[ridge_data$variable == var, c("value", "level"), with = F]
                  names(coef_table)[names(coef_table) == "value"] = paste(var, "RIDGE_COEF_FOLD", as.character(fold), sep = "_")
                  data <- merge(x = data, y = coef_table, by.x = var, by.y = "level", all.x = T)
            }

      }

      datafold <- dummieData[, c(ID, "fold"), with = F]
      names(datafold)[names(datafold) == "fold"] <- "RIDGE_FOLD"


      keep_pattern = c("RIDGE_COEF_FOLD")
      vars <- c()
      for (pattern in keep_pattern) {
            vars <- c(vars, grep(pattern, names(data), value = TRUE))
      }
      vars <- c(vars, ID)

      data_ridge <- data[, vars, with = F]

      db <- merge(x = db, y = datafold, by = ID, all.x = T)
      db <- merge(x = db, y = data_ridge, by = ID, all.x = T)

      return(db)

}

# from kaggle scripts
na.roughfix2 <- function (object, ...) {
      res <- lapply(object, roughfix)
      structure(res, class = "data.frame", row.names = seq_len(nrow(object)))
}

#from kaggle scripts
roughfix <- function(x) {
      missing <- is.na(x)
      if (!any(missing)) return(x)

      if (is.numeric(x)) {
            x[missing] <- median.default(x[!missing])
      } else if (is.factor(x)) {
            freq <- table(x)
            x[missing] <- names(freq)[which.max(freq)]
      } else {
            stop("na.roughfix only works for numeric or factor")
      }
      x
}

# Convert v22 to hexavigesimal base - from kaggle scripts - not yet used
az_to_int <- function(az) {
      xx <- strsplit(tolower(az), "")[[1]]
      pos <- match(xx, letters[(1:26)])
      result <- sum( pos* 26^rev(seq_along(xx)-1))
      return(result)
}

clust_pca <- function(data, idvar, ignore, n_clust, clust_method = "hclust"){

      require(data.table)
      require(dplyr)
      require(ClustOfVar)
      require(FactoMineR)
      require(stringr)

      # FactoMineR doesn't work with data.table
      data <- as.data.frame(data)
      # select data for pca analysis
      numeric_vars <- names(data)[sapply(data, is.numeric)]
      output_data <- data[!names(data) %in% setdiff(numeric_vars, c(idvar, ignore))]
      data <- data[names(data) %in% numeric_vars]
      # data <- na.omit(data)
      ind.sup <- which(!complete.cases(data))
      id <- data[[idvar]]
      data <- data[!names(data) %in% c(idvar, ignore)]
      row.names(data) <- id

      # define groups of clustered data
      print("Clustering variables...")
      if (clust_method == "kmeans"){
            res <- kmeansvar(na.omit(data), init = n_clust)$var # list, row.names are variables
      } else if (clust_method == "hclust"){
            hclust <- hclustvar(na.omit(data))
            res <- cutreevar(hclust, n_clust)$var # list, row.names are variables
      } else{
            print("clust_method should be kmeans or hclust")
      }

      # do a pca on each cluster - if more then 1 dimension
      print("PCA for each cluster...")
      new_data <- NULL
      colweight <- c()
      for (i in 1:n_clust){
            print(i)
            names <- row.names(res[[i]]) # extract vars in cluster
            n <- length(names)
            if (n > 1){
                  # pca needed
                  pca <- PCA(data[names(data) %in% names], graph = F, ncp = n, ind.sup = ind.sup)
                  n_vars <- which(pca$eig$`cumulative percentage of variance` > 98) %>% min()
                  colweight <- c(colweight,
                                 rep(1 / sqrt(pca$eig$`eigenvalue`[1]), n_vars))
                  # extract vars of pca
                  out1 <- pca$ind$coord
                  out2 <- pca$ind.sup$coord
                  out1 <- out1[, 1:n_vars] %>% as.data.frame()
                  out2 <- out2[, 1:n_vars] %>% as.data.frame()
                  out1$dist <- pca$ind$dist
                  out2$dist <- pca$ind.sup$dist
                  out <- rbind(out1, out2)
                  names(out)[1] <- "Dim.1" # because sometimes name got changed
                  prefix <- paste(clust_method, str_pad(i, 2, pad = "0"), "pca", sep ="_")
                  names(out) <- paste(prefix, names(out), sep = "_")
                  out[[idvar]] <- row.names(out)
            } else{
                  # no pca needed
                  out <- data[names(data) %in% names]
                  colweight <- c(colweight, 1)
                  prefix <- paste(clust_method, str_pad(i, 2, pad = "0"), "org", sep ="_")
                  names(out) <- paste(prefix, names(out), sep = "_")
                  out[[idvar]] <- row.names(out)
            }

            if (is.null(new_data)){
                  new_data <- out
            } else{
                  # cbind did not work well with data.table
                  new_data <- merge(new_data, out, by = idvar, all.x = T, sort = F)
            }
      }

      # add this data to output data table
      output_data <- merge(output_data, new_data, by = idvar, all.x=T)

      # do again a PCA, but on previous results
      print("PCA on previous pca output...")
      keep_pattern <- c("pca_Dim", "_org")
      var_names <- c()
      for (pattern in keep_pattern) {
            var_names <- c(var_names, grep(pattern, names(new_data), value = TRUE))
      }
      new_data <- new_data[names(new_data) %in% var_names]
      pca <- PCA(new_data, col.w = colweight, graph = F, ncp = ncol(new_data), ind.sup = ind.sup)
      n_vars <- which(pca$eig$`cumulative percentage of variance` > 98) %>% min()
      out1 <- as.data.frame(pca$ind$coord)[, 1:n_vars]
      out2 <- as.data.frame(pca$ind$coord)[, 1:n_vars]
      out1$dist <- pca$ind$dist
      out2$dist <- pca$ind$dist
      out <- rbind(out1, out2)
      prefix <- paste("gpca", sep ="_")
      names(out) <- paste(prefix, names(out), sep = "_")
      out[[idvar]] <- row.names(out)

      # add global pca data to output data
      output_data <- merge(output_data, out, by = idvar, all.x=T, sort = F)
      return(as.data.table(output_data))
}


stacking_features <- function(names){

      # put all predictions in df
      for (i in 1:length(names)){
            path <- paste("./finished_models_output/", names[i], "_stacking.csv", sep = "")
            db <- read.csv(path)
            db$rand <- NULL
            db$fold <- NULL
            db$X <- NULL
            names(db)[names(db) != "ID"] <- names[i]
            if (i == 1){
                  out <- db
            } else{
                  out <- merge(out, db, "ID")
            }
      }

      # get some extra features
      out$stacker_pctgsd <- rowSds(out[,names] %>% as.matrix(), na.rm=TRUE) /
                              rowMeans(out[,names] %>% as.matrix(), na.rm=TRUE)
      out$stacker_min <- rowQuantiles(out[,names] %>% as.matrix(), na.rm=TRUE, probs = 0)
      out$stacker_max <- rowQuantiles(out[,names] %>% as.matrix(), na.rm=TRUE, probs = 0)
      out$stacker_median <- rowQuantiles(out[,names] %>% as.matrix(), na.rm=TRUE, probs = 0)
      out$stacker_mean <- rowMeans(out[,names] %>% as.matrix(), na.rm=TRUE)

      return(out)


}

