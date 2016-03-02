# defines functions for feature engineering
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

}

manual_cleaning <- function(data) {
      # leverage forum insight and some manual explorations
      # see if we continue with this manual approach or if we can automate

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

      # v58 almost equals v100
      data$v58v100_RESLM <-
            predict(lm(v58 ~ v100, data), data) - data$v58
data$v100 <- NULL

      # v72 = v129 + v38 + v62 (Exact)
      data$v72_ISZERO <- ifelse(data$v72 == 0, 1, 0)
      data$v129v72_RATIO <-
            ifelse(
                  data$v72 == 0, mean(data$v129, na.rm = T) / mean(data$v72, na.rm = T),
                  data$v129 / data$v72
            )
      data$v138v72_RATIO <-
            ifelse(
                  data$v72 == 0, mean(data$v138, na.rm = T) / mean(data$v72, na.rm = T),
                  data$v138 / data$v72
            )
      data$v62v72_RATIO <-
            ifelse(
                  data$v72 == 0, mean(data$v162, na.rm = T) / mean(data$v72, na.rm = T),
                  data$v62 / data$v72
            )
data$v62 <- NULL ; data$v38 <- NULL ; data$v129 <- NULL

            return(data)
}

# MCA & PCA can be interesting and will probably be quiker
# Also Multiple Factor Analysis where you group based on hierarchical clustering