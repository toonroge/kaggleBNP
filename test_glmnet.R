library(glmnet) ; library(caret) ; library(doParallel)

# source("data_preparation.R")

keep_pattern = c("STD_NORM_IMP_customRF", "GROUP_LOW_CATEG_n_500_max_50")
vars <- c()
for (pattern in keep_pattern) {
      vars <- c(vars, grep(pattern, names(data), value = TRUE))
}

ignore_pattern = c("GROUP_LOW_CATEG_n_500_max_50_IMP")
ignore_vars <- c()
for (pattern in ignore_pattern) {
      ignore_vars <-
            c(ignore_vars, grep(pattern, names(data), value = TRUE))
}
vars <- vars[!vars %in% ignore_vars]
vars_to_keep <- c("ID", "target", "rand", vars, "MISSING_LABEL")

data <- data[, vars_to_keep, with = F]
dummies <- dummyVars(target ~ ., data = data)
dummieData <-
      predict(dummies, newdata = data) %>% as.data.table() %>% cbind(data[,c("target"), with = F])

no_vars <- c("ID", "target", "rand")
dummieData$fold <- ifelse(dummieData$rand < 50, 1, 2)

registerDoParallel(cores = 4)

for (fold in 1:2){

      glmnet <- cv.glmnet(
            x = as.matrix(dummieData[fold == fold, -no_vars, with = F]),
            y = as.matrix(dummieData[fold == fold, c("target"), with = F]),
            family = "binomial",
            alpha = 0,
            parallel = TRUE
      )

      coefs <- coef.cv.glmnet(glmnet, s="lambda.1se") %>% as.matrix()
      ridge_data <- data.frame(var = row.names(coefs), value = as.numeric(coefs[, 1])) %>% as.data.table()

}


# apply a stringsplit on the value

custom_split <- function(x){
      unlist(strsplit(as.character(x), "[.]"))
}

splitdata <- lapply(as.character(ridge_data$var), custom_split) %>% as.data.frame() %>% t() %>% as.data.frame()
splitdata <- sapply(splitdata, as.character) %>% as.data.table()
ridge_data <- cbind(ridge_data, splitdata) %>% as.data.table()

categorical_vars <- names(data)[!sapply(data, is.numeric)]

for (var in categorical_vars){
      coefs <- unique(ridge_data[V1 == var, 2:4, with = F])
      data[[var]] <-
}

# get categorical variables in data and replace with coefs from ridge_data
# This way we transformed categorical variables into continuous data,
# where we really used the ridge coefficents (giving the marginal effect of the variable)


# https://web.stanford.edu/~hastie/Papers/Glmnet_Vignette.pdf
