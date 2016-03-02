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

registerDoParallel(cores = 4)

glmnet <- cv.glmnet(
      x = as.matrix(dummieData[rand < 75, -no_vars, with = F]),
      y = as.matrix(dummieData[rand < 75, c("target"), with = F]),
      family = "binomial",
      alpha = 0,
      parallel = TRUE
)

plot(glmnet)

coefs <- coef.cv.glmnet(glmnet, s=c("lambda.1se","lambda.min")) %>% as.matrix()
ridge_data <- data.frame(var = row.names(coefs), value = as.numeric(coefs[, 1]))

# apply a stringsplit on the value

custom_split <- function(x){
      unlist(strsplit(as.character(x), "[.]"))
}

splitdata <- lapply(as.character(ridge_data$var), custom_split) %>% as.data.frame() %>% t() %>% as.data.frame()
row.names(splitdata) <- NULL
ridge_data <- cbind(ridge_data, splitdata)

# get categorical variables in data and replace with coefs from ridge_data
# This way we transformed categorical variables into continuous data,
# where we really used the ridge coefficents (giving the marginal effect of the variable)


# https://web.stanford.edu/~hastie/Papers/Glmnet_Vignette.pdf
