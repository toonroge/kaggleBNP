# This script does some data manipulation before modeling
# Toon Roge

rm(list = ls()) ; gc()

library(data.table) ; library(caret) ; library(dplyr) ; library(FactoMineR)

set.seed(666)

source("data_manipulation_functions.R")

train <- fread("data/train.csv") ; train$rand <- round(runif(nrow(train)) * 100)
test <- fread("data/test.csv") ; test$target <- 0 ; test$rand <- 101

data <- rbind(train, test, fill = T) ; rm(train, test) ; gc()

cat_vars <- names(data)[!sapply(data, is.numeric)]
for (var in cat_vars){
      data[[var]] <- as.factor(data[[var]])
}
data <- data[, c("ID", cat_vars), with = F]

mca <- MCA(X = data[, -c("ID", "v22"), with = F], ncp = 250, graph = F)

coord <- as.data.frame(mca$ind$coord)
names(coord) <- paste("MCA", gsub(" ", "", names(coord)), sep = "_")
data <- cbind(data, coord)
rm(coord) ; gc()

data$PASTE_v113v56 <- paste(data$v113, data$v56, sep = "_")
data$PASTE_v110v47v79 <- paste(data$v110, data$v47, data$v79, sep = "_")
data$PASTE_v31v3 <- paste(data$v31, data$v3, sep = "_")
data$PASTE_v107v52v91 <- paste(data$v107, data$v52, data$v9, sep = "_")
data$PASTE_v125v112v75v71 <- paste(data$v125, data$v112, data$v75, data$v71, sep = "_")
data$PASTE_v66v30 <- paste(data$v66, data$v30, sep = "_")
data$PASTE_v74v24 <- paste(data$v74, data$v24, sep = "_")
data$PASTE_v22 <- data$v22

# select model variables based on the suffix
keep_pattern <-
      c("ID", "PASTE", "MCA") # to keep
vars_to_keep <- c()
for (pattern in keep_pattern) {
      vars_to_keep <-
            c(vars_to_keep, grep(pattern, names(data), value = TRUE))
}
data <- data[, vars_to_keep, with = F]

save(data, file = "data/featureData5.Rdata")
