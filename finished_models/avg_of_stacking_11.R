rm(list = ls()) ; gc()

extc_stacking_01_submission <- read.csv("./finished_models_output/extc_stacking_01_submission.csv")
extc_stacking_02_submission <- read.csv("./finished_models_output/extc_stacking_02_submission.csv")
toon_xgb_stacking_06_submission <- read.csv("./finished_models_output/toon_xgb_stacking_06_submission.csv")
toon_xgb_stacking_07_submission <- read.csv("./finished_models_output/toon_xgb_stacking_07_submission.csv")
toon_xgb_stacking_09_submission <- read.csv("./finished_models_output/toon_xgb_stacking_09_submission.csv")

db <- merge(toon_xgb_stacking_07_submission, toon_xgb_stacking_06_submission, "ID")
db <- merge(db, extc_stacking_02_submission, "ID")
db <- merge(db, toon_xgb_stacking_09_submission, "ID")
names(db) <- c("ID", "predxgb7", "predxgb6", "predextc", "predxgb9")

head(db)

db$PredictedProb <- 0.20 * db$predxgb7 + 0.20 * db$predxgb6 + 0.30 * db$predextc + 0.30 * db$predxgb9

db$predxgb7 <- NULL
db$predxgb6 <- NULL
db$predxgb9 <- NULL
db$predextc <- NULL

write.csv(db, file = "./finished_models_output/avg_of_stacking_11_submission.csv", row.names = F)