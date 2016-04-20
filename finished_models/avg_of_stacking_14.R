rm(list = ls()) ; gc()

forum_NN_extc <- read.csv("./finished_models_output/addNNLinearFt.csv")
extc_stacking_01_submission <- read.csv("./finished_models_output/extc_stacking_01_submission.csv")
extc_stacking_02_submission <- read.csv("./finished_models_output/extc_stacking_02_submission.csv")
toon_xgb_stacking_06_submission <- read.csv("./finished_models_output/toon_xgb_stacking_06_submission.csv")
toon_xgb_stacking_07_submission <- read.csv("./finished_models_output/toon_xgb_stacking_07_submission.csv")
toon_xgb_stacking_08_submission <- read.csv("./finished_models_output/toon_xgb_stacking_08_submission.csv")
toon_xgb_stacking_09_submission <- read.csv("./finished_models_output/toon_xgb_stacking_09_submission.csv")

db <- merge(toon_xgb_stacking_07_submission, toon_xgb_stacking_06_submission, "ID")
db <- merge(db, extc_stacking_02_submission, "ID")
db <- merge(db, toon_xgb_stacking_08_submission, "ID")
db <- merge(db, toon_xgb_stacking_09_submission, "ID")
db <- merge(db, forum_NN_extc, "ID")
names(db) <- c("ID", "predxgb7", "predxgb6", "predextc", "predxgb8", "predxgb9", "nn_extc_forum")

head(db)

db$PredictedProb <- 0.05 * db$predxgb7 + 0.05 * db$predxgb6 + 0.25 * db$predextc + 0.35 * db$predxgb9 +
                        0.30 * db$nn_extc_forum

db$predxgb7 <- NULL
db$predxgb6 <- NULL
db$predxgb8 <- NULL
db$predxgb9 <- NULL
db$predextc <- NULL
db$nn_extc_forum <- NULL

write.csv(db, file = "./finished_models_output/avg_of_stacking_114_submission.csv", row.names = F)