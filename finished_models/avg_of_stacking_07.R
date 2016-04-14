rm(list = ls()) ; gc()

extc_stacking_01_submission <- read.csv("./finished_models_output/extc_stacking_01_submission.csv")
extc_stacking_02_submission <- read.csv("./finished_models_output/extc_stacking_02_submission.csv")
toon_xgb_stacking_06_submission <- read.csv("./finished_models_output/toon_xgb_stacking_06_submission.csv")
toon_xgb_stacking_07_submission <- read.csv("./finished_models_output/toon_xgb_stacking_07_submission.csv")

db <- merge(toon_xgb_stacking_07_submission, toon_xgb_stacking_06_submission, "ID")
db <- merge(db, extc_stacking_02_submission, "ID")

head(db)

db$PredictedProb <- 0.65 * (0.35 * db$PredictedProb.x + 0.65 * db$PredictedProb.y) + 0.35 * db$PredictedProb

db$PredictedProb.x <- NULL
db$PredictedProb.y <- NULL

write.csv(db, file = "./finished_models_output/avg_of_stacking_07_submission.csv", row.names = F)