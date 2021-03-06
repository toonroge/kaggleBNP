rm(list = ls()) ; gc()

extc_stacking_02_submission <- read.csv("./finished_models_output/extc_stacking_02_submission.csv")
toon_xgb_stacking_04_submission <- read.csv("./finished_models_output/toon_xgb_stacking_04_submission.csv")

db <- merge(extc_stacking_02_submission, toon_xgb_stacking_04_submission, "ID")

head(db)

db$PredictedProb <- 0.4 * db$PredictedProb.x + 0.6 * db$PredictedProb.y

db$PredictedProb.x <- NULL
db$PredictedProb.y <- NULL

write.csv(db, file = "./finished_models_output/avg_of_stacking_05_submission.csv", row.names = F)

