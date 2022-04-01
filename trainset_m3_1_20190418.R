xgbparams = list(
  eval_metric = 'mae'  ,
  objective = 'binary:logistic'  ,
  tree_method = 'hist'  ,
  max_bin = 64  ,
  grow_policy = 'lossguide'  ,
  single_precision_histogram = TRUE,
  max_depth = 9,
  min_child_weight = 2500
)

rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_3)
cv_model_1b_3_1 = xgb.train(  data = dtrain,  params = xgbparams,
                        watchlist = list(train = dtrain, test = dtest),maximize = F,  
                        print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                        eta = 0.35  ,nrounds = 7000   ,gamma = 2.85
)
#[6900]	train-mae:0.303483	test-mae:0.360680 
save(cv_model_1b_3_1, file = "model/tmp/model20190418_1b_3_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_3)
cv_model_2b_3_1 = xgb.train(   data = dtrain,  params = xgbparams,
                               watchlist = list(train = dtrain, test = dtest),maximize = F,  
                               print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                               eta = 0.35  ,nrounds = 7000   ,gamma = 2.8
)
#[6900]	train-mae:0.286418	test-mae:0.341000 
save(cv_model_2b_3_1, file = "model/tmp/model20190418_2b_3_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_3)
cv_model_3b_3_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6800   ,gamma = 3.0
)
#[5173]	train-mae:0.286469	test-mae:0.330102
save(cv_model_3b_3_1, file = "model/tmp/model20190418_3b_3_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_3)
cv_model_1s_3_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 7000   ,gamma = 3.0
)
#[7000]	train-mae:0.302635	test-mae:0.366582 
save(cv_model_1s_3_1, file = "model/tmp/model20190418_1s_3_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_3)
cv_model_2s_3_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 7100   ,gamma = 2.95
)
#[7100]	train-mae:0.299055	test-mae:0.367881 
save(cv_model_2s_3_1, file = "model/tmp/model20190418_2s_3_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_3)
cv_model_3s_3_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 7200   ,gamma = 3
)
#[6635]	train-mae:0.293723	test-mae:0.359667
save(cv_model_3s_3_1, file = "model/tmp/model20190418_3s_3_1.Rdata")

       
