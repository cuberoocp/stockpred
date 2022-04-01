xgbparams = list(
  eval_metric = 'mae'  ,
  objective = 'binary:logistic'  ,
  tree_method = 'hist'  ,
  max_bin = 64  ,
  grow_policy = 'lossguide'  ,
  single_precision_histogram = TRUE,
  max_depth = 9,
  min_child_weight = 1800
)

rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_4)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_4)
cv_model_1b_4_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.3,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6500   ,gamma = 3.25
)
#[6500]	train-mae:0.282065	test-mae:0.337471 
save(cv_model_1b_4_1, file = "model/tmp/model20190418_1b_4_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_4)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_4)
cv_model_2b_4_1 = xgb.train(   data = dtrain,  params = xgbparams,
                               watchlist = list(train = dtrain, test = dtest),maximize = F,  
                               print_every_n = 500,  base_score = 0.3,early_stopping_rounds = 300,
                               eta = 0.35  ,nrounds = 6800   ,gamma = 2.8
)
#[4758]	train-mae:0.278120	test-mae:0.323514
save(cv_model_2b_4_1, file = "model/tmp/model20190418_2b_4_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_4)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_4)
cv_model_3b_4_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.3,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6600   ,gamma = 3.35
)
#[6600]	train-mae:0.258987	test-mae:0.305964 
save(cv_model_3b_4_1, file = "model/tmp/model20190418_3b_4_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_4)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_4)
cv_model_1s_4_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.3,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6500   ,gamma = 3.4
)
#[5320]	train-mae:0.282475	test-mae:0.339412
save(cv_model_1s_4_1, file = "model/tmp/model20190418_1s_4_1.Rdata")

rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_4)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_4)
cv_model_2s_4_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.3,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6500   ,gamma = 3.65
)
#[5669]	train-mae:0.277945	test-mae:0.336106
save(cv_model_2s_4_1, file = "model/tmp/model20190418_2s_4_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_4)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_4)
cv_model_3s_4_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.3,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6700   ,gamma = 3.6
)
#[6800]	train-mae:0.261417	test-mae:0.322818 
save(cv_model_3s_4_1, file = "model/tmp/model20190418_3s_4_1.Rdata")

