xgbparams = list(
  eval_metric = 'mae'  ,
  objective = 'binary:logistic'  ,
  tree_method = 'hist'  ,
  max_bin = 64  ,
  grow_policy = 'lossguide'  ,
  single_precision_histogram = TRUE,
  max_depth = 9,
  min_child_weight = 1200
)

rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_1)
cv_model_1b_1_1 = xgb.train(
  data = dtrain,  params = xgbparams,  watchlist = list(train = dtrain, test = dtest),maximize = F,
  print_every_n = 500,  base_score = 0.9,  early_stopping_rounds = 300  ,
  eta = 0.35  ,nrounds = 7500   ,gamma = 1.1
)
#[7500]	train-mae:0.126435	test-mae:0.179772 
save(cv_model_1b_1_1, file = "model/tmp/model20190418_1b_1_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_1)
cv_model_2b_1_1 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7700   ,gamma = 2.3
)
#[7700]	train-mae:0.118599	test-mae:0.173286 
save(cv_model_2b_1_1, file = "model/tmp/model20190418_2b_1_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_1)
cv_model_3b_1_1 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7500   ,gamma = 2.0
)
#[7500]	train-mae:0.115044	test-mae:0.168831 
save(cv_model_3b_1_1, file = "model/tmp/model20190418_3b_1_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_1)
cv_model_1s_1_1 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7400   ,gamma = 2.55
)
#[7500]	train-mae:0.134400	test-mae:0.191241 
save(cv_model_1s_1_1, file = "model/tmp/model20190418_1s_1_1.Rdata")

rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_1)
cv_model_2s_1_1 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 500,
  eta = 0.35  ,nrounds = 7100   ,gamma = 2.5
)
#[7100]	train-mae:0.137919	test-mae:0.193312 
save(cv_model_2s_1_1, file = "model/tmp/model20190418_2s_1_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_1)
cv_model_3s_1_1 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7200   ,gamma = 2.4
)
#[6271]	train-mae:0.137417	test-mae:0.187280
save(cv_model_3s_1_1, file = "model/tmp/model20190418_3s_1_1.Rdata")


