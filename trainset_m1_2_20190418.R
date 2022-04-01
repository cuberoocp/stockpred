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
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_1)
cv_model_1b_1_2 = xgb.train(   data = dtrain,  params = xgbparams,   watchlist = list(train = dtrain, test = dtest),
  maximize = F,  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300  ,
  eta = 0.35  ,nrounds = 7100   ,gamma = 2
)

#[7100]	train-mae:0.120388	test-mae:0.173611 
save(cv_model_1b_1_2, file = "model/tmp/model20190418_1b_1_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_1)
cv_model_2b_1_2 = xgb.train(   data = dtrain,  params = xgbparams,   watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7200   ,gamma = 2
)
#[7200]	train-mae:0.110011	test-mae:0.161222 
save(cv_model_2b_1_2, file = "model/tmp/model20190418_2b_1_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_1)
cv_model_3b_1_2 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7200   ,gamma = 2.45
)
#[7100]	train-mae:0.104236	test-mae:0.155502 
save(cv_model_3b_1_2, file = "model/tmp/model20190418_3b_1_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_1)
cv_model_1s_1_2 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6900   ,gamma = 2.65
)
#[6900]	train-mae:0.127715	test-mae:0.181511 
save(cv_model_1s_1_2, file = "model/tmp/model20190418_1s_1_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_1)
cv_model_2s_1_2 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7000   ,gamma = 2.9
)
#[6406]	train-mae:0.132425	test-mae:0.185111
save(cv_model_2s_1_2, file = "model/tmp/model20190418_2s_1_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_1)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_1)
cv_model_3s_1_2 = xgb.train(
  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.9,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 7000 ,gamma = 2.7
)
#[7000]	train-mae:0.122224	test-mae:0.178386 
save(cv_model_3s_1_2, file = "model/tmp/model20190418_3s_1_2.Rdata")

