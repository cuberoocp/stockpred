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
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_2)
cv_model_1b_2_1 = xgb.train(  data = dtrain,  params = xgbparams,
                        watchlist = list(train = dtrain, test = dtest),maximize = F,  
                        print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
                        eta = 0.35  ,nrounds = 6400   ,gamma = 3.25
)

#[6400]	train-mae:0.262465	test-mae:0.327093 
save(cv_model_1b_2_1, file = "model/tmp/model20190418_1b_2_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_2)
cv_model_2b_2_1 = xgb.train(   data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6700   ,gamma = 3.0
)
#[5696]	train-mae:0.246187	test-mae:0.307394
save(cv_model_2b_2_1, file = "model/tmp/model20190418_2b_2_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_2)
cv_model_3b_2_1 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6700   ,gamma = 3.25
)
#[6700]	train-mae:0.234499	test-mae:0.295503 
save(cv_model_3b_2_1, file = "model/tmp/model20190418_3b_2_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_2)
cv_model_1s_2_1 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6400   ,gamma = 3.2
)
#[5771]	train-mae:0.266828	test-mae:0.331467
save(cv_model_1s_2_1, file = "model/tmp/model20190418_1s_2_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_2)
cv_model_2s_2_1 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6400   ,gamma = 3.55
)
#[6400]	train-mae:0.266379	test-mae:0.333621
save(cv_model_2s_2_1, file = "model/tmp/model20190418_2s_2_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_2)
cv_model_3s_2_1 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6600   ,gamma = 3.25
)
#[6600]	train-mae:0.258037	test-mae:0.324021 
save(cv_model_3s_2_1, file = "model/tmp/model20190418_3s_2_1.Rdata")

