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
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_2)
cv_model_1b_2_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6600   ,gamma = 3.2
)
#[6600]	train-mae:0.258327	test-mae:0.327537 
save(cv_model_1b_2_2, file = "model/tmp/model20190418_1b_2_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_2)
cv_model_2b_2_2 = xgb.train(   data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6500   ,gamma = 3.15
)
#[6108]	train-mae:0.234234	test-mae:0.303041

save(cv_model_2b_2_2, file = "model/tmp/model20190418_2b_2_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_2)
cv_model_3b_2_2 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6500   ,gamma = 3.35
)
#[6500]	train-mae:0.221120	test-mae:0.286515 
save(cv_model_3b_2_2, file = "model/tmp/model20190418_3b_2_2.Rdata")



rm(dtrain)
gc()
dtrain = xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_2)
cv_model_1s_2_2 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 300,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6600   ,gamma = 3.2
)
#[6600]	train-mae:0.253495	test-mae:0.328570 
save(cv_model_1s_2_2, file = "model/tmp/model20190418_1s_2_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_2)
cv_model_2s_2_2 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6500   ,gamma = 3.45
)
#[6165]	train-mae:0.255655	test-mae:0.331205
save(cv_model_2s_2_2, file = "model/tmp/model20190418_2s_2_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_2)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_2)
cv_model_3s_2_2 = xgb.train(  data = dtrain,  params = xgbparams,
  watchlist = list(train = dtrain, test = dtest),maximize = F,  
  print_every_n = 500,  base_score = 0.7,early_stopping_rounds = 300,
  eta = 0.35  ,nrounds = 6500   ,gamma = 3.4
)
#[5088]	train-mae:0.251038	test-mae:0.321433
save(cv_model_3s_2_2, file = "model/tmp/model20190418_3s_2_2.Rdata")

