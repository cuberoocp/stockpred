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
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_5)
cv_model_1b_5_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6800   ,gamma = 1.5
)
#[6900]	train-mae:0.150124	test-mae:0.198761 
save(cv_model_1b_5_2, file = "model/tmp/model20190418_1b_5_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_5)
cv_model_2b_5_2 = xgb.train(   data = dtrain,  params = xgbparams,
                               watchlist = list(train = dtrain, test = dtest),maximize = F,  
                               print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                               eta = 0.35  ,nrounds = 6900   ,gamma = 1.55
)
#[7000]	train-mae:0.142962	test-mae:0.191633 
save(cv_model_2b_5_2, file = "model/tmp/model20190418_2b_5_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_5)
cv_model_3b_5_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6900   ,gamma = 1.65
)
#[7000]	train-mae:0.138644	test-mae:0.189366 
save(cv_model_3b_5_2, file = "model/tmp/model20190418_3b_5_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_5)
cv_model_1s_5_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6800   ,gamma = 3.35
)
#[6800]	train-mae:0.151134	test-mae:0.199826 
save(cv_model_1s_5_2, file = "model/tmp/model20190418_1s_5_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_5)
cv_model_2s_5_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6900   ,gamma = 3.15
)
#[7000]	train-mae:0.142829	test-mae:0.194364 
save(cv_model_2s_5_2, file = "model/tmp/model20190418_2s_5_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_5)
cv_model_3s_5_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6800   ,gamma = 2.8
)
#[6800]	train-mae:0.128032	test-mae:0.179255 
save(cv_model_3s_5_2, file = "model/tmp/model20190418_3s_5_2.Rdata")
  
