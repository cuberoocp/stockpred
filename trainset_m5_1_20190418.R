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
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_5)
cv_model_1b_5_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6600   ,gamma = 1.75
)
#[6600]	train-mae:0.158434	test-mae:0.197209 
save(cv_model_1b_5_1, file = "model/tmp/model20190418_1b_5_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_5)
cv_model_2b_5_1 = xgb.train(   data = dtrain,  params = xgbparams,
                               watchlist = list(train = dtrain, test = dtest),maximize = F,  
                               print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                               eta = 0.35  ,nrounds = 6800   ,gamma = 1.75
)
#[6800]	train-mae:0.152415	test-mae:0.188163 
save(cv_model_2b_5_1, file = "model/tmp/model20190418_2b_5_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_5)
cv_model_3b_5_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6700   ,gamma = 2.05
)
#[6700]	train-mae:0.150959	test-mae:0.182900 
save(cv_model_3b_5_1, file = "model/tmp/model20190418_3b_5_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_5)
cv_model_1s_5_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6600   ,gamma = 2.05
)
#[6600]	train-mae:0.157542	test-mae:0.197875 
save(cv_model_1s_5_1, file = "model/tmp/model20190418_1s_5_1.Rdata")

rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_5)
cv_model_2s_5_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6600   ,gamma = 2.25
)
#[6600]	train-mae:0.154877	test-mae:0.192402 
save(cv_model_2s_5_1, file = "model/tmp/model20190418_2s_5_1.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_5)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_5)
cv_model_3s_5_1 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.1,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6800   ,gamma = 2.25
)
#[6800]	train-mae:0.145986	test-mae:0.183311 
save(cv_model_3s_5_1, file = "model/tmp/model20190418_3s_5_1.Rdata")
       
