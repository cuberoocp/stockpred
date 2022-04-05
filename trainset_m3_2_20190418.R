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
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_1b > 0, ]$label_1b_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1b > 0, ]$label_1b_3)
cv_model_1b_3_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 6900   ,gamma = 2.65
)
#[6136]	train-mae:0.300841	test-mae:0.362228
save(cv_model_1b_3_2, file = "model/tmp/model20190418_1b_3_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2b > 0, ]$label_2b_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_2b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2b > 0, ]$label_2b_3)
cv_model_2b_3_2 = xgb.train(   data = dtrain,  params = xgbparams,
                               watchlist = list(train = dtrain, test = dtest),maximize = F,  
                               print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                               eta = 0.35  ,nrounds = 7100   ,gamma = 2.65
)
#[7100]	train-mae:0.275287	test-mae:0.338400 
save(cv_model_2b_3_2, file = "model/tmp/model20190418_2b_3_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3b > 0, c(10:55, 86:95, 98:104), with =                                                 F], 2, as.numeric)), label = train[label_3b > 0, ]$label_3b_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3b > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3b > 0, ]$label_3b_3)
cv_model_3b_3_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 7100   ,gamma = 3.15
)
#[6725]	train-mae:0.264135	test-mae:0.318826
save(cv_model_3b_3_2, file = "model/tmp/model20190418_3b_3_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_1s > 0, ]$label_1s_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_1s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_1s > 0, ]$label_1s_3)
cv_model_1s_3_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 7000   ,gamma = 3.05
)
#[7000]	train-mae:0.294238	test-mae:0.365019 
save(cv_model_1s_3_2, file = "model/tmp/model20190418_1s_3_2.Rdata")

rm(dtrain)
gc()
dtrain = xgb.DMatrix(data = as.matrix(apply(train[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_2s > 0, ]$label_2s_3)
dtest = xgb.DMatrix(data = as.matrix(apply(test[label_2s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_2s > 0, ]$label_2s_3)
cv_model_2s_3_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 7000   ,gamma = 3.4
)
#[7000]	train-mae:0.286321	test-mae:0.360583 
save(cv_model_2s_3_2, file = "model/tmp/model20190418_2s_3_2.Rdata")


rm(dtrain)
gc()
dtrain <- xgb.DMatrix(data = as.matrix(apply(train[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                     F], 2, as.numeric)), label = train[label_3s > 0, ]$label_3s_3)
dtest <- xgb.DMatrix(data = as.matrix(apply(test[label_3s > 0, c(10:55, 86:95, 98:104), with =
                                                   F], 2, as.numeric)), label = test[label_3s > 0, ]$label_3s_3)
cv_model_3s_3_2 = xgb.train(  data = dtrain,  params = xgbparams,
                              watchlist = list(train = dtrain, test = dtest),maximize = F,  
                              print_every_n = 500,  base_score = 0.5,early_stopping_rounds = 300,
                              eta = 0.35  ,nrounds = 7100   ,gamma = 3.05
                              )
#[7100]	train-mae:0.269658	test-mae:0.343614 
save(cv_model_3s_3_2, file = "model/tmp/model20190418_3s_3_2.Rdata")
     
