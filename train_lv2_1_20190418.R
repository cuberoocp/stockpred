trainset_lv2 <- get.lv2.feature1_20200506(train2)

# fwrite(trainset_lv2,"data/trainset_lv2.csv",row.names = F,quote = FALSE)


testset_lv2 <- get.lv2.feature1_20200506(test[is.na(label_3b) == FALSE & is.na(label_3s) == FALSE, ])

# fwrite(testset_lv2,"data/testset_lv2.csv",row.names = F,quote = FALSE)

trainset_lv2 <- trainset_lv2[, label := label_1b * 0.5 + label_2b * 0.3 + label_3b * 0.2,
  by = .(code, date)][label > 0, ]
testset_lv2 <- testset_lv2[, label := label_1b * 0.5 + label_2b * 0.3 + label_3b * 0.2,
  by = .(code, date)][label > 0, ]

xgbparams <- list(
  eval_metric = "mae",
  tree_method = "hist",
  max_bin = 128,
  grow_policy = "lossguide",
  max_depth = 10,
  min_child_weight = 100
)

dtrain <- xgb.DMatrix(data = as.matrix(apply(trainset_lv2[, 9:68, with = F], 2, as.numeric)), label = trainset_lv2$label)
dtest <- xgb.DMatrix(data = as.matrix(  apply(testset_lv2[, 9:68, with = F], 2, as.numeric)), label = testset_lv2$label)
gc()

cv_model_lv2_b_1 <- xgb.train(  data = dtrain,
  params = xgbparams, eta = 0.01, watchlist = list(train = dtrain, test = dtest),
  nrounds = 8000, early_stopping_rounds = 50,
  maximize = F, print_every_n = 30,  gamma = 0.0023
)

#[583]	train-mae:0.017389	test-mae:0.018646
save(cv_model_lv2_b_1, file = "model/cv_model20190418_lv2_b_1.Rdata")


cv_model_lv2_b_1 <- xgb.train(  data = dtrain,
  params = xgbparams, eta = 0.01, watchlist = list(train = dtrain, test = dtest),
  nrounds = 8000, early_stopping_rounds = 50,
  maximize = F, print_every_n = 30,  gamma = 0.0015
)

#[580]	train-mae:0.017388	test-mae:0.018644
save(cv_model_lv2_b_1, file = "model/cv_model20190418_lv2_b_1.Rdata")


trainset_lv2 <- trainset_lv2[, label := label_1s * 0.2 + label_2s * 0.3 + label_3s * 0.5,
  by = .(code, date)][label > 0, ]
testset_lv2 <- testset_lv2[, label := label_1s * 0.2 + label_2s * 0.3 + label_3s * 0.5,
  by = .(code, date)][label > 0, ]


dtrain <- xgb.DMatrix(data = as.matrix(apply(trainset_lv2[, 9:68, with = F], 2, as.numeric)), label = trainset_lv2$label)
dtest <- xgb.DMatrix(data = as.matrix(  apply(testset_lv2[, 9:68, with = F], 2, as.numeric)), label = testset_lv2$label)
gc()

cv_model_lv2_s_1 <- xgb.train(  data = dtrain,
  params = xgbparams, eta = 0.01, watchlist = list(train = dtrain, test = dtest),
  nrounds = 8000, early_stopping_rounds = 50,
  maximize = F, print_every_n = 30,  gamma = 0.0033
)
#[514]	train-mae:0.025491	test-mae:0.028038
save(cv_model_lv2_s_1, file = "model/cv_model20190418_lv2_s_1.Rdata")


cv_model_lv2_s_1 <- xgb.train(  data = dtrain,
  params = xgbparams, eta = 0.01, watchlist = list(train = dtrain, test = dtest),
  nrounds = 8000, early_stopping_rounds = 50,
  maximize = F, print_every_n = 30,  gamma = 0.0022
)
#[510]	train-mae:0.025494	test-mae:0.028040
save(cv_model_lv2_s_1, file = "model/cv_model20190418_lv2_s_1.Rdata")
