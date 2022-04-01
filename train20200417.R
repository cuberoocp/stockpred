source("lib_fea.R")

list_train <- get.train_20190703()

train <- list_train[[1]]
test <- list_train[[2]]

rm(list_train)

list_train <- get.train2_20190703(gdhs4)

train <- list_train[[1]]
test <- list_train[[2]]

rm(list_train)

list_train <- get.trainlv2_20190703(gdhs4)

train2 <- list_train[[1]]
test <- list_train[[2]]

rm(list_train)
