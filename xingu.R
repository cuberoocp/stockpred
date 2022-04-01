source("../lib.R")
source("lib_fea.R")


jsl1 = GET("https://www.jisilu.cn/data/new_stock/hkipo/?___jsl=LST___t=1571721878931")
jsl1 = fromJSON(  content( jsl1,"text") ) 
jsl1 =as.data.table(jsl1$rows$cell)  

tmp = jsl1[,.(code=stock_cd,name=stock_nm,apply_dt2,first_incr_rt = as.numeric(first_incr_rt)
              ,gray_incr_rt = as.numeric(gray_incr_rt)
              ,market
              ,price1=as.numeric(unlist(strsplit(price_range,split='-'))[1])
              ,price2=as.numeric(unlist(strsplit(price_range,split='-'))[2])
#              ,advise =jsl_advise_text
              ,green_rt=as.numeric(sub("%", "",green_rt))/100
              ,above_rt = as.numeric(above_rt)
              ,money = as.numeric(single_draw_money)
              ,pe1 = as.numeric(unlist(strsplit(issue_pe_range,split='-'))[1])
              ,pe2 = as.numeric(unlist(strsplit(issue_pe_range,split='-'))[2])
              ,raise_money1 = as.numeric( unlist(strsplit(raise_money,split='-'))[1])
              ,raise_money2 = as.numeric( unlist(strsplit(raise_money,split='-'))[2])
              ,raise_money
              ,issue_price

              ),by=stock_cd]

tmp = tmp[is.na(price2),price2:=price1]
tmp = tmp[is.na(raise_money2),raise_money1:= as.numeric(raise_money) / as.numeric(issue_price) * price1,]
tmp = tmp[is.na(raise_money2),raise_money2:= as.numeric(raise_money) / as.numeric(issue_price) * price2,]

tmp = tmp[,stock_cd:=NULL]
tmp = tmp[,raise_money:=NULL]
tmp = tmp[,issue_price:=NULL]

tmp = tmp[is.na(pe2),pe2:=pe1]
tmp = tmp[is.na(pe1),pe1:=0]
tmp = tmp[is.na(pe2),pe2:=0]
tmp = tmp[is.na(green_rt),green_rt:=0]

tmp = tmp[,market:=ifelse(market=='主板',1,0)]
#tmp = tmp[,advise:=ifelse(advise=='建议回避',1,
#                          ifelse(advise=='不建议',2,
#                                 ifelse(advise=='尚可申购',3,
#                                        ifelse(advise=='建议申购',4,
#                                               ifelse(advise=='强烈建议',5,0)))
#                                 ))]

quantile(tmp$first_incr_rt,c(0.1,0.9),na.rm = T)

tmp = tmp[first_incr_rt>50,first_incr_rt:=50][first_incr_rt < -15,first_incr_rt:= -15]

tmp = tmp[,fixdt:= format(Sys.Date(), "%Y-%m-%d") ]

xingutrain = fread("data2/xingutrain.csv",encoding = 'UTF-8',colClasses=c(code="string"))

xingutrain = unique(rbind(xingutrain,tmp[is.na(first_incr_rt) == F,]))

xingutrain = xingutrain[order(code,-fixdt)][,head(.SD,1),by=code]

fwrite(xingutrain,"data2/xingutrain.csv",row.names = F,quote = FALSE)

xingutrain = xingutrain[,bili:=price2/price1]
xingutrain = xingutrain[,pe2:=NULL]
xingutrain = xingutrain[,price:=(price2+price1)/2]
xingutrain = xingutrain[,raise:=(raise_money1+raise_money2)/2]
xingutrain = xingutrain[,price1:=NULL]
xingutrain = xingutrain[,price2:=NULL]
xingutrain = xingutrain[,raise_money1:=NULL]
xingutrain = xingutrain[,raise_money2:=NULL]
xingutrain = xingutrain[,fixdt:=NULL]

# xingutrain = unique(fread("data2/xingutrain.csv",encoding = 'UTF-8',colClasses=c(code="string")))
# 
# #xingutrain = xingutrain[code!='08441' | first_incr_rt!=0,]
# fwrite(xingutrain,"data2/xingutrain.csv",row.names = F,quote = FALSE)

dtrain = xgb.DMatrix(data = as.matrix(apply(xingutrain[,6:13,with=F],2,as.numeric)
),label=xingutrain$first_incr_rt)

xgbparams=list( eval_metric= 'mae'
                ,objective = 'reg:linear' 
                ,tree_method = 'hist'
                ,max_bin=16
                ,grow_policy = 'lossguide'
                ,max_leaves=32
                ,max_depth=8
)

cv_model = xgb.cv(data=dtrain 
                        ,params=xgbparams,nfold=5
                        , nrounds = 2000, early_stopping_rounds=100,print_every_n=30
                        ,eta = 0.02,min_child_weight = 50
                        ,maximize = F)


zz_model = xgb.train(data=dtrain 
                           ,params=xgbparams,nrounds = 20
                           ,eta = 0.02,min_child_weight = 40
                           ,maximize = F)

save(zz_model, file="model/zz_model.Rdata")

load("model/zz_model.Rdata")

pred = tmp[is.na(first_incr_rt),]

code1 = '01971'
pred = pred[code==code1, pe1:=20.35][code==code1, above_rt:=96]

code1 = '09989'
pred = pred[code==code1, pe1:=23.24][code==code1, above_rt:=35]

code1 = '01477'
pred = pred[code==code1, pe1:=0][code==code1, above_rt:=1040]

code1 = '06958'
pred = pred[code==code1, pe1:=31.19][code==code1, above_rt:=102]

code1 = '06969'
pred = pred[code==code1, pe1:=22.2][code==code1, above_rt:=76]

code1 = '06978'
pred = pred[code==code1, pe1:=0][code==code1, above_rt:=202]

code1 = '01163'
pred = pred[code==code1, pe1:=22.7][code==code1, above_rt:=8.32]

code1 = '01957'
pred = pred[code==code1, pe1:=13.6][code==code1, above_rt:=8.32]

code1 = '09906'
pred = pred[code==code1, pe1:=16.2][code==code1, above_rt:=8.75]

code1 = '09990'
pred = pred[code==code1, pe1:=58.4][code==code1, above_rt:=15.96]

code1 = '06933'
pred = pred[code==code1, pe1:=12.9][code==code1, above_rt:=4.55]

code1 = '08659'
pred = pred[code==code1, pe1:=12.9][code==code1, above_rt:=10.47]

pred = pred[,bili:=price2/price1]
pred = pred[,pe2:=NULL]
pred = pred[,price:=(price2+price1)/2]
pred = pred[,raise:=(raise_money1+raise_money2)/2]
pred = pred[,price1:=NULL]
pred = pred[,price2:=NULL]
pred = pred[,raise_money1:=NULL]
pred = pred[,raise_money2:=NULL]
pred = pred[,fixdt:=NULL]

predset_lv2 = xgb.DMatrix(data = as.matrix(apply(pred[,6:13,with=F],2,as.numeric)
))
pred = cbind(pred,data.table(score2=predict(zz_model,predset_lv2)))
