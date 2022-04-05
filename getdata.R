source("lib_fea.R")

library(Tushare)
api = Tushare::pro_api(token = "79fba1aebdf24a8fc0dd552714d79f6bfa5ca6e0fa742d8f5ab76268")
bar = Tushare::pro_bar(token = "79fba1aebdf24a8fc0dd552714d79f6bfa5ca6e0fa742d8f5ab76268")
index = fread("tradeday.csv")

######################################


lday = 20220401

repeat {
  isready = fread("constant/isready.csv")
  if (min(isready[V2 != 1,]$V1) > lday) {
    break
  }
  getpricebat(index, lday)
}


api(api_name = 'adj_factor',ts_code='689009.SH', trade_date='20220401')

bar(ts_code = '689009.SH',
    start_date = '20220330',
    end_date = as.character(lday),
    adj = "hfq"
)

tmp1 = data.table(api(api_name = 'daily', trade_date = as.character(lday)))
repeat {
  price = unique(rbind(fread('data/pricedata.csv')
                       #,fread('data/price2015.csv')
                       ,fread('data/price2020.csv')
                       #,fread('data/price2000.csv')
                       #,fread('data/price2005.csv')
                       #,fread('data/price2010.csv')
  ))
  
  if ( nrow(tmp1)- nrow(price[trade_date==lday,])<2 ){break}

  getpricebat2(index,lday) 
}

savedata()

#################
list = data.table(api(api_name = "stock_basic", list_status = "L"))
max(as.character(list$list_date))
#'20220331'
fwrite(list[, .(ts_code, symbol, name, list_date, list_status = "L")],
       "constant/c2name.csv", row.names = F, quote = FALSE)

list = data.table(api(api_name = "stock_basic", list_status = "D"))
fwrite(list[, .(ts_code, symbol, name, list_date, list_status = "D")],
       "constant/c2name_d.csv", row.names = F, quote = FALSE)

list = data.table(api(api_name = "stock_basic", list_status = "P"))
fwrite(list[, .(ts_code, symbol, name, list_date, list_status = "P")],
       "constant/c2name_p.csv", row.names = F, quote = FALSE)

################### get date ##################
tradeday0 = data.table(api(api_name = 'trade_cal',exchange_id='', start_date='20211300'))

tradeday = fread("tradeday.csv")

tradeday = merge(tradeday,tradeday0[,.(date=as.integer(as.character(cal_date))
                                       ,is_open=as.integer(is_open))]
                 ,by=c("date","is_open"),all.x = T,all.y = T)

tradeday = tradeday[,.(is_open=min(is_open)),by=date]

tradeday  = tradeday[,t1:=cumsum(is_open)]
tradeday  = tradeday[,t2:=cumsum(sign(date))]

tradeday = tradeday[,did:= 100000+t2]
tradeday = tradeday[,tdid := 9999+t1]

fwrite(tradeday[,.(date,is_open,tdid,did)],"tradeday.csv",row.names = F,quote = FALSE)

################################


########################

index = fread("tradeday.csv")

isready = fread("constant/isready.csv")
isready = merge(index[is_open==1,.(V1=date)],isready,by="V1",all.x = T)
isready = pad.null(isready,0)
fwrite(isready,"constant/isready.csv",row.names = F,quote = FALSE)




installr::updateR()
