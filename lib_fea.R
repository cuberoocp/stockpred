library(data.table)
library(ggplot2)
#library(rowr)
library(forecast)
library(xgboost)
library(dummies)
#library(rJava)
library(sos)
library(httr)
library(jsonlite)
library(XML)
#library(lightgbm)
library(Tushare)
library(stringr)
library(dplyr)

api = Tushare::pro_api(token = '79fba1aebdf24a8fc0dd552714d79f6bfa5ca6e0fa742d8f5ab76268')
bar = Tushare::pro_bar(token = '79fba1aebdf24a8fc0dd552714d79f6bfa5ca6e0fa742d8f5ab76268')

pad.null = function(tableA,padv){
  ## 对null值用padv进行填充
  setDF(tableA)
  tableA[is.na(tableA)]=padv
  tableA[is.null(tableA)]=padv
  setDT(tableA)
}

get.overall = function (hfq, index) {
  overall = fread("data/overall.csv")
  
  hfq_2 = merge(hfq, index[is_open == 1], by = "date")
  hfq_3 = hfq_2[!did %in% overall$did,]
  
  overall_tmp = merge(hfq_3[, .(date, code, close, tdid, did)]
                      , hfq_2[, .(code, closebef = close, tdid = tdid + 3)], by = c("code", "tdid"))
  overall_tmp = overall_tmp[, .(code, did, per = (close - closebef) / closebef)]
  overall_new = overall_tmp[, .(over3avg = as.integer(round(mean(per) *
                                                              10000))
                                ,
                                over3median = as.integer(round(median(per) * 10000))), by = did]
  
  overall_tmp = merge(hfq_3[, .(date, code, close, tdid, did)], hfq_2[, .(code, closebef =
                                                                            close, tdid = tdid + 8)], by = c("code", "tdid"))
  overall_tmp = overall_tmp[, .(code, did, per = (close - closebef) / closebef)]
  overall_new = merge(overall_new, overall_tmp[, .(over8avg = as.integer(round(mean(per) *
                                                                                 10000))
                                                   ,
                                                   over8median = as.integer(round(median(per) * 10000))), by = did]
                      , by = "did")
  
  overall_tmp = merge(hfq_3[, .(date, code, close, tdid, did)], hfq_2[, .(code, closebef =
                                                                            close, tdid = tdid + 21)], by = c("code", "tdid"))
  overall_tmp = overall_tmp[, .(code, did, per = (close - closebef) / closebef)]
  overall_new = merge(overall_new, overall_tmp[, .(over21avg = as.integer(round(mean(per) *
                                                                                  10000))
                                                   ,
                                                   over21median = as.integer(round(median(per) * 10000))), by = did]
                      , by = "did")
  
  overall_tmp = merge(hfq_3[, .(date, code, close, tdid, did)], hfq_2[, .(code, closebef =
                                                                            close, tdid = tdid + 55)], by = c("code", "tdid"))
  overall_tmp = overall_tmp[, .(code, did, per = (close - closebef) / closebef)]
  overall_new = merge(overall_new, overall_tmp[, .(over55avg = as.integer(round(mean(per) *
                                                                                  10000))
                                                   ,
                                                   over55median = as.integer(round(median(per) * 10000))), by = did]
                      , by = "did")
  
  overall_tmp = merge(hfq_3[, .(date, code, close, tdid, did)], hfq_2[, .(code, closebef =
                                                                            close, tdid = tdid + 144)], by = c("code", "tdid"))
  overall_tmp = overall_tmp[, .(code, did, per = (close - closebef) / closebef)]
  overall_new = merge(overall_new, overall_tmp[, .(over144avg = as.integer(round(mean(per) *
                                                                                   10000))
                                                   ,
                                                   over144median = as.integer(round(median(per) * 10000))), by = did]
                      , by = "did")
  
  overall = rbind(overall, overall_new)
  
  fwrite(overall,
         "data/overall.csv",
         row.names = F,
         quote = FALSE)
  
  overall
}


get.f1_20190418 = function(hfq, index, baseid) {
  hfq = merge(hfq, index, by = "date")
  hfq = hfq[order(date)]
  hfq = hfq[, nid := cumsum(is_open), by = code]
  
  basedate = merge(hfq, baseid[, .(code, date)], by = c("date", "code"))
  basedate = basedate[, nid_m := nid]
  
  tmp  = hfq[, .(
    code,
    bclose = close,
    bhigh = high,
    blow = low,
    bdid = did,
    bnid = nid,
    bamount = amount,
    bnid2 = nid,
    bnid3 = nid + 233
  )]
  
  basedate3 = data.table()
  for (i in 0:99) {
    print(paste(i, '/99 ', sep = ''))
    codelist = unique(hfq[, .(code)])[as.integer(code) %% 100 == i, .(code)]
    
    basedate2 = basedate[tmp[code %in% codelist[, code]], .(
      code,
      close,
      high,
      low,
      did,
      date,
      amount,
      nid,
      bclose,
      bhigh,
      blow,
      bdid,
      bnid,
      bamount
    ), on = .(code, nid_m >= bnid2, nid_m < bnid3), allow.cartesian = TRUE
    , nomatch = 0]
    
    basedate2 = basedate2[, minnid := min(bnid), by = .(code, did)]
    
    basedate2 = basedate2[minnid == nid - 232, ]
    
    basedate2 = basedate2[, a233 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 232, a233d := mean(bamount), by =
                            .(code, did)]
    basedate2 = basedate2[bnid == nid - 232, c233 := bclose, ][, c233 :=
                                                                 max(c233, na.rm = T), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 232, c233nid := min(bnid), by =
                            .(code, did)
                          , ][bnid == c233nid, c233d := bclose, ][, c233d :=
                                                                    max(c233d, na.rm = T), by = .(code, did)]
    
    basedate2 = basedate2[bnid >= nid - 143, ]
    
    basedate2 = basedate2[, a144 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 143, a144d := mean(bamount), by =
                            .(code, did)]
    basedate2 = basedate2[bnid == nid - 143, c144 := bclose, ][, c144 :=
                                                                 max(c144, na.rm = T), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 143, c144nid := min(bnid), by =
                            .(code, did)
                          , ][bnid == c144nid, c144d := bclose, ][, c144d :=
                                                                    max(c144d, na.rm = T), by = .(code, did)]
    
    basedate2 = basedate2[bnid >= nid - 88, ]
    
    basedate2 = basedate2[, a89 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 88, a89d := mean(bamount), by =
                            .(code, did)]
    basedate2 = basedate2[bnid == nid - 88, c89 := bclose, ][, c89 := max(c89, na.rm = T), by =
                                                               .(code, did)]
    basedate2 = basedate2[bdid >= did - 88, c89nid := min(bnid), by = .(code, did)
                          , ][bnid == c89nid, c89d := bclose, ][, c89d :=
                                                                  max(c89d, na.rm = T), by = .(code, did)]
    
    basedate2 = basedate2[bnid >= nid - 54, ]
    
    basedate2 = basedate2[, a55 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 54, a55d := mean(bamount), by =
                            .(code, did)]
    basedate2 = basedate2[bnid == nid - 54, c55 := bclose, ][, c55 := max(c55, na.rm = T), by =
                                                               .(code, did)]
    basedate2 = basedate2[bdid >= did - 54, c55nid := min(bnid), by = .(code, did)
                          , ][bnid == c55nid, c55d := bclose, ][, c55d :=
                                                                  max(c55d, na.rm = T), by = .(code, did)]
    
    
    basedate2 = basedate2[bnid >= nid - 33, ]
    
    basedate2 = basedate2[, a34 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 33, a34d := mean(bamount), by =
                            .(code, did)]
    basedate2 = basedate2[bnid == nid - 33, c34 := bclose, ][, c34 := max(c34, na.rm = T), by =
                                                               .(code, did)]
    basedate2 = basedate2[bdid >= did - 33, c34nid := min(bnid), by = .(code, did)
                          , ][bnid == c34nid, c34d := bclose, ][, c34d :=
                                                                  max(c34d, na.rm = T), by = .(code, did)]
    
    
    basedate2 = basedate2[bnid >= nid - 20, ]
    
    basedate2 = basedate2[, a21 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 20, a21d := mean(bamount), by =
                            .(code, did)]
    basedate2 = basedate2[bnid == nid - 20, c21 := bclose, ][, c21 := max(c21, na.rm = T), by =
                                                               .(code, did)]
    basedate2 = basedate2[bdid >= did - 20, c21nid := min(bnid), by = .(code, did)
                          , ][bnid == c21nid, c21d := bclose, ][, c21d :=
                                                                  max(c21d, na.rm = T), by = .(code, did)]
    
    basedate2 = basedate2[bnid >= nid - 12, ]
    
    basedate2 = basedate2[, a13 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 12, a13d := mean(bamount), by =
                            .(code, did)]
    basedate2 = basedate2[bnid == nid - 12, c13 := bclose, ][, c13 := max(c13, na.rm = T), by =
                                                               .(code, did)]
    basedate2 = basedate2[bdid >= did - 12, c13nid := min(bnid), by = .(code, did)
                          , ][bnid == c13nid, c13d := bclose, ][, c13d :=
                                                                  max(c13d, na.rm = T), by = .(code, did)]
    
    basedate2 = basedate2[bnid >= nid - 7, ]
    
    basedate2 = basedate2[, a8 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 7, a8d := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bnid == nid - 7, c8 := bclose, ][, c8 := max(c8, na.rm = T), by =
                                                             .(code, did)]
    basedate2 = basedate2[bdid >= did - 7, c8nid := min(bnid), by = .(code, did)
                          , ][bnid == c8nid, c8d := bclose, ][, c8d := max(c8d, na.rm = T), by =
                                                                .(code, did)]
    
    basedate2 = basedate2[bnid >= nid - 4, ]
    
    basedate2 = basedate2[, a5 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[, h5 := max(bhigh), by = .(code, did)]
    basedate2 = basedate2[, l5 := min(blow), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 4, a5d := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bnid == nid - 4, c5 := bclose, ][, c5 := max(c5, na.rm = T), by =
                                                             .(code, did)]
    basedate2 = basedate2[bdid >= did - 4, c5nid := min(bnid), by = .(code, did)
                          , ][bnid == c5nid, c5d := bclose, ][, c5d := max(c5d, na.rm = T), by =
                                                                .(code, did)]
    basedate2 = basedate2[bnid >= nid - 2, ]
    
    basedate2 = basedate2[, a3 := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[, h3 := max(bhigh), by = .(code, did)]
    basedate2 = basedate2[, l3 := min(blow), by = .(code, did)]
    basedate2 = basedate2[bdid >= did - 2, a3d := mean(bamount), by = .(code, did)]
    basedate2 = basedate2[bnid == nid - 2, c3 := bclose, ][, c3 := max(c3, na.rm = T), by =
                                                             .(code, did)]
    basedate2 = basedate2[bdid >= did - 2, c3nid := min(bnid), by = .(code, did)
                          , ][bnid == c3nid, c3d := bclose, ][, c3d := max(c3d, na.rm = T), by =
                                                                .(code, did)]
    
    basedate2 = basedate2[bnid == nid, ]
    
    basedate2 = basedate2[, .(
      close
      ,
      c3 = as.integer(c3 / close * 1000) ,
      c3d = as.integer(max(c3d / close, 0) * 1000)
      ,
      h3 = as.integer(h3 / close * 1000),
      l3 = as.integer(l3 / close * 1000)
      ,
      c5 = as.integer(c5 / close * 1000) ,
      c5d = as.integer(max(c5d / close, 0) * 1000)
      ,
      h5 = as.integer(h5 / close * 1000),
      l5 = as.integer(l5 / close * 1000)
      ,
      c8 = as.integer(c8 / close * 1000) ,
      c8d = as.integer(max(c8d / close, 0) * 1000)
      ,
      c13 = as.integer(c13 / close * 1000),
      c13d = as.integer(max(c13d / close, 0) * 1000)
      ,
      c21 = as.integer(c21 / close * 1000),
      c21d = as.integer(max(c21d / close, 0) * 1000)
      ,
      c34 = as.integer(c34 / close * 1000),
      c34d = as.integer(max(c34d / close, 0) * 1000)
      ,
      c55 = as.integer(c55 / close * 1000),
      c55d = as.integer(max(c55d / close, 0) * 1000)
      ,
      c89 = as.integer(c89 / close * 1000),
      c89d = as.integer(max(c89d / close, 0) * 1000)
      ,
      c144 = as.integer(c144 / close * 1000),
      c144d = as.integer(max(c144d / close, 0) * 1000)
      ,
      c233 = as.integer(c233 / close * 1000),
      c233d = as.integer(max(c233d / close, 0) * 1000)
      
      ,
      a233,
      a233d = as.integer(max(a233d / a233, 0) * 1000)
      ,
      a144 = as.integer(a144 / a233 * 1000),
      a144d = as.integer(max(a144d / a233, 0) * 1000)
      
      ,
      a89 = as.integer(a89 / a233 * 1000),
      a89d = as.integer(max(a89d / a233, 0) * 1000)
      ,
      a55 = as.integer(a55 / a233 * 1000),
      a55d = as.integer(max(a55d / a233, 0) * 1000)
      ,
      a34 = as.integer(a34 / a233 * 1000),
      a34d = as.integer(max(a34d / a233, 0) * 1000)
      ,
      a21 = as.integer(a21 / a233 * 1000),
      a21d = as.integer(max(a21d / a233, 0) * 1000)
      ,
      a13 = as.integer(a13 / a233 * 1000),
      a13d = as.integer(max(a13d / a233, 0) * 1000)
      ,
      a8 = as.integer(a8 / a233 * 1000),
      a8d = as.integer(max(a8d / a233, 0) * 1000)
      ,
      a5 = as.integer(a5 / a233 * 1000),
      a5d = as.integer(max(a5d / a233, 0) * 1000)
      ,
      a3 = as.integer(a3 / a233 * 1000),
      a3d = as.integer(max(a3d / a233, 0) * 1000)
      ,
      amount = as.integer(amount / a233 * 1000)
    ), by = .(code, did, date)]
    
    
    basedate3 = rbind(basedate3, basedate2)
    
  }
  
  
  out = pad.null(basedate3, 0)
  
}

get.gdhs = function() {
  gdhs = fread("data/gdhs2020.csv", colClasses = list(character = c("code","jzdate","fbdate")))

  gdhs = gdhs[, .(code, holders = as.integer(holders), jzdate, fbdate, pp, pp3)]
  tt = data.table()
  for (i in 1:9) {
    url = paste("http://datacenter-web.eastmoney.com/api/data/v1/get?callback=jQuery112304489919903850479_1632756574951&sortColumns=HOLD_NOTICE_DATE%2CSECURITY_CODE&sortTypes=-1%2C-1&pageSize=500&pageNumber=",
                i,
                "&reportName=RPT_HOLDERNUMLATEST&columns=SECURITY_CODE%2CSECURITY_NAME_ABBR%2CEND_DATE%2CINTERVAL_CHRATE%2CAVG_MARKET_CAP%2CAVG_HOLD_NUM%2CTOTAL_MARKET_CAP%2CTOTAL_A_SHARES%2CHOLD_NOTICE_DATE%2CHOLDER_NUM%2CPRE_HOLDER_NUM%2CHOLDER_NUM_CHANGE%2CHOLDER_NUM_RATIO%2CEND_DATE%2CPRE_END_DATE&quoteColumns=f2%2Cf3&source=WEB&client=WEB"
    ,sep = "")
    r = GET(url)
    t2 = unlist(strsplit(content(r, "text"),split="\\["))[2]
    t2 = unlist(strsplit(t2,split="\\]"))[1]
    if (nchar(t2)>10) {
    tt = rbind(tt,fromJSON(paste('[',t2,']',sep="")))
    }
  }
  gdhs = rbind(gdhs, tt[, .(
    code = SECURITY_CODE,
    holders = HOLDER_NUM,
    jzdate = substr(END_DATE, 1, 10),
    fbdate = substr(HOLD_NOTICE_DATE, 1, 10),
    pp = -1,
    pp3 = -1
  )])
  
  tt = data.table()
  for (i in 1:9) {
    url = paste("http://datacenter-web.eastmoney.com/api/data/v1/get?callback=jQuery112304489919903850479_1632756574951&sortColumns=HOLD_NOTICE_DATE%2CSECURITY_CODE&sortTypes=-1%2C-1&pageSize=500&pageNumber=",
                i,
                "&reportName=RPT_HOLDERNUM_DET&columns=SECURITY_CODE%2CSECURITY_NAME_ABBR%2CEND_DATE%2CINTERVAL_CHRATE%2CAVG_MARKET_CAP%2CAVG_HOLD_NUM%2CTOTAL_MARKET_CAP%2CTOTAL_A_SHARES%2CHOLD_NOTICE_DATE%2CHOLDER_NUM%2CPRE_HOLDER_NUM%2CHOLDER_NUM_CHANGE%2CHOLDER_NUM_RATIO%2CEND_DATE%2CPRE_END_DATE&quoteColumns=f2%2Cf3&source=WEB&client=WEB&filter=(END_DATE%3D%272021-09-30%27)" 
                ,sep = "")
    r = GET(url)
    t2 = unlist(strsplit(content(r, "text"),split="\\["))[2]
    t2 = unlist(strsplit(t2,split="\\]"))[1]
    if (nchar(t2)>10) {
      tt = rbind(tt,fromJSON(paste('[',t2,']',sep="")))
    }
  }

  gdhs = rbind(gdhs, tt[, .(
    code = SECURITY_CODE,
    holders = HOLDER_NUM,
    jzdate = substr(END_DATE, 1, 10),
    fbdate = substr(HOLD_NOTICE_DATE, 1, 10),
    pp = -1,
    pp3 = -1
  )])

  tt = data.table()
  for (i in 1:9) {
    url = paste("https://datacenter-web.eastmoney.com/api/data/v1/get?callback=jQuery1123039306568728517666_1645804862080&sortColumns=HOLD_NOTICE_DATE,SECURITY_CODE&sortTypes=-1,-1&pageSize=50&pageNumber=",
                i,
                "&reportName=RPT_HOLDERNUM_DET&columns=SECURITY_CODE,SECURITY_NAME_ABBR,END_DATE,INTERVAL_CHRATE,AVG_MARKET_CAP,AVG_HOLD_NUM,TOTAL_MARKET_CAP,TOTAL_A_SHARES,HOLD_NOTICE_DATE,HOLDER_NUM,PRE_HOLDER_NUM,HOLDER_NUM_CHANGE,HOLDER_NUM_RATIO,END_DATE,PRE_END_DATE&quoteColumns=f2,f3&source=WEB&client=WEB&filter=(END_DATE='2021-12-31')" 
                ,sep = "")
    r = GET(url)
    t2 = unlist(strsplit(content(r, "text"),split="\\["))[2]
    t2 = unlist(strsplit(t2,split="\\]"))[1]
    if (nchar(t2)>10) {
      tt = rbind(tt,fromJSON(paste('[',t2,']',sep="")))
    }
  }
  gdhs = rbind(gdhs, tt[, .(
    code = SECURITY_CODE,
    holders = HOLDER_NUM,
    jzdate = substr(END_DATE, 1, 10),
    fbdate = substr(HOLD_NOTICE_DATE, 1, 10),
    pp = -1,
    pp3 = -1
  )])

  gdhs = gdhs[fbdate != '' |
                !holders > 0, .(
                  fbdate = min(fbdate, na.rm = T)
                  ,
                  holders = max(holders, na.rm = T)
                  ,
                  pp = max(pp, na.rm = T)
                  ,
                  pp3 = max(pp3, na.rm = T)
                ), by = .(code, jzdate)]
  
  gdhs$holders = as.integer(gdhs$holders)
  
  gdhs = gdhs[holders == pp, pp := NA,]
  gdhs = gdhs[pp == -Inf, pp := NA,]
  gdhs = gdhs[holders == pp3, pp3 := NA,]
  gdhs = gdhs[pp3 == -Inf, pp3 := NA,]
  
  gdhs = gdhs[jzdate>'2020-01-00'][order(code, jzdate, fbdate)]
  
  fwrite(gdhs,
         "data/gdhs2020.csv",
         row.names = F,
         quote = T)
}

get.gdhs4 = function(gdhs,starttdid) {
  tradeday = fread("tradeday.csv")
  
  tradeday = tradeday[, .(date = as.character(as.Date.character(as.character(date), '%Y%m%d')), is_open, tdid, did)]
  
  gdhs = merge(gdhs[holders > 300,], tradeday[, .(jzdate = date ,
                                                  jzdid = did,
                                                  jztdid = tdid)], by = "jzdate")
  
  gdhs = merge(gdhs, tradeday[, .(fbdate = date, fbtdid = tdid)], by = "fbdate", all.x = T)
  
  gdhs = pad.null(gdhs, max(tradeday$tdid))
  
  gdhs2 = gdhs[fbdate != '', .(jzdate = max(jzdate)), by = .(code, fbtdid)]
  
  gdhs2 = merge(gdhs, gdhs2, by = c("code", "jzdate", "fbtdid"))
  
  gdhs2 = gdhs2[order(code, fbdate)][, flag := 1,][, fbid := cumsum(flag), by =
                                                     code]
  
  gdhs2 = gdhs2[order(code, jzdate)][, jzid := cumsum(flag), by = code]
  
  gdhs2 = gdhs2[jzid >= fbid,]
  
  
  
  gdhs2 = merge(gdhs2[, .(code, holders, jzdid)], gdhs[, .(code, holders2 =
                                                             holders, jzdid2 = jzdid)]
                , by = "code", allow.cartesian = T)
  gdhs2 = gdhs2[jzdid2 < jzdid & jzdid2 > jzdid - 390,]
  
  
  
  gdhs2 = gdhs2[, .(
    code,
    holders,
    jzdid,
    ddid = jzdid - jzdid2,
    dholders = holders - holders2,
    pholders = (holders - holders2) / holders2
  )]
  
  gdhs2 = gdhs2[order(code, jzdid,-ddid)]
  
  gdhs3 = merge(gdhs2[, head(.SD, 1), by = .(code, jzdid)][ddid > 330, .(code, holders, jzdid, p3d =
                                                                           dholders, p3p = pholders)]
                , gdhs2[ddid < 235, head(.SD, 1), by = .(code, jzdid)][ddid >
                                                                         130, .(code, holders, jzdid, p2d = dholders, p2p = pholders)]
                , by = c("code", "holders", "jzdid"))
  
  gdhs3 = merge(gdhs3, gdhs2[ddid < 140, head(.SD, 1), by = .(code, jzdid)][ddid >
                                                                              50, .(code, holders, jzdid, p1d = dholders, p1p = pholders)]
                , by = c("code", "holders", "jzdid"))
  
  gdhs4 = merge(
    gdhs,
    gdhs3,
    by = c("code", "jzdid", "holders"),
    allow.cartesian = T
  )
  
  tmp  = merge(gdhs4[, .(code, fbtdid, jztdid)], gdhs4[, .(code, fbtdid2 =
                                                             fbtdid, jztdid2 = jztdid)], by = "code", allow.cartesian = T)
  
  tmp = tmp[jztdid2 >= jztdid,]
  
  tmp  = unique(tmp[fbtdid2 > fbtdid, fbtdid2 := min(fbtdid2), by = .(code, fbtdid)])
  
  tmp = tmp[fbtdid2 >= fbtdid,]
  
  tmp = tmp[, .(fbtdid2 = min(ifelse(fbtdid2 == fbtdid, fbtdid + 140, fbtdid2))), by =
              .(code, fbtdid)]
  
  tmp = tmp[, .(fbtdid = min(fbtdid)), by = .(code, fbtdid2)]
  tmp3 = data.table()
  for (i in 0:19) {
    print(paste(i, '/19 ', sep = ''))
    tmp2 = tmp[as.integer(code) %% 20 == i,]
    tmp2 = merge(tmp2[, .(code, flag = 1, fbtdid, fbtdid2)],
                 unique(tradeday[tdid>starttdid, .(tdid, flag = 1)])
                 ,
                 by = "flag",
                 allow.cartesian = T)
    tmp3 = rbind(tmp3, tmp2[tdid >= fbtdid &
                              tdid < fbtdid2 &
                              tdid < fbtdid + 130,])
  }
  
  
  
  gdhs4 = merge(gdhs4[, .(code, fbtdid, jztdid, p1d, p1p=round(p1p,8), p2d, p2p=round(p2p,8), p3d, p3p=round(p3p,8))],
                tmp3[, .(code, fbtdid, tdid)]
                ,
                by = c("code", "fbtdid"),
                allow.cartesian = T)
  
  gdhs4 = merge(unique(gdhs4), tradeday[is_open == 1, .(tdid, did)], by =
                  "tdid")
  out = gdhs4
}

get.label2_20190418 =function(hfq,index,baseid){
  
  hfq = merge(hfq,index,by="date")
  hfq = hfq[, nid := cumsum(is_open), by = code]
  hfq = hfq[,.(date,code,open,ting,tdid,did,nid)]
  
  basedate = hfq[baseid,on=.(code,did),]
  basedate = basedate[,nid_1:=nid+1]
  basedate = basedate[,nid_18:=nid+18]
  basedate = basedate[,did_180:=did+180]
  basedate = basedate[,open:=NULL]
  basedate = basedate[,ting:=NULL]
  
  tmp  = hfq[,.(code,bopen=open,bdid=did,bnid=nid,ting)]
  
  basedate2 = basedate[tmp,.(code,did,bopen,bnid,ting),on=.(code,nid_1<=bnid,nid_18>=bnid,did_180>=bdid)
                               ,allow.cartesian=TRUE , nomatch=0
                        ]
  
  basedate2 = basedate2[,cnt:=.N,by=.(code,did)][cnt==18,][,cnt:=NULL]
  
  basedate3 = rbind(basedate2,basedate2[order(code,did,-bnid)][,head(.SD,1),by=.(code,did)][,.(code,did,bopen,bnid,ting=0)])
  
  basedate3 = basedate3[,buy_min:=min(ifelse(ting<1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,buy_mean:=mean(ifelse(ting<1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,sell_max:=max(ifelse(ting>-1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,sell_mean:=mean(ifelse(ting>-1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  
  
  label_m3b = basedate3[order(code,did,bnid)][ting<1,head(.SD,1),by=.(code,did)][,.(code,did,label_3b=(buy_min+buy_mean)/bopen/2)]
  label_m3s = basedate3[order(code,did,bnid)][ting>-1,head(.SD,1),by=.(code,did)][,.(code,did,label_3s=(sell_max+sell_mean)/bopen/2)]
  
  basedate = basedate[,nid_18:=NULL]
  basedate = basedate[,nid_10:=nid+10]

  basedate2 = basedate[tmp,.(code,did,bopen,bnid,ting),on=.(code,nid_1<=bnid,nid_10>=bnid,did_180>=bdid)
                       ,allow.cartesian=TRUE , nomatch=0
                       ]
  
  basedate2 = basedate2[,cnt:=.N,by=.(code,did)][cnt==10,][,cnt:=NULL]
  
  basedate3 = rbind(basedate2,basedate2[order(code,did,-bnid)][,head(.SD,1),by=.(code,did)][,.(code,did,bopen,bnid,ting=0)])
  
  basedate3 = basedate3[,buy_min:=min(ifelse(ting<1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,buy_mean:=mean(ifelse(ting<1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,sell_max:=max(ifelse(ting>-1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,sell_mean:=mean(ifelse(ting>-1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  
  
  label_m2b = basedate3[order(code,did,bnid)][ting<1,head(.SD,1),by=.(code,did)][,.(code,did,label_2b=(buy_min+buy_mean)/bopen/2)]
  label_m2s = basedate3[order(code,did,bnid)][ting>-1,head(.SD,1),by=.(code,did)][,.(code,did,label_2s=(sell_max+sell_mean)/bopen/2)]
  
  
  basedate = basedate[,nid_10:=NULL]
  basedate = basedate[,nid_4:=nid+4]
  
  basedate2 = basedate[tmp,.(code,did,bopen,bnid,ting),on=.(code,nid_1<=bnid,nid_4>=bnid,did_180>=bdid)
                       ,allow.cartesian=TRUE , nomatch=0
                       ]
  
  basedate2 = basedate2[,cnt:=.N,by=.(code,did)][cnt==4,][,cnt:=NULL]
  
  basedate3 = rbind(basedate2,basedate2[order(code,did,-bnid)][,head(.SD,1),by=.(code,did)][,.(code,did,bopen,bnid,ting=0)])
  
  basedate3 = basedate3[,buy_min:=min(ifelse(ting<1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,buy_mean:=mean(ifelse(ting<1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,sell_max:=max(ifelse(ting>-1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  basedate3 = basedate3[,sell_mean:=mean(ifelse(ting>-1,bopen,NA),na.rm = TRUE),by=.(code,did)]
  
  
  label_m1b = basedate3[order(code,did,bnid)][ting<1,head(.SD,1),by=.(code,did)][,.(code,did,label_1b=(buy_min+buy_mean)/bopen/2)]
  label_m1s = basedate3[order(code,did,bnid)][ting>-1,head(.SD,1),by=.(code,did)][,.(code,did,label_1s=(sell_max+sell_mean)/bopen/2)]
  
  out = merge(label_m1b,label_m1s,by=c('code','did'),all.x=T,all.y=T)
  out = merge(out,label_m2b,by=c('code','did'),all.x=T)
  out = merge(out,label_m2s,by=c('code','did'),all.x=T)
  out = merge(out,label_m3b,by=c('code','did'),all.x=T)
  out = merge(out,label_m3s,by=c('code','did'),all.x=T)
  
}

get.label2_20201127 =function(hfq,index,baseid){
  
  hfq = merge(hfq,index[did %in% baseid[,did],],by="date")
  hfq = hfq[, nid := cumsum(is_open), by = code]
  hfq = hfq[,.(date,code,open,ting,tdid,did,nid)]
  
  basedate = hfq[baseid,on=.(code,did),]
  basedate = basedate[,nid_1:=nid+1]
  basedate = basedate[,nid_22:=nid+22]
  basedate = basedate[,did_120:=did+120]
  basedate = basedate[,open:=NULL]
  basedate = basedate[,ting:=NULL]
  
  tmp  = hfq[,.(code,bopen=open,bdid=did,bnid=nid,ting)]
  
  final = data.table()
  for (i in 0:99) {
    print(paste(i, '/99 ', sep = ''))
    codelist = unique(hfq[, .(code)])[as.integer(code) %% 100 == i, .(code)]
  
  basedate2 = basedate[tmp[code %in% codelist[, code]],.(code,did,bopen,bnid,ting),on=.(code,nid_1<=bnid,nid_22>=bnid,did_120>=bdid)
                       ,allow.cartesian=TRUE , nomatch=0]
  
  basedate2 = basedate2[order(code,did,bnid)]
  
  basedate2 = basedate2[,allting:=sum(ting),by=.(code,did)]
  
  basestep1 = basedate2[allting >= 4,head(.SD,1),by=.(code,did)]
  basestep2 = basedate2[allting < 4 & allting > -4,]
  basestep3 = basedate2[allting <= -4,head(.SD,1),by=.(code,did)]
  
  basedate3 = basestep2[,head(.SD,16),by=.(code,did)][,cnt:=.N,by=.(code,did)][cnt==16,][,cnt:=NULL]
  basedate3 = basedate3[,tail(.SD,15),by=.(code,did)]

  basedate3 = basedate3[,meanx:=mean(bopen),by=.(code,did)]
  basedate3 = basedate3[,maxx:=max(bopen),by=.(code,did)]
  
  label_m3s = merge(basestep2[ting>-1,head(.SD,1),by=.(code,did)], basedate3[,head(.SD,1),by=.(code,did)][,.(code,did,meanx,maxx)]
                    ,by = c("code","did"))[,.(code,did,label_3s=round((maxx+meanx)*(1+allting*0.002)/bopen/2,3))]

  label_m3s = rbind(label_m3s[,label_3s:=max(0.9,min(label_3s,1.15)),by=.(code,did)]
                    ,basestep1[,.(code,did,label_3s=1.15)]
                    ,basestep3[,.(code,did,label_3s=0.9)])
  
  basedate2 = basedate2[,head(.SD,15),by=.(code,did)][,allting:=sum(ting),by=.(code,did)]
  
  basestep1 = basedate2[allting >= 4,head(.SD,1),by=.(code,did)]
  basestep2 = basedate2[allting < 4 & allting > -4,]
  basestep3 = basedate2[allting <= -4,head(.SD,1),by=.(code,did)]
  
  basedate3 = basestep2[,head(.SD,11),by=.(code,did)][,cnt:=.N,by=.(code,did)][cnt==11,][,cnt:=NULL]
  basedate3 = basedate3[,tail(.SD,10),by=.(code,did)]
  
  basedate3 = basedate3[,minx:=min(bopen),by=.(code,did)]
  basedate3 = basedate3[,meanx:=mean(bopen),by=.(code,did)]
  
  label_m3b = merge(basestep2[ting<1,head(.SD,1),by=.(code,did)], basedate3[,head(.SD,1),by=.(code,did)][,.(code,did,minx,meanx)]
                    ,by = c("code","did"))[,.(code,did,label_3b=round((minx+meanx)*(1+allting*0.002)/bopen/2,3))]
  
  label_m3b = rbind(label_m3b[,label_3b:=min(1.1,max(label_3b,0.85)),by=.(code,did)]
                    ,basestep1[,.(code,did,label_3b=1.1)]
                    ,basestep3[,.(code,did,label_3b=0.85)])
  

  basedate2 = basedate2[,head(.SD,12),by=.(code,did)][,allting:=sum(ting),by=.(code,did)]
  
  basestep1 = basedate2[allting >= 3,head(.SD,1),by=.(code,did)]
  basestep2 = basedate2[allting < 3 & allting > -3,]
  basestep3 = basedate2[allting <= -3,head(.SD,1),by=.(code,did)]
  
  basedate3 = basestep2[,head(.SD,9),by=.(code,did)][,cnt:=.N,by=.(code,did)][cnt==9,][,cnt:=NULL]
  basedate3 = basedate3[,tail(.SD,8),by=.(code,did)]
  
  basedate3 = basedate3[,meanx:=mean(bopen),by=.(code,did)]
  basedate3 = basedate3[,maxx:=max(bopen),by=.(code,did)]
  
  label_m2s = merge(basestep2[ting>-1,head(.SD,1),by=.(code,did)], basedate3[,head(.SD,1),by=.(code,did)][,.(code,did,meanx,maxx)]
                    ,by = c("code","did"))[,.(code,did,label_2s=round((maxx+meanx)*(1+allting*0.002)/bopen/2,3))]
  
  label_m2s = rbind(label_m2s[,label_2s:=max(0.91,min(label_2s,1.14)),by=.(code,did)]
                    ,basestep1[,.(code,did,label_2s=1.14)]
                    ,basestep3[,.(code,did,label_2s=0.91)])
  

  basedate2 = basedate2[,head(.SD,9),by=.(code,did)][,allting:=sum(ting),by=.(code,did)]
  
  basestep1 = basedate2[allting >= 3,head(.SD,1),by=.(code,did)]
  basestep2 = basedate2[allting < 3 & allting > -3,]
  basestep3 = basedate2[allting <= -3,head(.SD,1),by=.(code,did)]
  
  basedate3 = basestep2[,head(.SD,6),by=.(code,did)][,cnt:=.N,by=.(code,did)][cnt==6,][,cnt:=NULL]
  basedate3 = basedate3[,tail(.SD,5),by=.(code,did)]
  
  basedate3 = basedate3[,minx:=min(bopen),by=.(code,did)]
  basedate3 = basedate3[,meanx:=mean(bopen),by=.(code,did)]
  
  label_m2b = merge(basestep2[ting<1,head(.SD,1),by=.(code,did)], basedate3[,head(.SD,1),by=.(code,did)][,.(code,did,minx,meanx)]
                    ,by = c("code","did"))[,.(code,did,label_2b=round((minx+meanx)*(1+allting*0.002)/bopen/2,3))]
  
  label_m2b = rbind(label_m2b[,label_2b:=min(1.09,max(label_2b,0.87)),by=.(code,did)]
                    ,basestep1[,.(code,did,label_2b=1.09)]
                    ,basestep3[,.(code,did,label_2b=0.87)])
  
  

  basedate2 = basedate2[,head(.SD,7),by=.(code,did)][,allting:=sum(ting),by=.(code,did)]
  
  basestep1 = basedate2[allting >= 2,head(.SD,1),by=.(code,did)]
  basestep2 = basedate2[allting < 2 & allting > -2,]
  basestep3 = basedate2[allting <= -2,head(.SD,1),by=.(code,did)]
  
  basedate3 = basestep2[,head(.SD,5),by=.(code,did)][,cnt:=.N,by=.(code,did)][cnt==5,][,cnt:=NULL]
  basedate3 = basedate3[,tail(.SD,3),by=.(code,did)]
  
  basedate3 = basedate3[,meanx:=mean(bopen),by=.(code,did)]
  basedate3 = basedate3[,maxx:=max(bopen),by=.(code,did)]
  
  label_m1s = merge(basestep2[ting>-1,head(.SD,1),by=.(code,did)], basedate3[,head(.SD,1),by=.(code,did)][,.(code,did,meanx,maxx)]
                    ,by = c("code","did"))[,.(code,did,label_1s=round((maxx+meanx)*(1+allting*0.002)/bopen/2,3))]
  
  label_m1s = rbind(label_m1s[,label_1s:=max(min(label_1s,1.16),0.89),by=.(code,did)]
                    ,basestep1[,.(code,did,label_1s=1.16)]
                    ,basestep3[,.(code,did,label_1s=0.89)]) 
  

  
  basedate2 = basedate2[,head(.SD,5),by=.(code,did)][,allting:=sum(ting),by=.(code,did)]
  
  basestep1 = basedate2[allting >= 2,head(.SD,1),by=.(code,did)]
  basestep2 = basedate2[allting < 2 & allting > -2,]
  basestep3 = basedate2[allting <= -2,head(.SD,1),by=.(code,did)]
  
  basedate3 = basestep2[,head(.SD,3),by=.(code,did)][,cnt:=.N,by=.(code,did)][cnt==3,][,cnt:=NULL]
  basedate3 = basedate3[,tail(.SD,2),by=.(code,did)]
  
  basedate3 = basedate3[,minx:=min(bopen),by=.(code,did)]
  basedate3 = basedate3[,meanx:=mean(bopen),by=.(code,did)]
  
  label_m1b = merge(basestep2[ting<1,head(.SD,1),by=.(code,did)], basedate3[,head(.SD,1),by=.(code,did)][,.(code,did,minx,meanx)]
                    ,by = c("code","did"))[,.(code,did,label_1b=round((minx+meanx)*(1+allting*0.002)/bopen/2,3))]
  
  label_m1b = rbind(label_m1b[,label_1b:=min(1.09,max(label_1b,0.9)),by=.(code,did)]
                    ,basestep1[,.(code,did,label_1b=1.09)]
                    ,basestep3[,.(code,did,label_1b=0.9)])

  
  out = merge(label_m1b,label_m1s,by=c('code','did'),all.x=T,all.y=T)
  out = merge(out[is.na(code)==F,],label_m2b,by=c('code','did'),all.x=T)
  out = merge(out[is.na(code)==F,],label_m2s,by=c('code','did'),all.x=T)
  out = merge(out[is.na(code)==F,],unique(label_m3b),by=c('code','did'),all.x=T)
  out = merge(out[is.na(code)==F,],unique(label_m3s),by=c('code','did'),all.x=T)
  
  final = rbind(final, out[is.na(code)==F,])
  
  }
  final
  
}

get.train_20190703 = function(){
  
  feature = rbind(
    fread("data2/labelset2000_20201127.csv", colClasses = c(code = "character"))  
    , fread("data2/labelset2005_20201127.csv", colClasses = c(code = "character"))  
    , fread("data2/labelset2010_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2015_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2020_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2022_20201127.csv", colClasses = c(code = "character"))
  )
  
  feature = feature[,label_1b_1:=ifelse(label_1b>=0.9668,1,0),]
  feature = feature[,label_1b_2:=ifelse(label_1b>=0.9872,1,0),]
  feature = feature[,label_1b_3:=ifelse(label_1b>=0.9967,1,0),]
  feature = feature[,label_1b_4:=ifelse(label_1b>=1.0056,1,0),]
  feature = feature[,label_1b_5:=ifelse(label_1b>=1.0235,1,0),]
  #quantile(feature[label_1b>0,]$label_1b,c(0.15,0.35,0.5,0.65,0.85))
  feature = feature[,label_2b_1:=ifelse(label_2b>=0.9477,1,0),]
  feature = feature[,label_2b_2:=ifelse(label_2b>=0.9770,1,0),]
  feature = feature[,label_2b_3:=ifelse(label_2b>=0.9905,1,0),]
  feature = feature[,label_2b_4:=ifelse(label_2b>=1.0026,1,0),]
  feature = feature[,label_2b_5:=ifelse(label_2b>=1.0242,1,0),]
  
  feature = feature[,label_3b_1:=ifelse(label_3b>=0.9261,1,0),]
  feature = feature[,label_3b_2:=ifelse(label_3b>=0.9654,1,0),]
  feature = feature[,label_3b_3:=ifelse(label_3b>=0.9840,1,0),]
  feature = feature[,label_3b_4:=ifelse(label_3b>=1.0003,1,0),]
  feature = feature[,label_3b_5:=ifelse(label_3b>=1.0275,1,0),]
  
  feature = feature[,label_1s_1:=ifelse(label_1s>=0.9696,1,0),]
  feature = feature[,label_1s_2:=ifelse(label_1s>=0.9940,1,0),]
  feature = feature[,label_1s_3:=ifelse(label_1s>=1.0071,1,0),]
  feature = feature[,label_1s_4:=ifelse(label_1s>=1.0214,1,0),]
  feature = feature[,label_1s_5:=ifelse(label_1s>=1.0528,1,0),]
  
  feature = feature[,label_2s_1:=ifelse(label_2s>=0.9765,1,0),]
  feature = feature[,label_2s_2:=ifelse(label_2s>=1.0004,1,0),]
  feature = feature[,label_2s_3:=ifelse(label_2s>=1.0157,1,0),]
  feature = feature[,label_2s_4:=ifelse(label_2s>=1.0336,1,0),]
  feature = feature[,label_2s_5:=ifelse(label_2s>=1.0748,1,0),]
  
  feature = feature[,label_3s_1:=ifelse(label_3s>=0.9728,1,0),]
  feature = feature[,label_3s_2:=ifelse(label_3s>=1.0032,1,0),]
  feature = feature[,label_3s_3:=ifelse(label_3s>=1.0246,1,0),]
  feature = feature[,label_3s_4:=ifelse(label_3s>=1.0503,1,0),]
  feature = feature[,label_3s_5:=ifelse(label_3s>=1.1095,1,0),]
  
  
  periodid = max(feature$did)-200
  
  # feature = feature[,roll:=runif(1),by=.(code,date)]
  # feature = feature[(did-99000)/(periodid-99000)>roll,]
  # feature = feature[,roll:=NULL]
  
  
  train = feature[(did <= periodid & (as.integer(code)+did*77) %% 100 > 2 & (as.integer(code)+did*77) %% 100 < 72)
                  | (did > periodid & (as.integer(code)+did*77) %% 100 >= 28 & (as.integer(code)+did*77) %% 100 < 62),]
  test  = feature[(did <= periodid & (as.integer(code)+did*77) %% 100 <= 2 )
                  | (did > periodid & ((as.integer(code)+did*77) %% 100 < 28 | (as.integer(code)+did*77) %% 100 >= 62)),]
  
  overall = fread('data/overall.csv')
  
  train = merge(train,overall,by="did")
  test = merge(test,overall,by="did")
  
  result = list(train,test)
  
}

get.train2_20190703 = function(gdhs4){
  
  feature = rbind(
    fread("data2/labelset2000_20201127.csv", colClasses = c(code = "character"))  
    , fread("data2/labelset2005_20201127.csv", colClasses = c(code = "character"))  
    , fread("data2/labelset2010_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2015_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2020_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2022_20201127.csv", colClasses = c(code = "character"))
  )
  
  feature = feature[,label_1b_1:=ifelse(label_1b>=0.9668,1,0),]
  feature = feature[,label_1b_2:=ifelse(label_1b>=0.9872,1,0),]
  feature = feature[,label_1b_3:=ifelse(label_1b>=0.9967,1,0),]
  feature = feature[,label_1b_4:=ifelse(label_1b>=1.0056,1,0),]
  feature = feature[,label_1b_5:=ifelse(label_1b>=1.0235,1,0),]
  #quantile(feature[label_1b>0,]$label_1b,c(0.15,0.35,0.5,0.65,0.85))
  feature = feature[,label_2b_1:=ifelse(label_2b>=0.9477,1,0),]
  feature = feature[,label_2b_2:=ifelse(label_2b>=0.9770,1,0),]
  feature = feature[,label_2b_3:=ifelse(label_2b>=0.9905,1,0),]
  feature = feature[,label_2b_4:=ifelse(label_2b>=1.0026,1,0),]
  feature = feature[,label_2b_5:=ifelse(label_2b>=1.0242,1,0),]
  
  feature = feature[,label_3b_1:=ifelse(label_3b>=0.9261,1,0),]
  feature = feature[,label_3b_2:=ifelse(label_3b>=0.9654,1,0),]
  feature = feature[,label_3b_3:=ifelse(label_3b>=0.9840,1,0),]
  feature = feature[,label_3b_4:=ifelse(label_3b>=1.0003,1,0),]
  feature = feature[,label_3b_5:=ifelse(label_3b>=1.0275,1,0),]
  
  feature = feature[,label_1s_1:=ifelse(label_1s>=0.9696,1,0),]
  feature = feature[,label_1s_2:=ifelse(label_1s>=0.9940,1,0),]
  feature = feature[,label_1s_3:=ifelse(label_1s>=1.0071,1,0),]
  feature = feature[,label_1s_4:=ifelse(label_1s>=1.0214,1,0),]
  feature = feature[,label_1s_5:=ifelse(label_1s>=1.0528,1,0),]
  
  feature = feature[,label_2s_1:=ifelse(label_2s>=0.9765,1,0),]
  feature = feature[,label_2s_2:=ifelse(label_2s>=1.0004,1,0),]
  feature = feature[,label_2s_3:=ifelse(label_2s>=1.0157,1,0),]
  feature = feature[,label_2s_4:=ifelse(label_2s>=1.0336,1,0),]
  feature = feature[,label_2s_5:=ifelse(label_2s>=1.0748,1,0),]
  
  feature = feature[,label_3s_1:=ifelse(label_3s>=0.9728,1,0),]
  feature = feature[,label_3s_2:=ifelse(label_3s>=1.0032,1,0),]
  feature = feature[,label_3s_3:=ifelse(label_3s>=1.0246,1,0),]
  feature = feature[,label_3s_4:=ifelse(label_3s>=1.0503,1,0),]
  feature = feature[,label_3s_5:=ifelse(label_3s>=1.1095,1,0),]
  
  
  periodid = max(feature$did)-200
  
  # feature = feature[,roll:=runif(1),by=.(code,date)]
  # feature = feature[(did-99000)/(periodid-99000)>roll,]
  # feature = feature[,roll:=NULL]
  
  
  train = feature[(did <= periodid & (as.integer(code)+did*77) %% 100 > 2 & (as.integer(code)+did*77) %% 100 < 72)
                  | (did > periodid & (as.integer(code)+did*77) %% 100 >= 28 & (as.integer(code)+did*77) %% 100 < 62),]
  test  = feature[(did <= periodid & (as.integer(code)+did*77) %% 100 <= 2 )
                  | (did > periodid & ((as.integer(code)+did*77) %% 100 < 28 | (as.integer(code)+did*77) %% 100 >= 62)),]
  
  overall = fread('data/overall.csv')
  
  train = merge(train,overall,by="did")
  test = merge(test,overall,by="did")
  
  train = merge(train,gdhs4[,.(code,did,jztdid,tdid,p1p,p2p,p3p,p1d,p2d,p3d)],by=c("code","did"))
  train = train[,delay:=tdid-jztdid]
  
  test = merge(test,gdhs4[,.(code,did,jztdid,tdid,p1p,p2p,p3p,p1d,p2d,p3d)],by=c("code","did"))
  test = test[,delay:=tdid-jztdid]
  
  result = list(train,test)
  
}

get.trainlv2_20190703 = function(gdhs4){
  
  feature = rbind(
    fread("data2/labelset2000_20201127.csv", colClasses = c(code = "character"))  
    , fread("data2/labelset2005_20201127.csv", colClasses = c(code = "character"))  
    , fread("data2/labelset2010_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2015_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2020_20201127.csv", colClasses = c(code = "character"))
    , fread("data2/labelset2022_20201127.csv", colClasses = c(code = "character"))
  )
  
  periodid = max(feature$did)-200
  
  train2 = feature[(did <= periodid & (as.integer(code)+did*77) %% 100 >= 72 )
                   | (did > periodid & (as.integer(code)+did*77) %% 100 >= 62),]  
  test  = feature[(did <= periodid & (as.integer(code)+did*77) %% 100 <= 2 )
                  | (did > periodid & (as.integer(code)+did*77) %% 100 < 28),]
  
  overall = fread('data/overall.csv')
  
  train2 = merge(train2,overall,by="did")
  test = merge(test,overall,by="did")
  
  train2 = merge(train2,gdhs4[,.(code,did,jztdid,tdid,p1p,p2p,p3p,p1d,p2d,p3d)],by=c("code","did"))
  train2 = train2[,delay:=tdid-jztdid]
  
  test = merge(test,gdhs4[,.(code,did,jztdid,tdid,p1p,p2p,p3p,p1d,p2d,p3d)],by=c("code","did"))
  test = test[,delay:=tdid-jztdid]
  
  result = list(train2,test)
  
}

get.lv2.feature1_20200506 = function (train2){
  
  dtrain_lv2_1 <- xgb.DMatrix(data = as.matrix(apply(train2[,c(10:55, 56:65),with=F],2,as.numeric)))
  dtrain_lv2_2 <- xgb.DMatrix(data = as.matrix(apply(train2[,c(10:55, 56:65,68:74),with=F],2,as.numeric)))
  
  print("model_1_1")
  
  load("model/model20190418_1b_1_1.Rdata")
  trainset_lv2 = cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)],data.table(predict(cv_model_1b_1_1,dtrain_lv2_1)))[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s,p_1b_1_1=V1)]
  
  load("model/model20190418_2b_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_1_1=V1)],by=c("code",'date'))
  
  print("model_2_1")
  
  load("model/model20190418_1b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_2_1=V1)],by=c("code",'date'))
  
  print("model_3_1")
  
  load("model/model20190418_1b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_3_1=V1)],by=c("code",'date'))
  
  print("model_4_1")
  
  load("model/model20190418_1b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_4_1=V1)],by=c("code",'date'))
  
  print("model_5_1")
  
  load("model/model20190418_1b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_5_1=V1)],by=c("code",'date'))
  

  load("model/model20190418_1s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_5_1=V1)],by=c("code",'date'))
  
  print("model_1_2")
  
  load("model/model20190418_1b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_1_2=V1)],by=c("code",'date'))
  
  print("model_2_2")
  
  load("model/model20190418_1b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_2_2=V1)],by=c("code",'date'))
  
  print("model_3_2")
  
  load("model/model20190418_1b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_3_2=V1)],by=c("code",'date'))
  
  print("model_4_2")
  
  load("model/model20190418_1b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_4_2=V1)],by=c("code",'date'))
  
  print("model_5_2")
  
  load("model/model20190418_1b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_5_2=V1)],by=c("code",'date'))
  
  out = trainset_lv2
}

get.lv2.feature2_20200506 = function (train2){
  
  dtrain_lv2_1 <- xgb.DMatrix(data = as.matrix(apply(train2[,c(10:55, 56:65),with=F],2,as.numeric)))
  dtrain_lv2_2 <- xgb.DMatrix(data = as.matrix(apply(train2[,c(10:55, 56:65,68:74),with=F],2,as.numeric)))
  
  print("model_1_1")
  
  load("model/model20190418_1b_1_1.Rdata")
  trainset_lv2 = cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)],data.table(predict(cv_model_1b_1_1,dtrain_lv2_1)))[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s,p_1b_1_1=V1)]
  
  load("model/model20190418_2b_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_1_1=V1)],by=c("code",'date'))
  
  print("model_2_1")
  
  load("model/model20190418_1b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_2_1=V1)],by=c("code",'date'))
  
  print("model_3_1")
  
  load("model/model20190418_1b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_3_1=V1)],by=c("code",'date'))
  
  print("model_4_1")
  
  load("model/model20190418_1b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_4_1=V1)],by=c("code",'date'))
  
  print("model_5_1")
  
  load("model/model20190418_1b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_5_1=V1)],by=c("code",'date'))
  
  
  load("model/model20190418_1s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_5_1=V1)],by=c("code",'date'))
  
  print("model_1_2")
  
  load("model/model20190418_1b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_1_2=V1)],by=c("code",'date'))
  
  print("model_2_2")
  
  load("model/model20190418_1b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_2_2=V1)],by=c("code",'date'))
  
  print("model_3_2")
  
  load("model/model20190418_1b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_3_2=V1)],by=c("code",'date'))
  
  print("model_4_2")
  
  load("model/model20190418_1b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_4_2=V1)],by=c("code",'date'))
  
  print("model_5_2")
  
  load("model/model20190418_1b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_1s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_2s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date,label_1b,label_2b,label_3b,label_1s,label_2s,label_3s)]
                              ,data.table(predict(cv_model_3s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_5_2=V1)],by=c("code",'date'))
  
  out = trainset_lv2
}

get.lv2.predfeature_20190418 = function (train2){
  
  dtrain_lv2_1 = xgb.DMatrix(data = as.matrix(apply(train2[,c(4:49, 51:60),with=F],2,as.numeric)))
  dtrain_lv2_2 = xgb.DMatrix(data = as.matrix(apply(train2[,c(4:49, 51:60,63:69),with=F],2,as.numeric)))
  
  
  load("model/model20190418_1b_1_1.Rdata")
  trainset_lv2 = cbind(train2[,.(code,date)],data.table(predict(cv_model_1b_1_1,dtrain_lv2_1)))[,.(code,date,p_1b_1_1=V1)]
  
  load("model/model20190418_2b_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_1_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_1_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_1_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_1_1=V1)],by=c("code",'date'))
  
  print("model_2_1")
  
  load("model/model20190418_1b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_2_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_2_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_2_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_2_1=V1)],by=c("code",'date'))
  
  print("model_3_1")
  
  load("model/model20190418_1b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_3_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_3_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_3_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_3_1=V1)],by=c("code",'date'))
  
  print("model_4_1")
  
  load("model/model20190418_1b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_4_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_4_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_4_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_4_1=V1)],by=c("code",'date'))
  
  print("model_5_1")
  
  load("model/model20190418_1b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_1b_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_2b_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_3b_5_1=V1)],by=c("code",'date'))
  
  
  load("model/model20190418_1s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_1s_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_2s_5_1=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_5_1.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_5_1,dtrain_lv2_1)))
                       [,.(code,date,p_3s_5_1=V1)],by=c("code",'date'))
  
  print("model_1_2")
  
  load("model/model20190418_1b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_1_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_1_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_1_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_1_2=V1)],by=c("code",'date'))
  
  print("model_2_2")
  
  load("model/model20190418_1b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_2_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_2_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_2_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_2_2=V1)],by=c("code",'date'))
  
  print("model_3_2")
  
  load("model/model20190418_1b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_3_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_3_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_3_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_3_2=V1)],by=c("code",'date'))
  
  print("model_4_2")
  
  load("model/model20190418_1b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_4_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_4_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_4_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_4_2=V1)],by=c("code",'date'))
  
  print("model_5_2")
  
  load("model/model20190418_1b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_1b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_2b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3b_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3b_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_3b_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_1s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_1s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_1s_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_2s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_2s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_2s_5_2=V1)],by=c("code",'date'))
  
  load("model/model20190418_3s_5_2.Rdata")
  trainset_lv2 = merge(trainset_lv2
                       ,cbind(train2[,.(code,date)]
                              ,data.table(predict(cv_model_3s_5_2,dtrain_lv2_2)))
                       [,.(code,date,p_3s_5_2=V1)],by=c("code",'date'))
  
  out = trainset_lv2
}


getprice = function(times,lday){
  isready = fread("constant/isready.csv")
  # price = unique(rbind(#fread('data/pricedata.csv')
  #                      #,fread('data/price2015.csv')
  #                      #,fread('data/price2000.csv')
  #                      #,fread('data/price2005.csv')
  #                      #,fread('data/price2010.csv')
  #                      fread('data/price2020.csv')
  #                      ,fread('data/price2022.csv')
  # ))
  price = fread('data/price2022.csv')
  
  flag=0
  tmp2= data.table()
  repeat { 
    time = lday
    tmp1 = data.table(api(api_name = 'daily', trade_date = as.character(time)))
    tmp1 = tmp1[substr(ts_code,8,9)!='BJ',]
    
    if( nrow(tmp1)==nrow(price[trade_date==time,]) ) {
      isready = isready[V1==time,V2:=1]
      # flag=flag+1
      # if (flag %% 10 == 0 )
      # {
      fwrite(isready,"constant/isready.csv",row.names = F,quote = FALSE)
      Sys.sleep(0.01)
      # }
      print(time)
      break
    } else {
      break
    }
  }
  lastrow = nrow(tmp1)-nrow(price[trade_date==time,])
  print(lastrow)
  
  # if (flag > 0 )
  # {
  #   fwrite(isready,"constant/isready.csv",row.names = F,quote = FALSE)
  #   print(flag)
  # }
  tmp1 = tmp1[, .(ts_code = as.character(ts_code)
                  ,trade_date = as.character(trade_date))]
  tmp0 = merge(price,tmp1[,.(ts_code,trade_date=as.integer(trade_date))]
               ,by=c("ts_code","trade_date"),all.x=T,all.y=T)
  tmp1 = tmp0[is.na(amount) & trade_date==time,]
  mynrow = min(30,lastrow-1)
  tmp1 = tmp1[1:mynrow,][!is.na(ts_code),]
  for (code in tmp1$ts_code) {
    #print(code)
    tmp3 = bar(
      ts_code = code,
      start_date = as.character(time),
      end_date = as.character(lday),
      adj = "hfq"
    )

    tmp3 = as.data.table(tmp3)[,.(ts_code
                                  ,trade_date
                                  ,close
                                  ,open
                                  ,high
                                  ,low
                                  ,amount = as.integer(amount)+1
                                  ,ting = ifelse(as.single(pct_chg) > 4.5 & as.integer(amount) < 20000 & high==low,1
                                                 ,ifelse(as.single(pct_chg) < -4.5 & as.integer(amount) < 20000 & high==low,-1,0)
                                  )
    )]
    tmp2 = rbind(tmp2, tmp3)
  }
  tmp2
}

getpricebat = function(index,lday){
  
  pricedata = getprice(index,lday)
  pricedata = pricedata[,.(ts_code,trade_date= as.integer(trade_date)
                           ,close
                           ,open
                           ,high
                           ,low
                           ,amount,ting
  )]
  pricedata = pricedata[order(ts_code,trade_date,amount)]
  pricedata = pricedata[,head(.SD,1),by=.(ts_code,trade_date)]
  
  pricedata = pricedata[amount>0,]
  
  flag = nrow(pricedata)
  
   # price1 = unique(rbind(pricedata[trade_date>20200000 & trade_date<20220000,]
   #                       ,fread("data/price2020.csv"),fread("data/price2021.csv")))[amount>0,][trade_date>20200000  & trade_date<20220000,]
   # fwrite(price1,"data/price2020.csv",row.names = F,quote = FALSE)

  
  price1 = unique(rbind(pricedata[trade_date>20220000,]
                            ,fread("data/price2022.csv")))[close>0,][trade_date>20220000,]
      fwrite(price1,"data/price2022.csv",row.names = F,quote = FALSE)
    
  Sys.sleep(2)
}

savedata = function(){

  
  if (nrow(pricedata[trade_date>20200000,])>0){
    price1 = unique(rbind(pricedata[trade_date>20200000,]
                          ,fread("data/price2020.csv")
    ))[close>-10000,][trade_date>20200000,]
    fwrite(price1,"data/price2020.csv",row.names = F,quote = FALSE)
  }
  
  fwrite(pricedata[trade_date<20000000,],"data/pricedata.csv",row.names = F,quote = FALSE)
  
}