source("lib_fea.R")

feature <- rbind(fread("data2/labelset2020_20201127.csv", colClasses = c(code = "character")))

periodid <- max(feature$did) - 200
f1_price <- fread("data2/f1_price_20190703.csv", colClasses = c(code = "character"))
pred <- f1_price[, maxdid := max(did), by = code][did > maxdid - 10 & did > periodid, ][order(code, did)][, tail(.SD, 1), by = code]
overall <- fread("data/overall.csv")
pred <- merge(pred, overall, by = "did")
rm(overall)
pred <- merge(pred, gdhs4[, .(code, did, jztdid, tdid, p1p, p2p, p3p, p1d, p2d, p3d)], by = c("code", "did"), all.x = T)
pred <- pred[, delay := tdid - jztdid]

pred_step1 <- function(pred) {
  predset <- get.lv2.predfeature_20190418(pred)

  predset <- predset[, score :=
    p_1b_1_1 + p_2b_1_1 + p_3b_1_1 + p_1s_1_1 + p_2s_1_1 + p_3s_1_1
    + p_1b_2_1 + p_2b_2_1 + p_3b_2_1 + p_1s_2_1 + p_2s_2_1 + p_3s_2_1
    + p_1b_3_1 + p_2b_3_1 + p_3b_3_1 + p_1s_3_1 + p_2s_3_1 + p_3s_3_1
    + p_1b_4_1 + p_2b_4_1 + p_3b_4_1 + p_1s_4_1 + p_2s_4_1 + p_3s_4_1
    + p_1b_5_1 + p_2b_5_1 + p_3b_5_1 + p_1s_5_1 + p_2s_5_1 + p_3s_5_1
    + p_1b_1_2 + p_2b_1_2 + p_3b_1_2 + p_1s_1_2 + p_2s_1_2 + p_3s_1_2
    + p_1b_2_2 + p_2b_2_2 + p_3b_2_2 + p_1s_2_2 + p_2s_2_2 + p_3s_2_2
    + p_1b_3_2 + p_2b_3_2 + p_3b_3_2 + p_1s_3_2 + p_2s_3_2 + p_3s_3_2
    + p_1b_4_2 + p_2b_4_2 + p_3b_4_2 + p_1s_4_2 + p_2s_4_2 + p_3s_4_2
    + p_1b_5_2 + p_2b_5_2 + p_3b_5_2 + p_1s_5_2 + p_2s_5_2 + p_3s_5_2]

  predset_lv2 <- xgb.DMatrix(data = as.matrix(
    apply(predset[, 3:62, with = F], 2, as.numeric)
  ))

  load("model/cv_model20190418_lv2_b_1.Rdata")
  result_1 <- cbind(predset[, .(code, date)], data.table(score_b = predict(cv_model_lv2_b_1, predset_lv2)))

  predset_lv2 <- xgb.DMatrix(data = as.matrix(
    apply(predset[, 3:62, with = F], 2, as.numeric)
  ))

  load("model/cv_model20190418_lv2_s_1.Rdata")
  result_2 <- cbind(predset[, .(code, date)], data.table(score_s = predict(cv_model_lv2_s_1, predset_lv2)))


  result <- merge(predset[, .(code, date, score)], result_1, by = c("code", "date"))
  result <- merge(result, result_2, by = c("code", "date"))


  c2name <- fread("constant/c2name.csv", colClasses = c(symbol = "character"), encoding = "UTF-8")
  c2name <- c2name[, .(ts_code, code = symbol, name)]


  result <- merge(result, c2name, by = "code", all.x = T)
  result <- merge(result, pred[, .(code, date, delay)], by = c("code", "date"))

  result2 <- result[, .(m_b = median(score_b), avg_b = mean(score_b), m_s = median(score_s), avg_s = mean(score_s))]
  print(result2)

  result
}

result <- pred_step1(pred)
result <- result[, score5 := (score / 35 + score_b * 10 + score_s * 11) / 22, ]
result[, .(x1 = mean(score5), x2 = median(score5))]



record <- fread("机器能赢吗.csv", colClasses = c(code = "character"), encoding = "UTF-8")
record <- merge(record[, .(code, yper = as.double(yper), tper = as.double(tper))], result, by = "code", all.y = T)
record <- record[order(-score5)][, rid := 1][, rid := cumsum(rid)]
record <- pad.null(record, 0)
record <- record[order(-tper, -date, -score5)][1:600, ][, yper := tper]

record <- record[, tmp := (score5 * 1.63 - 1.02 * 1.63 - rid / 50000) * 0.68, by = code]
record <- record[, tper := max(ifelse(yper > tmp, ceiling(tmp * 65 + yper * 35) / 100, floor(tmp * 65 + yper * 35) / 100), 0), by = code]

record <- record[order(-yper, -score5)][, rid := 1][tper > 0, rid2 := cumsum(rid)]
record <- record[, .(code, name, date, yper, tper, score5, rid2)]
record <- record[(tper > 0 & rid2 < 38 + nrow(record[tper == 0 & yper > 0, ])) | yper > 0, ][order(-score5)]

fwrite(record[is.na(code) == F, .(code, name, date, yper, tper)], "机器能赢吗.csv", row.names = F, quote = FALSE)


rm(f1_price)
gc()


library(Tushare)
api <- Tushare::pro_api(token = "79fba1aebdf24a8fc0dd552714d79f6bfa5ca6e0fa742d8f5ab76268")
bar <- Tushare::pro_bar(token = "79fba1aebdf24a8fc0dd552714d79f6bfa5ca6e0fa742d8f5ab76268")

df <- as.data.table(api(api_name = "daily", trade_date = max(result$date)))

zijin <- 26.0
record2 <- fread("机器小熊.csv", colClasses = c(code = "character", ycnt = "integer", tcnt = "integer"), encoding = "UTF-8")
record2 <- record2[, date := NULL]
record2 <- record2[, name := NULL]
record2 <- record2[, score5 := NULL]
record2 <- record2[, close := NULL]
record2 <- merge(record2, result, by = "code", all.y = T)
record2 <- merge(record2, df[, .(ts_code, date = as.integer(trade_date), close)], by = c("ts_code", "date"))
record2 <- record2[substr(ts_code, 1, 3) != "688"][substr(name, 1, 3) != "*ST"]

record2 <- record2[order(-score5)][close > 1.6, ][, rid := 1][, rid := cumsum(rid)]
record2 <- pad.null(record2, 0)
record2 <- record2[order(-tcnt, -date, -score5)][rid <= 10 | tcnt > 0, ][, yper := tcnt * close / 10000 / zijin][, ycnt := tcnt]
record2 <- record2[, tmp := ((score5 - 1.02) * 1.46 - rid / 30000) * 0.99, by = code]
record2 <- record2[, tper := max(0, tmp * 0.65 + yper * 0.35), by = ts_code]
record2 <- record2[, tcnt := round(tper * zijin * 10000 / close / 100) * 100]
record2 <- record2[, tcnt := ifelse((tcnt - ycnt) * close > 10000 |
  (tcnt - ycnt) * close < -7000 | tcnt < 0.001, tcnt, ycnt)]
record2 <- record2[, tmp2 := max(0, tmp), by = ts_code]
xiuzheng <- 0

cangwei <- max(0.05, min(1, sum(record2[order(-score5)][1:10, ]$tmp2)))

print(cangwei)

s <- max(0.01, sum(record2[, .(tprice = tcnt * close)]$tprice) / zijin / 10000)
b <- s / cangwei

times <- 0

while (abs(b - 1) > 0.05 + 0.001 * times | sum(record2[, .(tprice = tcnt * close)]$tprice) / zijin / 10000 > 1) {
  xiuzheng <- xiuzheng + ifelse(b > 1, -0.005, 0.006)
  record2 <- record2[, tmp := ((score5 - 1.02 + xiuzheng) * 1.46 - rid / 30000) * 0.99, by = code]
  record2 <- record2[, tper2 := max(0, tmp * 0.65 + yper * 0.35), by = ts_code]
  record2 <- record2[, tcnt := round(tper2 * zijin * 10000 / close / 100) * 100]
  record2 <- record2[, tcnt := ifelse((tcnt - ycnt) * close > 10000 | (tcnt - ycnt) * close < -7000 | tcnt < 0.001, tcnt, ycnt)]
  s <- max(0.01, sum(record2[, .(tprice = tcnt * close)]$tprice) / zijin / 10000)
  b <- s / cangwei
  times <- times + 1
  if (times %% 3 == 0) {
    print(paste(
      b, " ", xiuzheng, " ", abs(b - 1), " ",
      sum(record2[, .(tprice = tcnt * close)]$tprice) / zijin / 10000
    ))
  }
}

record2 <- record2[tcnt > 0 | ycnt > 0, ][order(code)]
record3 <- record2[, .(code, name, rid, ycnt, tcnt, score_b, score_s, score5, delay, close)]

fwrite(record2[is.na(code) == F, .(code, name, date, ycnt, tcnt, score5, close)], "机器小熊.csv", row.names = F, quote = FALSE)

gc()


cookie <- "kbzw_r_uname=maoxiong; kbz_newcookie=1; kbzw__Session=trk9iifetngalcbbraedp4moe1; kbzw__user_login=7Obd08_P1ebax9aX5MPl2eDm4d-Cq47k2ujc8NvpxtS-otvZ25bSldXb2Z2ynqnEqJLd26qvlaiS19uqobDQrMamgrKk6OHFzr6fqq6eqY2yj8ui1dSexdDqyuDl1piumqeCnrjg5dfn2OOBws2Vmqmap52WuODlqayckNmqrbCJ6-Kxm66Pp6CTv8bTzOOop5mqnKeTppKXvdzqxtbQ7Kiuoaec"
headers2 <- c(
  "Accept" = "application/json",
  "Content-Type" = "text/plain",
  "User-Agent" = "Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.  36 (KHTML, like Gecko) Chrome/49.0.2623.221 Safari/537.36 SE 2.X Met aSr 1.0",
  "Referer" = "https://wx.qq.com/",
  "Connection" = "keep-alive",
  "cookie" = cookie
)

jsl1 <- GET("https://www.jisilu.cn/data/cbnew/cb_list/?___jsl=LST___t=1554547931751", add_headers(.headers = headers2))
jsl1 <- fromJSON(content(jsl1, "text"))
jsl1 <- as.data.table(jsl1$rows$cell)

if (nrow(jsl1) > 100) {
  tmp <- jsl1[, .(
    zzcode = bond_id, zzname = bond_nm, name = stock_nm, price = full_price,
    pb, value = convert_value, yijia = premium_rt, pingji = rating_cd,
    remain = year_left, convertflag = convert_cd_tip, ytm_rt, code = stock_id,
    cprice = as.double(convert_price),
    rday = ifelse(is.na(redeem_dt), 50, as.Date(redeem_dt) - Sys.Date()),
    forceprice = as.double(force_redeem_price), putprice = as.double(put_convert_price),
    redeemprice = as.integer(redeem_price),
    tflag = ifelse(abs(as.numeric(sub("%", "", sincrease_rt))) > 4.5, 1, 0)
  )]

  jsl2 <- GET("https://www.jisilu.cn/data/cbnew/pre_list/?___jsl=LST___t=1554548125413")
  jsl2 <- fromJSON(content(jsl2, "text"))
  jsl2 <- as.data.table(jsl2$rows$cell)
  tmp2 <- jsl2[, .(
    zzcode = bond_id, zzname = bond_nm, name = stock_nm, value = pma_rt,
    pingji = rating_cd, code = stock_id
  )]
  tmp2 <- tmp2[!is.na(zzname), ]

  tmp2 <- tmp2[, price := 100][, pb := 2]
  tmp2 <- tmp2[, convertflag := "未到转股期"][, ytm_rt := 0.01][, yijia := 100 - as.double(value)]

  tmp3 <- rbind(
    tmp[, .(zzcode, zzname, name, code,
      value = as.double(value), pingji,
      price = as.double(price), pb = as.double(pb), convertflag,
      ytm_rt = as.numeric(sub("\\%", "", ytm_rt)) / 100,
      yijia = as.numeric(sub("\\%", "", yijia)) / 100,
      lifecycle = as.double(remain),
      rate1 = as.integer(round(forceprice / cprice * 100)), rate2 = as.integer(round(putprice / cprice * 100)),
      redeemprice, rday, tflag
    )],
    tmp2[!zzcode %in% tmp$zzcode, .(zzcode, zzname, name, code,
      value = as.double(value), pingji,
      price, pb, convertflag, ytm_rt, yijia, lifecycle = 6, rate1 = 130, rate2 = 70, redeemprice = 110,
      rday = 50, tflag = 0
    )]
  )

  tmp3 <- tmp3[, rday := ifelse(lifecycle < 0.25, lifecycle * 200, rday)]
  tmp3 <- pad.null(tmp3, 0)

  tmp3 <- tmp3[, eb := grepl(pattern = "EB", zzname)]
  tmp3 <- tmp3[, remain := ifelse(substr(convertflag, 1, 5) == "未到转股期", (lifecycle + 0.501 - as.integer(lifecycle + 0.501)), 0), by = zzcode]
  tmp3 <- tmp3[, pingji := ifelse(pingji == "AAA", 4,
    ifelse(pingji == "AA+", 3,
      ifelse(pingji == "AA", 2,
        ifelse(pingji == "AA-", 1, 0)
      )
    )
  )]

  tmp3 <- tmp3[, code := ifelse(nchar(code) == 8, substr(code, 3, 8), code)]

  rm(jsl1)
  rm(jsl2)

  zhuanzhai <- merge(unique(result[, .(code, score = score5 - 1)]), tmp3, by = "code", all.y = T)

  zhuanzhai <- zhuanzhai[order(-ytm_rt)]
  zhuanzhai <- zhuanzhai[, flag := 1]
  zhuanzhai <- zhuanzhai[, ytmorder := cumsum(flag)]
  zhuanzhai <- zhuanzhai[, ytmorder := ifelse(ytmorder < 41, 41 - ytmorder, 0) - 36 - (32 - rday) * 2 - ifelse(rday < 15, (32 - rday) * 5, 0)]

  adjust2 <- fread("pingji.csv")
  adjust2 <- adjust2[, .(zzcode = as.character(zzcode), power1, power1d, power2, power2d, power3, power3d)]

  library(openxlsx)
  adjust3 <- as.data.table(
    read.xlsx(
      "zhuanzhai.xlsx",
      sheet = 1, startRow = 1, colNames = TRUE,
      rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
      skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
      namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE
    )
  )
  adjust3 <- adjust3[, zzcode := as.character(zzcode)]
  adjust2 <- merge(adjust2[, .(zzcode, power1, power1d, power2, power2d, power3, power3d)],
    adjust3[, .(zzcode, cost1, cost2, cost3, cost4)],
    by = "zzcode", all.y = T
  )

  adjust2 <- adjust2[, .(power = 0.1 * sum(power1, na.rm = T)
    + 0.1 * sum(power1d, na.rm = T) + 0.2 * sum(power2, na.rm = T)
    + 0.1 * sum(power2d, na.rm = T) + 0.3 * sum(power3, na.rm = T)
    + 0.1 * sum(power3d, na.rm = T)), by = .(zzcode, cost1, cost2, cost3, cost4)]

  zhuanzhai <- merge(zhuanzhai, adjust2[, .(zzcode, power)], by = "zzcode", all.x = T)
  zhuanzhai <- pad.null(zhuanzhai, 0)
  zhuanzhai <- merge(zhuanzhai, adjust2[, .(zzcode, cost1, cost2, cost3, cost4)], by = "zzcode", all.x = T)
  zhuanzhai <- pad.null(zhuanzhai, "")

  zhuanzhai <- zhuanzhai[, .(zzcode, name, zzname, price, yijia,
    eb = as.integer(eb), remain, pingji = pingji / 2, pb, power, lifecycle, rate1, rate2, redeemprice, rday, tflag, ytmorder,
    score, cost1, cost2, cost3, cost4
  )]

  zhuanzhaitrain <- fread("data2/zhuanzhaitrain.csv")

  newtrain <- rbind(
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = -15, price = price * 1.15, yijia = yijia * 1.15 + 0.15, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = -10, price = price * 1.10, yijia = yijia * 1.1 + 0.10, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = -8, price = price * 1.08, yijia = yijia * 1.08 + 0.08, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = -6, price = price * 1.06, yijia = yijia * 1.06 + 0.06, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = -4, price = price * 1.04, yijia = yijia * 1.04 + 0.04, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = -2, price = price * 1.02, yijia = yijia * 1.02 + 0.02, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = 0, price, yijia, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = 2, price = price * 0.98, yijia = yijia * 0.98 - 0.02, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = 4, price = price * 0.96, yijia = yijia * 0.96 - 0.04, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = 6, price = price * 0.94, yijia = yijia * 0.94 - 0.06, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = 8, price = price * 0.92, yijia = yijia * 0.92 - 0.08, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = 10, price = price * 0.9, yijia = yijia * 0.9 - 0.1, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)],
    zhuanzhai[!zzcode %in% tmp2$zzcode & tflag == 0, .(label = 15, price = price * 0.85, yijia = yijia * 0.85 - 0.15, eb, remain, pingji, pb, power, lifecycle, rate1, rate2, redeemprice, rday)]
  )

  newtrain <- newtrain[, ds := as.character(Sys.Date())]

  dtrain <- xgb.DMatrix(data = as.matrix(apply(zhuanzhaitrain[, 2:13, with = F], 2, as.numeric)), label = zhuanzhaitrain$label)
  dtest <- xgb.DMatrix(data = as.matrix(apply(newtrain[, 2:13, with = F], 2, as.numeric)), label = newtrain$label)

  xgbparams <- list(
    eval_metric = "mae",
    tree_method = "hist",
    max_bin = 64,
    grow_policy = "lossguide",
    max_depth = 10,
    min_child_weight = 8000,
    eta = 0.2
  )

  cv_zz_model <- xgb.train(
    data = dtrain, params = xgbparams, watchlist = list(train = dtrain, test = dtest),
    nrounds = 8000, early_stopping_rounds = 300, print_every_n = 500,
    gamma = 2.8, maximize = F
  )

  #[8000]	train-mae:1.822543	test-mae:1.629042 
  save(cv_zz_model, file = "model/cv_zz_model.Rdata")
} else {
  stop("cookie error")
}


load("model/cv_zz_model.Rdata")
predset_lv2 <- xgb.DMatrix(data = as.matrix(apply(zhuanzhai[, 4:15, with = F], 2, as.numeric)))
zhuanzhai <- cbind(zhuanzhai, data.table(score2 = predict(cv_zz_model, predset_lv2)))
zhuanzhai <- zhuanzhai[, score2 := as.integer(score2 * 10)]

index <- sample(x = 2, size = nrow(zhuanzhaitrain), replace = TRUE, prob = c(0.995, 0.005))

zhuanzhaitrain <- rbind(zhuanzhaitrain[index == 1, ], newtrain)
fwrite(zhuanzhaitrain, "data2/zhuanzhaitrain.csv", row.names = F, quote = FALSE)

# zhuanzhai = zhuanzhai[,yijia:=ifelse(yijia<0.3,yijia,yijia-min(max((pb-1)*(0.1+power),0),(105-as.double(price) )/70)),by=zzcode]

# p0 = zhuanzhai[yijia<0.02,][,score1:=as.integer(score*1000) ]
#
# p1 = zhuanzhai[yijia>=0.02 & yijia<0.1,][,score1:= as.integer(score*800)]
#
# p2 = zhuanzhai[yijia>=0.1 & yijia<0.5,][,score1:= as.integer(score*500)]
#
# p3 = zhuanzhai[yijia>=0.5,][,score1:= as.integer(score*100)]
#
#
# zhuanzhai = unique(rbind(p0,p1,p2,p3))

zhuanzhai <- zhuanzhai[, score1 := round(score * 400 * (10 * (ifelse(yijia < 0, 0, yijia) + 0.1) + 10) / (8 * ifelse(yijia < 0, 0, yijia) + 1.8))]

zhuanzhai <- zhuanzhai[, final := score1 + score2 + ytmorder][order(-final)]

zhuanzhai <- zhuanzhai[, .(zzcode, name, zzname, price, yijia, cost1, cost2, cost3, cost4, score1, score2, final, ytmorder)]

write.xlsx(unique(zhuanzhai[, .(zzcode, name, zzname, price, yijia, cost1, cost2, cost3, cost4, final, score1)]), "zhuanzhai.xlsx", sheetName = "Sheet1", colnames = TRUE, rownames = FALSE, append = FALSE, overwrite = T)
fwrite(unique(zhuanzhai[, .(zzcode, name, zzname, price, yijia, cost1, cost2, cost3, cost4, final, score1)]), "zhuanzhai.csv", row.names = F, quote = FALSE)

