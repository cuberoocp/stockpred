source("lib_fea.R")

get.gdhs()

gdhs4 <- get.gdhs4(rbind(
  fread("data/gdhs2013.csv", colClasses = list(character = c("code", "jzdate", "fbdate"))),
  fread("data/gdhs2020.csv", colClasses = list(character = c("code", "jzdate", "fbdate")))
)
[, .(code, jzdate, fbdate, holders = as.integer(holders))], 14845)
gdhs2 <- rbind(
  fread("data2/gdhs2000_20201127.csv", colClasses = list(character = c("code"))),
  fread("data2/gdhs2005_20201127.csv", colClasses = list(character = c("code"))),
  fread("data2/gdhs2010_20201127.csv", colClasses = list(character = c("code"))),
  fread("data2/gdhs2015_20201127.csv", colClasses = list(character = c("code")))
)
max(gdhs2$tdid)
min(gdhs4$tdid)

gdhs4 <- rbind(gdhs2, gdhs4)
rm(gdhs2)



tradeday <- fread("tradeday.csv")
index <- tradeday

hfq <- rbind(
  fread("data/price2000.csv", colClasses = c(ts_code = "character", amount = "numeric")),
  fread("data/price2005.csv", colClasses = c(ts_code = "character", amount = "numeric")),
  fread("data/price2010.csv", colClasses = c(ts_code = "character", amount = "numeric")),
  fread("data/price2015.csv", colClasses = c(ts_code = "character", amount = "numeric")),
  fread("data/price2020.csv", colClasses = c(ts_code = "character", amount = "numeric")),
  fread("data/price2022.csv", colClasses = c(ts_code = "character", amount = "numeric"))
)

colnames(hfq) <- c("code", "date", "close", "open", "high", "low", "amount", "ting")

hfq <- hfq[!is.na(close), ][, code := substr(code, 1, 6)]

overall <- get.overall(hfq, index)

f1_price <- fread("data2/f1_price_20190703.csv", colClasses = c(code = "character"))

feature <- rbind(
  fread("data2/labelset2000_20201127.csv", colClasses = c(code = "character")),
  fread("data2/labelset2005_20201127.csv", colClasses = c(code = "character")),
  fread("data2/labelset2010_20201127.csv", colClasses = c(code = "character")),
  fread("data2/labelset2015_20201127.csv", colClasses = c(code = "character")),
  fread("data2/labelset2020_20201127.csv", colClasses = c(code = "character")),
  fread("data2/labelset2022_20201127.csv", colClasses = c(code = "character"))
)

# feature = rbind( fread("data2/labelset2020_20201127.csv", colClasses = c(code = "character"))
#  #, fread("data2/labelset2021_20201127.csv", colClasses = c(code = "character"))
# )

feature_new <- data.table()

periodid <- max(feature$did) - 200

onlist <- merge(hfq, f1_price[, .(code, date, c5)], by = c("date", "code"), all.x = T)[is.na(c5) == T, ]
onlist <- merge(onlist, feature[label_3b > 0 & label_3s > 0, .(code, date, c8)], by = c("date", "code"), all.x = T)[is.na(c8) == T, ]
onlist <- merge(onlist, index, by = "date")
onlist <- onlist[did > periodid, ]
onlist <- onlist[(as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ]

f1_price_new <- get.f1_20190418(hfq, index, onlist)

f1_price <- unique(rbind(
  f1_price, f1_price_new[, .(date, code,
    did = as.integer(did), close,
    c3, c3d, h3, l3, c5, c5d, h5, l5, c8, c8d,
    c13, c13d, c21, c21d,
    c34, c34d, c55, c55d,
    c89, c89d, c144, c144d, c233, c233d,
    a233, a233d, a144, a144d,
    a89, a89d, a55, a55d,
    a34, a34d, a21, a21d,
    a13, a13d, a8, a8d, a5, a5d, a3, a3d,
    amount
  )],
  feature[is.na(label_3b) | is.na(label_3s), .(
    date, code, did, close,
    c3, c3d, h3, l3, c5, c5d, h5, l5, c8, c8d,
    c13, c13d, c21, c21d,
    c34, c34d, c55, c55d,
    c89, c89d, c144, c144d, c233, c233d,
    a233, a233d, a144, a144d,
    a89, a89d, a55, a55d,
    a34, a34d, a21, a21d,
    a13, a13d, a8, a8d, a5, a5d, a3, a3d,
    amount
  )]
))

f1_price <- f1_price[did > periodid, ]

f1_label <- get.label2_20201127(hfq, index, unique(f1_price[, .(code, did), ]))


feature_new <- merge(f1_label, f1_price, by = c("code", "did"))

feature <- rbind(feature[label_3b > 0 & label_3s > 0, ], feature_new)

feature <- feature[order(code, did, label_1b, label_1s, label_2b, label_2s, label_3b, label_3s)][, head(.SD, 1), by = .(code, did)]

nrow(feature[date < 20050000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ])
# 151939
fwrite(feature[date < 20050000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ],
  "data2/labelset2000_20201127.csv",
  row.names = F, quote = FALSE
)
fwrite(merge(gdhs4, feature[date < 20050000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), .(code, did)],
  by = c("code", "did")
), "data2/gdhs2000_20201127.csv", row.names = F, quote = FALSE)

nrow(feature[date < 20100000 & date >= 20050000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ])
# 555831
fwrite(feature[date < 20100000 & date >= 20050000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ],
  "data2/labelset2005_20201127.csv",
  row.names = F, quote = FALSE
)
fwrite(merge(gdhs4, feature[date < 20100000 & date >= 20050000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), .(code, did)],
  by = c("code", "did")
), "data2/gdhs2005_20201127.csv", row.names = F, quote = FALSE)


nrow(feature[date < 20150000 & date >= 20100000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ])
# 1419952
fwrite(feature[date < 20150000 & date >= 20100000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ],
  "data2/labelset2010_20201127.csv",
  row.names = F, quote = FALSE
)
fwrite(merge(gdhs4, feature[date < 20150000 & date >= 20100000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), .(code, did)],
  by = c("code", "did")
), "data2/gdhs2010_20201127.csv", row.names = F, quote = FALSE)


nrow(feature[date < 20200000 & date >= 20150000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ])
# 2761072
fwrite(feature[date < 20200000 & date >= 20150000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ],
  "data2/labelset2015_20201127.csv",
  row.names = F, quote = FALSE
)
fwrite(merge(gdhs4, feature[date < 20200000 & date >= 20150000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), .(code, did)],
  by = c("code", "did")
), "data2/gdhs2015_20201127.csv", row.names = F, quote = FALSE)


nrow(feature[date < 20220000 & date >= 20200000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ])
# 1769547
fwrite(feature[date < 20220000 & date >= 20200000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ],
  "data2/labelset2020_20201127.csv",
  row.names = F, quote = FALSE
)


nrow(feature[date < 20230000 & date >= 20220000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ])
# 233949
fwrite(feature[date < 20230000 & date >= 20220000 & (as.integer(code) + did * 777) %% 1000 <= 1000 * (did - 99999) / (periodid - 99999), ],
  "data2/labelset2022_20201127.csv",
  row.names = F, quote = FALSE
)


f1_price <- merge(f1_price, feature[, .(code, date, label_3b, label_3s)], by = c("date", "code"), all.x = T)[is.na(label_3b) == T | is.na(label_3s) == T, ]
f1_price <- f1_price[, label_3b := NULL][, label_3s := NULL]
f1_price <- unique(f1_price)

fwrite(f1_price, "data2/f1_price_20190703.csv", row.names = F, quote = FALSE)

rm(hfq)
rm(feature)
rm(feature_new)
rm(train)
rm(test)
gc()
