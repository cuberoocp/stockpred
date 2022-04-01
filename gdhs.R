source("../lib.R")
source("lib_fea.R")

get.gdhs()

gdhs <- fread("data/gdhs2020.csv", colClasses = c(code = "character", jzdate = "character", fbdate = "character"))

gdhs <- gdhs[, .(code, holders = as.integer(holders), jzdate, fbdate, pp, pp3)]

r <- GET("http://data.eastmoney.com/DataCenter_V3/gdhs/GetList.ashx?reportdate=&market=&changerate==&range==&pagesize=5000&page=1&sortRule=-1&sortType=NoticeDate&js=var%20FAzTAOfi")

tt <- as.data.table(fromJSON(substr(content(r, "text"), 14, 1000000000)))

gdhs <- rbind(gdhs, tt[, .(code = data.SecurityCode, holders = data.HolderNum, jzdate = substr(data.EndDate, 1, 10), fbdate = substr(data.NoticeDate, 1, 10), pp = -1, pp3 = -1)])

r <- GET("http://data.eastmoney.com/DataCenter_V3/gdhs/GetList.ashx?reportdate=2020-09-30&market=&changerate==&range==&pagesize=5000&page=1&sortRule=-1&sortType=NoticeDate&js=var%20FAzTAOfi")

tt <- as.data.table(fromJSON(substr(content(r, "text"), 14, 1000000000)))

gdhs <- rbind(gdhs, tt[, .(code = data.SecurityCode, holders = data.HolderNum, jzdate = substr(data.EndDate, 1, 10), fbdate = substr(data.NoticeDate, 1, 10), pp = -1, pp3 = -1)])

r <- GET("http://data.eastmoney.com/DataCenter_V3/gdhs/GetList.ashx?reportdate=2020-12-31&market=&changerate==&range==&pagesize=5000&page=1&sortRule=-1&sortType=NoticeDate&js=var%20FAzTAOfi")

tt <- as.data.table(fromJSON(substr(content(r, "text"), 14, 1000000000)))

gdhs <- rbind(gdhs, tt[, .(code = data.SecurityCode, holders = data.HolderNum, jzdate = substr(data.EndDate, 1, 10), fbdate = substr(data.NoticeDate, 1, 10), pp = -1, pp3 = -1)])

gdhs <- gdhs[fbdate != "" | !holders > 0, .(
  fbdate = min(fbdate, na.rm = T),
  holders = max(holders, na.rm = T),
  pp = max(pp, na.rm = T),
  pp3 = max(pp3, na.rm = T)
), by = .(code, jzdate)]

gdhs$holders <- as.integer(gdhs$holders)

gdhs <- gdhs[holders == pp, pp := NA, ]
gdhs <- gdhs[pp == -Inf, pp := NA, ]
gdhs <- gdhs[holders == pp3, pp3 := NA, ]
gdhs <- gdhs[pp3 == -Inf, pp3 := NA, ]

gdhs <- gdhs[order(code, jzdate, fbdate)]

fwrite(gdhs, "data/gdhs.csv", row.names = F, quote = T)




require(rvest)
require(magrittr)
url <- "http://www.yidiancangwei.com/gudong/?StartDay=2020-08-10&EndDay=2021-02-08&Page=1&Limit=70000"
content <- read_html(url)
trade <- html_table(content, header = TRUE)[[1]]

trade <- as.data.table(trade)
colnames(trade) <- c("id", "name", "code0", "pp3", "xx", "jzdate")

trade <- trade[, .(id, name, code0, pp3 = as.integer(pp3), xx, jzdate)]
trade <- trade[pp3 > 0, ]

c2name <- rbind(
  fread("constant/c2name.csv", header = T, colClasses = c(symbol = "character"), encoding = "UTF-8"),
  fread("constant/c2name_d.csv", header = T, colClasses = c(symbol = "character"), encoding = "UTF-8")
)

codexx <- c2name[, .(code = symbol, name2 = name, code0 = as.integer(symbol))]

codexx <- unique(rbind(codexx, data.table(code = "601313", name2 = "江南嘉捷", code0 = 601313)))

trade <- merge(trade[code0 < 200000 | (code0 >= 300000 & code0 < 900000), ], codexx, by = "code0", all.x = T, all.y = T)

trade <- trade[pp3 > 0 & !is.na(code), .(code, pp3, jzdate)]

trade2 <- fread("constant/yidian.csv", colClasses = c(code = "character", jzdate = "character"))

trade <- merge(trade[, .(code, pp3, jzdate, flag1 = 1)], trade2[, .(code, pp3, jzdate, flag2 = 1)], by = c("code", "pp3", "jzdate"), all.x = T)

trade2 <- trade[is.na(flag2), ][as.integer(pp3) > 100, ]

fwrite(trade[, .(code, pp3, jzdate)], "constant/yidian.csv", row.names = F, quote = T)

trade2 <- rbind(trade2, data.table(code = "xxx", pp3 = "xx", jzdate = "xx", flag1 = 1, flag2 = 1))
gdhsx <- merge(gdhs[, .(code, jzdate, fbdate, holders, pp, flag1 = 1, pp3)], trade2[, .(code, pp3 = as.integer(pp3), jzdate, flag2 = 1)], by = c("code", "jzdate", "pp3"), all.x = T, all.y = T)

gdhsx <- gdhsx[, .(
  fbdate = min(fbdate, na.rm = T),
  holders = max(holders, na.rm = T),
  pp = max(pp, na.rm = T),
  pp3 = max(pp3, na.rm = T),
  flag1 = max(flag1, na.rm = T),
  flag2 = max(flag2, na.rm = T)
),
by = .(code, jzdate)
]

gdhsx <- gdhsx[, flag2 := sign(flag2)][, flag1 := sign(flag1)][flag1 * flag2 == -1 & flag2 == 1, flag3 := 1]
gdhsx <- gdhsx[, flag3 := max(flag3, na.rm = T), by = code]

gdhsx <- gdhsx[order(flag3, code, jzdate)]

gdhsx <- gdhsx[, .(code, jzdate, fbdate, holders, pp, pp3)]

gdhsx <- gdhsx[pp == -Inf, pp := NA, ]
gdhsx <- gdhsx[pp3 == -Inf, pp3 := NA, ]

fwrite(gdhsx, "data/gdhs2.csv", row.names = F, quote = T)



rm(tmp)
rm(tmp2)
rm(tmp3)
rm(gdhs)
rm(gdhs2)
rm(gdhs3)
rm(content)
rm(trade)
rm(codelist2)
rm(codexx)


gdhs2020 <- fread("data/gdhs2020.csv", colClasses = c(code = "character", jzdate = "character", fbdate = "character"))

codelist <- unique(gdhs2020$code)
errorcode <- data.table()
j <- 0
for (i in codelist)
{
  url <- paste("http://data.eastmoney.com/DataCenter_V3/gdhs/GetDetial.ashx?code=",
    i, "&js=var%20IEGsjwHz&pagesize=5000&page=1",
    sep = ""
  )
  r <- GET(url)

  if (r$status_code == 200) {
    if (fromJSON(substr(content(r, "text"), 14, 100000000))$page == 1
    ) {
      tt <- as.data.table(fromJSON(substr(content(r, "text"), 14, 100000000)))
      gdhs2020 <- rbind(gdhs2020, tt[, .(code = i, jzdate = substr(data.EndDate, 1, 10), fbdate = substr(data.NoticeDate, 1, 10), holders = data.HolderNum, pp = -1, pp3 = -1)])
      gdhs2020 <- unique(gdhs2020[as.integer(holders) > 500, ])
    }
  } else {
    print(i)
    errorcode <- rbind(errorcode, data.table(code = i))
  }
  j <- j + 1
  if (j %% 30 == 1) {
    print(j)
  }
}

gdhs$holders <- as.integer(gdhs$holders)

fwrite(gdhs, "gdhs.csv", row.names = F, quote = FALSE)

fwrite(errorcode, "errorcode.csv", row.names = F, quote = FALSE)



crealer <- fread("data/crawler_graph.csv", header = F, sep = ",")

crealer2 <- crealer[, .(code = substr(V1, 1, 6), data = unlist(strsplit(V1, split = ";"))), by = V1]

crealer2 <- crealer2[code != data, .(code, data)]

crealer <- crealer2[, .(len = nchar(data)), by = .(code, data)]

crealer <- crealer[, .(code, pp = substr(data, 1, len - 9), date = substr(data, len - 7, len)), by = .(code, data)]

gdhs <- merge(gdhs, crealer[, .(code, pp2 = pp, jzdate = paste("20", date, sep = ""))], by = c("code", "jzdate"), all.x = T, all.y = T)

gdhs <- gdhs[, .(pp = max(pp, pp2, na.rm = T)), by = .(code, jzdate, fbdate, holders)]

gdhs2020 <- gdhs2020[jzdate > "2020-00-00", ]

gdhs2013 <- fread("data/gdhs2013.csv", colClasses = c(code = "character"))
gdhs2000 <- fread("data/gdhs2000.csv", colClasses = c(code = "character"))
gdhs2020 <- fread("data/gdhs2020.csv", colClasses = c(code = "character"))
fwrite(gdhs2013[jzdate < "2020-00-00", ], "data/gdhs2013.csv", row.names = F, quote = T)

fwrite(gdhs2000, "data/gdhs2000.csv", row.names = F, quote = T)
fwrite(gdhs2013, "data/gdhs2013.csv", row.names = F, quote = T)
fwrite(gdhs2020, "data/gdhs2020.csv", row.names = F, quote = T)

gdhs2013 <- gdhs2013[, V7 := NULL]
fwrite(gdhs2013, "data/gdhs2013.csv", row.names = F, quote = T)

tmp2 <- gdhs2013[fbdate < jzdate, ]
gdhs2013 <- gdhs2013[fbdate == jzdate - 1, fbdate := jzdate]
gdhs2013 <- gdhs2013[fbdate == jzdate - 3, fbdate := jzdate]
gdhs2020 <- gdhs2020[fbdate == jzdate - 1, fbdate := jzdate]
gdhs2020 <- gdhs2020[fbdate == jzdate - 3, fbdate := jzdate]

gdhs2020 <- gdhs2020[order(code, jzdate, -pp3)]
gdhs2020 <- gdhs2020[, head(.SD, 1), by = .(code, jzdate)]
