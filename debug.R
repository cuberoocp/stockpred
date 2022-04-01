getpricebat <- function(index, lday) {
  pricedata <- getprice(index, lday)
  pricedata <- pricedata[, .(ts_code,
    trade_date = as.integer(trade_date),
    close = as.single(close),
    open = as.single(open),
    high = as.single(high),
    low = as.single(low),
    amount = round(as.single(amount), 0)
  )]
  pricedata <- pricedata[order(ts_code, trade_date, amount)]
  pricedata <- pricedata[, head(.SD, 1), by = .(ts_code, trade_date)]

  print(pricedata[is.na(close), .N, by = ts_code])

  pricedata <- unique(rbind(pricedata, fread("data/pricedata.csv")))[amount > 0, ]

  flag <- nrow(pricedata)

  if (flag > 10000) {
    if (nrow(pricedata[trade_date > 20000000 & trade_date < 20050000, ]) > 0) {
      price1 <- unique(rbind(
        pricedata[trade_date > 20000000 & trade_date < 20050000, ],
        fread("data/price2000.csv")
      ))[amount > 0, ][trade_date > 20000000 & trade_date < 20050000, ]
      fwrite(price1, "data/price2000.csv", row.names = F, quote = FALSE)
    }
    if (nrow(pricedata[trade_date > 20050000 & trade_date < 20100000, ]) > 0) {
      price1 <- unique(rbind(
        pricedata[trade_date > 20050000 & trade_date < 20100000, ],
        fread("data/price2005.csv")
      ))[amount > 0, ][trade_date > 20050000 & trade_date < 20100000, ]
      fwrite(price1, "data/price2005.csv", row.names = F, quote = FALSE)
    }
    if (nrow(pricedata[trade_date > 20100000 & trade_date < 20150000, ]) > 0) {
      price1 <- unique(rbind(
        pricedata[trade_date > 20100000 & trade_date < 20150000, ],
        fread("data/price2010.csv")
      ))[amount > 0, ][trade_date > 20100000 & trade_date < 20150000, ]
      fwrite(price1, "data/price2010.csv", row.names = F, quote = FALSE)
    }

    if (nrow(pricedata[trade_date > 20150000 & trade_date < 20190000, ]) > 0) {
      price1 <- unique(rbind(
        pricedata[trade_date > 20150000 & trade_date < 20190000, ],
        fread("data/price2015.csv")
      ))[amount > 0, ][trade_date > 20150000 & trade_date < 20190000, ]
      fwrite(price1, "data/price2015.csv", row.names = F, quote = FALSE)
    }

    if (nrow(pricedata[trade_date > 20200000, ]) > 0) {
      price1 <- unique(rbind(
        pricedata[trade_date > 20200000, ],
        fread("data/price2020.csv")
      ))[amount > 0, ][trade_date > 20200000, ]
      fwrite(price1, "data/price2020.csv", row.names = F, quote = FALSE)
    }

    fwrite(pricedata[trade_date < 20000000, ], "data/pricedata.csv", row.names = F, quote = FALSE)
  } else {
    fwrite(pricedata, "data/pricedate.csv", row.names = F, quote = FALSE)
  }



  Sys.sleep(4)
}
