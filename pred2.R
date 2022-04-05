cookie = 'kbzw_r_uname=maoxiong; kbz_newcookie=1; kbzw__user_login=7Obd08_P1ebax9aX5MPl2eDm4d-Cq47k2ujc8NvpxtS-otvZ25bSldXb2Z2ynqnEqJLd26qvlaiS19uqobDQrMamgrKk6OHFzr6fqq6eqY2yj8ui1dSexdDqyuDl1piumqeCnrjg5dfn2OOBws2Vmqmap52WuODlqayckNmqrbCJ6-Kxm6qPp6CTv8bTzOOop5mqnKeTppKXvdzqxtbQ7Kiunaec; kbzw__Session=el1e7pbamtaafblbqqabkd14s0'
headers2 <- c('Accept'='application/json',
             'Content-Type'='text/plain',          
             'User-Agent'='Mozilla/5.0 (Windows NT 6.1) AppleWebKit/537.  36 (KHTML, like Gecko) Chrome/49.0.2623.221 Safari/537.36 SE 2.X Met aSr 1.0',          
             'cookie'= cookie
)

jsl1  = GET("https://www.jisilu.cn/data/cbnew/cb_list/?___jsl=LST___t=1554547931751", add_headers(.headers = headers2))

jsl1 = fromJSON(  content( jsl1,"text") ) 
jsl1 =as.data.table(jsl1$rows$cell)  

if (nrow(jsl1)>100) {
  
  tmp = jsl1[,.(zzcode=bond_id,zzname=bond_nm
                ,name=stock_nm,price=full_price
                ,pb,value=convert_value
                ,yijia=premium_rt,pingji=rating_cd
                ,remain=year_left,convertflag=convert_cd_tip
                ,ytm_rt
                ,code=stock_id
                ,cprice = as.double(convert_price) 
                ,rday = ifelse(is.na(redeem_dt),50,as.Date(redeem_dt) - Sys.Date())
                ,forceprice=as.double(force_redeem_price)
                ,putprice = as.double(put_convert_price)
                ,redeemprice = as.integer(redeem_price)
                ,tflag =ifelse( abs(0.1-abs(as.numeric(sub("%", "",sincrease_rt))/100))   <0.01/as.double(sprice)
                                |abs(0.05 -abs(as.numeric(sub("%", "",sincrease_rt))/100))   <0.01/as.double(sprice) ,1,0)  
  )]
  
  
  
  jsl2 = GET("https://www.jisilu.cn/data/cbnew/pre_list/?___jsl=LST___t=1554548125413")
  jsl2 = fromJSON(  content( jsl2,"text") )
  jsl2 = as.data.table(jsl2$rows$cell)
  tmp2 = jsl2[,.(zzcode=bond_id,zzname=bond_nm,name=stock_nm,value=pma_rt
                 ,pingji=rating_cd,code =stock_id )]
  tmp2 = tmp2[!is.na(zzname),]
  
  tmp2 = tmp2[,price:=100][,pb:=2]
  tmp2 = tmp2[,convertflag:='未到转股期'][,ytm_rt:=0.01][,yijia:=100-as.double(value)]
  
  tmp3=rbind(tmp[,.(zzcode,zzname,name,code,value=as.double(value),pingji
                    ,price=as.double(price)
                    ,pb=as.double(pb)
                    ,convertflag
                    ,ytm_rt=as.numeric(sub("\\%", "", ytm_rt))/100
                    ,yijia=as.numeric(sub("\\%", "", yijia))/100
                    ,lifecycle=as.double(remain)
                    ,rate1 = as.integer(round(forceprice/cprice*100)) ,rate2 = as.integer(round(putprice/cprice*100))
                    ,redeemprice,rday,tflag)]
             ,tmp2[!zzcode %in% tmp$zzcode,.(zzcode,zzname,name,code,value=as.double(value),pingji
                                             ,price,pb,convertflag,ytm_rt,yijia,lifecycle=6,rate1=130,rate2=70,redeemprice=110
                                             ,rday=50,tflag=0)])
  
  tmp3 = tmp3[,rday:=ifelse(lifecycle<0.25,lifecycle*200,rday)]
  
  tmp3 = pad.null(tmp3,0)
  
  tmp3 = tmp3[,eb:=grepl(pattern = "EB",zzname)]
  tmp3 = tmp3[,remain:=ifelse(substr(convertflag,1,5)=='未到转股期',(lifecycle+0.501 - as.integer(lifecycle+0.501) ),0),by=zzcode]
  tmp3 = tmp3[,pingji:=ifelse(pingji=='AAA',4,
                              ifelse(pingji=='AA+',3,
                                     ifelse(pingji=='AA',2,
                                            ifelse(pingji=='AA-',1,0)
                                     )))]
  
  tmp3 = tmp3[,code:=ifelse(nchar(code)==8,substr(code,3,8),code)]
  
  rm(jsl1)
  rm(jsl2)
  #zhuanzhai = merge(unique(result[,.(code,score=score5-1)]),tt[YJL>-100,.(code=SWAPSCODE,name=SECURITYSHORTNAME,zzname=SNAME,zzcode=BONDCODE,price= as.double(ZGJZGJJZ)*(1++ as.double(YJL)/100)  ,yijia=as.numeric(YJL)/100,pb=PB),],by="code",all.y = T)
  
  zhuanzhai = merge(unique(result[,.(code,score=score5-1)])
                    ,tmp3,by="code",all.y = T)
  
  zhuanzhai = zhuanzhai[order(-ytm_rt)]
  zhuanzhai = zhuanzhai[,flag:=1]
  zhuanzhai = zhuanzhai[,ytmorder:=cumsum(flag)]
  zhuanzhai = zhuanzhai[,ytmorder:=ifelse(ytmorder<41,41-ytmorder,0) - 36-(32-rday)*2 - ifelse(rday<15,(32-rday)*5,0)]
  
  
  adjust2 = fread("pingji.csv")
  adjust2 = adjust2[,.(zzcode=as.character(zzcode)
                       ,power1,power1d,power2,power2d,power3,power3d
  )]
  
  library(openxlsx)
  adjust3 = as.data.table(read.xlsx("zhuanzhai.xlsx", sheet = 1, startRow = 1, colNames = TRUE,
                                    rowNames = FALSE, detectDates = FALSE, skipEmptyRows = TRUE,
                                    skipEmptyCols = TRUE, rows = NULL, cols = NULL, check.names = FALSE,
                                    namedRegion = NULL, na.strings = "NA", fillMergedCells = FALSE))
  
  adjust2 = merge(adjust2[,.(zzcode,power1,power1d,power2,power2d,power3,power3d)]
                  ,adjust3[,.(zzcode,cost1,cost2,cost3,cost4)]
                  ,by='zzcode',all.y = T)
  
  adjust2 = adjust2[,.(power=0.1*sum(power1, na.rm =T)
                       +0.1*sum(power1d, na.rm =T)
                       +0.2*sum(power2, na.rm =T)
                       +0.1*sum(power2d, na.rm =T)
                       +0.3*sum(power3, na.rm =T)
                       +0.1*sum(power3d, na.rm =T)
  ),by=.(zzcode,cost1,cost2,cost3,cost4)]
  
  zhuanzhai = merge(zhuanzhai,adjust2[,.(zzcode,power)],by="zzcode",all.x = T)
  zhuanzhai = pad.null(zhuanzhai,0)
  zhuanzhai = merge(zhuanzhai,adjust2[,.(zzcode,cost1,cost2,cost3,cost4)],by="zzcode",all.x = T)
  zhuanzhai = pad.null(zhuanzhai,'')
  
  zhuanzhai = zhuanzhai[,.(zzcode,name,zzname,price,yijia,eb=as.integer(eb),remain,pingji=pingji/2,pb,power,lifecycle,rate1,rate2,redeemprice,rday,tflag,ytmorder
                           ,score,cost1,cost2,cost3,cost4)]
  
  load("model/cv_zz_model.Rdata")
  predset_lv2 = xgb.DMatrix(data = as.matrix(apply(zhuanzhai[,4:15,with=F],2,as.numeric)
  ))
  zhuanzhai = cbind(zhuanzhai,data.table(score2=predict(cv_zz_model,predset_lv2)))
  zhuanzhai = zhuanzhai[,score2:=as.integer(score2*10)]
  
  #zhuanzhai = zhuanzhai[,yijia:=ifelse(yijia<0.3,yijia,yijia-min(max((pb-1)*(0.1+power),0),(105-as.double(price) )/70)),by=zzcode]
  
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
  
  zhuanzhai = zhuanzhai[,score1:=round(score*400*(10*(ifelse(yijia<0,0,yijia)+0.1)+10)/(8*ifelse(yijia<0,0,yijia)+1.8))]
  
  zhuanzhai = zhuanzhai[,final:=score1+score2+ytmorder][order(-final)]
  
  zhuanzhai = zhuanzhai[,.(zzcode,name,zzname,price,yijia,cost1,cost2,cost3,cost4,score1,score2,final,ytmorder)]
  write.xlsx(unique(zhuanzhai[,.(zzcode,name,zzname,price,yijia,cost1,cost2,cost3,cost4,final,score1)]), "zhuanzhai.xlsx", sheetName="Sheet1",col.names=TRUE, row.names=FALSE, append=FALSE,overwrite = T)
  
  
  gc()

} else {
  stop('cookie error')
}
  


