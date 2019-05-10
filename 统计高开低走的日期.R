rm(list=ls())
source(file.path(substr(getwd(),1,22),"header.R"))
sym="000001.SH"
loadRData(sym)



applyInd(sym,fun.name = "lagX",
         arguments=list(x=get(sym),column="close",k=1),
         label="lag1")
applyInd(sym,fun.name="multipleX",
         arguments=list(x=get(sym),column="lag1",f=1.01),
         label="temp")

applyInd(sym,fun.name="sigComparisionx",
         arguments=list(x=get(sym),columns=c("temp","open"),relationship="lt"),
         label="high015")## 高开超过1%

applyInd(sym,fun.name="sigComparisionx",
         arguments=list(x=get(sym),columns=c("close","open"),relationship="lt"),
         label="godown")## 低走

applyInd(sym,fun.name="sigComparisionx",
         arguments=list(x=get(sym),columns=c("close","lag1"),relationship="lt"),
         label="green")## 收绿

applyInd(sym,fun.name="sigAndx",
         arguments=list(x=get(sym),columns=c("high015","godown")),
         label="mysig")


applyInd(sym,fun.name="sigAndx",
         arguments=list(x=get(sym),columns=c("mysig","green")),
         label="mySig")


events.day<-index(get(sym))[which(get(sym)[,ncol(get(sym))]==1)]
y<-get(sym)

events<-xts(paste0(as.character(events.day),"   ",round(as.numeric(y[events.day,4]),0),"点"),
            as.Date(events.day,"%Y-%m-%d"))

print(xts::plot.xts(y[,4],main = "上证综指高开1%以上且当天收跌",))
# text(c(index(y)[c(which(y.peak),which(y.valley))],index(y)[c(which(y.peak),which(y.valley))]),as.numeric(y[events,"close"]),label="aa")

print(addEventLines(events,srt=90,pos=2,col="red"))
