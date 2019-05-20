source(file.path(substr(getwd(),1,22),"header.R"))
source("AexcessB.R")
source("ReturnRevertNew.R")

#correct for TZ issues if they crop up
oldtz<-Sys.getenv('UTC')
if(oldtz=='') {
  Sys.setenv(TZ="UTC")
}

loadRData("secName")


for(sy in substr(dir(path = dataPath,pattern = "882"),1,9)){

  symA="000016.SH"
  symB<-"399006.SZ"
  
  
  
  AexcessB(symA,symB,60,startDate="2010",only.last=T)
  
  idx.excess<-grep(paste0("excess60D"),colnames(get(symA)))
  idx.sma<-grep(paste0("SMA"),colnames(get(symA)))
  print(plot(get(symA)[,c(idx.excess,idx.sma)],main=secName[grep(symA,secName$code),2]),legend.loc="topleft")
  
}

library(readxl)
XX<-read_excel("成份及权重.xlsx")##50指数
source("ReturnRevertNew.R")
SS<-XX[[2]]

symB<-"000016.SH"
for(sy in SS){
  
  strategy.ReturnRevert(sy,symB,r.nDay = 60,sma1.nDay = 10,sma2.nDay = 30)
  
}



for(x in SS){
  
  load(paste0("R",x,".RData"))
  
}

anR<-sapply(SS,function(x) table.AnnualizedReturns(get(paste0("R",x))),simplify = T)
anR<-do.call(rbind,anR)



anR<-anR[order(anR[,3],decreasing = T),]

RR<-vector(mode = "list",length=length(SS))

for(i in 1:length(SS)){
  RR[[i]]<-get(paste0("R",SS[i]))
}
names(RR)<-SS
num=5
RS<-RR[substr(rownames(anR)[1:num],1,9)]
print(secName[match(substr(rownames(anR)[1:num],1,9),secName$code),])
RS<-do.call(merge,RS)
RS<-RS['2012/']
RS[is.na(RS)]<-0
W= xts(matrix(1/num,nrow=nrow(RS),ncol=num),order.by=index(RS))
RP<-xts(diag(RS%*%t(W)),order.by=index(RS))
names(RP)<-"市场中性策略"
SS<-charts.PerformanceSummary(RP,wealth.index = T,main="市场中性策略净值曲线")
table.AnnualizedReturns(RP)
table.Monthly(RP)




