
strategy.ReturnRevert<-function(sym1,sym2,r.nDay,sma1.nDay,sma2.nDay){
  
  strategy.name=paste0(sym1," Vs ",sym2)

  source(file.path(substr(getwd(),1,22),"header.R"))
  source("AexcessB.R")
  
  #correct for TZ issues if they crop up
  oldtz<-Sys.getenv('UTC')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  
  # Set initial values
  
  symA=sym1
  symB=sym2
  syms<-c(symA,symB)
  
  return.nDay=r.nDay
  SMA1.nDay=sma1.nDay
  SMA2.nDay=sma2.nDay
  
  startDate='2005-1-4'
  initEq=100000000
  tradeSize = initEq/length(syms)
  assign("tradeSize",tradeSize,envir=.GlobalEnv)
  
  
  currency("USD")
  for(symbol in syms){ # establish tradable instruments
    stock(symbol, currency="USD",multiplier=1)
  }
  
  
  initPortf(strategy.name, symbols=syms)
  initAcct(strategy.name, portfolios=strategy.name, initEq=initEq)
  initOrders(portfolio=strategy.name)
  print("setup completed")
  portfolio.st=strategy.name
  # Initialize a strategy object
  strategy(strategy.name, store=TRUE)
  
  
  
  AexcessB(
    symA = symA,
    symB = symB,
    return.nDay = return.nDay,
    SMA1.nDay = SMA1.nDay,
    SMA2.nDay=SMA2.nDay,
    startDate = "2005",
    only.last = T
  )
  
  applyInd(
    symA,
    fun.name="sigCrossover",
    arguments = list(
      label="longBuy",
      data=get(symA),
      columns=c(paste0("excess.SMA1",return.nDay),paste0("excess.SMA2",return.nDay)),
      relationship="gt"
    ),
    label="longBuy"
  )
  
  applyInd(
    symA,
    fun.name="sigCrossover",
    arguments = list(
      label="longSell",
      data=get(symA),
      columns=c(paste0("excess.SMA1",return.nDay),paste0("excess.SMA2",return.nDay)),
      relationship="lt"
    ),
    label="longSell"
  )
  
  tmp<-get(symA)
  tmp<-cbind(tmp,0,0)
  names(tmp)[c(ncol(tmp)-1,ncol(tmp))]<-paste0("X",symA,".",c("shortBuy","shortSell"))
  assign(symA,tmp,envir=.GlobalEnv)
  
  idx<-(ncol(get(symA))-3):ncol(get(symA))
  tmp<-get(symA)[,idx]
  names(tmp)<-paste0("X",symB,".",c("shortBuy","shortSell","longBuy","longSell"))
  assign(symB,cbind(get(symB),tmp),envir=.GlobalEnv)
  
  
  
  idx.excess<-grep(paste0("excess",return.nDay),colnames(get(symA)))
  idx.sma<-grep(paste0("SMA"),colnames(get(symA)))
  print(plot(get(symA)[,c(idx.excess,idx.sma)],main=secName[grep(symA,secName$code),2]),legend.loc="topleft")
  
  
  for(sym in syms){
    addPosLimit(
      portfolio=strategy.name,
      symbol=sym, 
      timestamp=startDate,  
      maxpos=initEq)
    
  }
  
  
  
  add.signal(strategy.name, name='sigThreshold',
             arguments = list(
               column=c("longBuy"),
               threshold=1,
               relationship="eq"
             ),
             label='longBuy'
  )
  add.signal(strategy.name, name='sigThreshold',
             arguments = list(
               column=c("longSell"),
               threshold=1,
               relationship="eq"
             ),
             label='longSell'
  )
  
  add.signal(strategy.name, name='sigThreshold',
             arguments = list(
               column=c("shortBuy"),
               threshold=1,
               relationship="eq"
             ),
             label='shortBuy'
  )
  add.signal(strategy.name, name='sigThreshold',
             arguments = list(
               column=c("shortSell"),
               threshold=1,
               relationship="eq"
             ),
             label='shortSell'
  )
  
  
  
  
  # There are two rules:
  # The first is to buy 
  add.rule(
    strategy.name,
    name = 'ruleSignal',
    arguments = list(
      sigcol = "longBuy",
      sigval = 1,
      orderqty = 100,
      osFUN="osFixedDollar",
      ordertype = 'market',
      orderside = 'long',
      TxnFees = 0,
      prefer = 'close'
    ),
    type = 'enter',
    path.dep = TRUE
  )
  
  # The second is to sell 
  add.rule(
    strategy.name,
    name = 'ruleSignal',
    arguments = list(
      sigcol = "longSell",
      sigval = 1,
      orderqty = 'all',
      ordertype = 'market',
      orderside = 'long',
      TxnFees = 0,
      prefer = 'close'
    ),
    type = 'exit',
    path.dep = TRUE
  )
  
  
  add.rule(
    strategy.name,
    name = 'ruleSignal',
    arguments = list(
      sigcol = "shortBuy",
      sigval = 1,
      orderqty = -100,
      osFUN="osFixedDollar",
      ordertype = 'market',
      orderside = 'short',
      TxnFees = 0,
      prefer = 'close'
    ),
    type = 'enter',
    path.dep = TRUE
  )
  
  # The second is to sell 
  add.rule(
    strategy.name,
    name = 'ruleSignal',
    arguments = list(
      sigcol = "shortSell",
      sigval = 1,
      orderqty = 'all',
      ordertype = 'market',
      orderside = 'short',
      TxnFees = 0,
      prefer = 'close'
    ),
    type = 'exit',
    path.dep = TRUE
  )
  
  # Process the indicators and generate trades
  start_t<-Sys.time()
  out<-try(applyStrategy(strategy=strategy.name, portfolios=strategy.name))
  end_t<-Sys.time()
  print("Strategy Loop:")
  print(end_t-start_t)
  
  # look at the order book
  #print(getOrderBook('ReturnRevert'))
  
  start_t<-Sys.time()
  updatePortf(Portfolio=strategy.name)
  updateAcct(strategy.name)
  updateEndEq(strategy.name)
  end_t<-Sys.time()
  print("trade blotter portfolio update:")
  print(end_t-start_t)
  
  
  ret1 <- PortfReturns(strategy.name)
  ret1$total <- rowSums(ret1)
  print(charts.PerformanceSummary(ret1$total, geometric=FALSE, wealth.index=TRUE,main=secName[grep(symA,secName$code),2]))
  
  assign(paste0("R",symA),ret1$total)
  save(list=c(paste0("R",symA)),file=paste0("R",symA,".RData"))
  
  
  
  
}

