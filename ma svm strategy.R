rm(list = ls(all = TRUE))

install.packages("quantmod")
install.packages("e1071")
install.packages("PerformanceAnalytics")
install.packages("ggplot2")
require(quantmod)
require(PerformanceAnalytics)
require(e1071)
require(ggplot2)

getSymbols("^GSPC",from = "2000-01-01", to = "2015-01-01")
Data <- GSPC
period <- 10
MAData <- SMA(Cl(Data),n = period)

Price <- lag(MAData,-1)-lag(MAData,0)
Class <- ifelse(Price>0,"Up","Down")

cci <- CCI(HLC(Data),n=20,c=0.015)
cmf <- CMF(HLC(Data),Vo(Data),n=20)
cmo <- CMO(Cl(Data),n=14)
dvi <- DVI(Cl(Data))$dvi
macd <- MACD(Cl(Data),nFast=12,nSlow=26,nSig=9)$macd
mfi <- MFI(HLC(Data),Vo(Data),n=14)
obv <- OBV(Cl(Data),Vo(Data))
Momentum <- momentum(Cl(Data))
rsi <- RSI(Cl(Data),n=14)
Stoch <- stoch(HLC(Data),nFastK=14,nFastD=3,nSlowD=3)
fastk <- Stoch$fastK
fastd <- Stoch$fastD
slowd <- Stoch$slowD
tdi <- TDI(Cl(Data),n=20,multiple=2)$tdi
williamsad <- williamsAD(HLC(Data))
wpr <- WPR(HLC(Data),n=14)
#Features from TTR package


DataSet <- data.frame(cci,cmf,cmo,dvi,macd,mfi,obv,Momentum,rsi,fastk,fastd,slowd,tdi,williamsad,wpr,Class)
DataSet <- DataSet[-c(1:251),]
Data <- Data[-c(1:251),]
if(nrow(Data)!=nrow(DataSet)){
  print("Error: different rows of Data and DataSet")
}
#check
select <- seq(1,nrow(DataSet),10)
DataSet.Select <- DataSet[select,]
colnames(DataSet) <- c("CCI","CMF","CMO","DVI","MACD","MFI","OBV","MOMENTUM","RSI","FASTK","FASTD","SLOWD","TDI","WILLIAMSAD","WPR","Class")
colnames(DataSet.Select) <- c("CCI","CMF","CMO","DVI","MACD","MFI","OBV","MOMENTUM","RSI","FASTK","FASTD","SLOWD","TDI","WILLIAMSAD","WPR","Class")

w <- 0.7
trainstart <- 1
trainend <- round(w*length(DataSet.Select[,1]))
teststart <- round(w*length(DataSet.Select[,1]))+1
testend <- length(DataSet.Select[,1])

Training <- DataSet.Select[trainstart:trainend,]
Test <- DataSet.Select[teststart:testend,]
#Separate the data into 80% training set to build our model, 20% test set to test the patterns we found


costs <- 1
gammas <- 0.01
kernel <- "radial"
formula = Class~CCI+CMF+CMO+DVI+MACD+MFI+OBV

best.svm <- best.tune(svm,
                      formula,
                      data=Training,kernel = kernel,cost = costs,gamma = gammas,
                      tunecontrol = tune.control(sampling = "cross",cross=5))
#Train SVM

TrainingPredictions<-predict(best.svm,Training,type="class")
TrainingData<-data.frame(Training,TrainingPredictions)
Training.error <- sum(TrainingData[,"Class"]!=TrainingData[,"TrainingPredictions"])/nrow(TrainingData)


modifyMyPos <- function(pos,periods){
   modifypos <- rep(pos,each=periods)
   modifypos <- modifypos[-(1:(periods-1))]
   return(modifypos)
}

myPosition <- function(x,model,periods){
  position <- ifelse(predict(model,x,type="class")=="Up",1,-1)
  modipos <- modifyMyPos(position,periods)
  return(modipos)
}
#Strategy: if the svm model says "Up", long it; else short it.

myStock <- Test
test.start.row <- which(rownames(DataSet) == rownames(Test[1,]))
test.end.row <- which(rownames(DataSet) == rownames(Test[nrow(Test),]))
test <- Cl(Data)[test.start.row:test.end.row,]

if(nrow(test)!=1+(nrow(Test)-1)*period){
  print("Error: test or Test has wrong #row")
}

myposition <- myPosition(x = myStock,model = best.svm,periods=period)
bmkReturns <- dailyReturn(test, type = "arithmetic")
myReturns <- bmkReturns*Lag(myposition,1)
myReturns[1] <- 0

names(bmkReturns) <- 'SP500'
names(myReturns) <- 'My Strategy'

charts.PerformanceSummary(cbind(bmkReturns,myReturns))

Performance <- function(x) {
  
  cumRetx = Return.cumulative(x)
  annRetx = Return.annualized(x, scale=252)
  sharpex = SharpeRatio.annualized(x, scale=252)
  winpctx = length(x[x > 0])/length(x[x != 0])
  annSDx = sd.annualized(x, scale=252)
  
  DDs <- findDrawdowns(x)
  maxDDx = min(DDs$return)
  maxLx = max(DDs$length)
  
  Perf = c(cumRetx, annRetx, sharpex, winpctx, annSDx, maxDDx, maxLx)
  names(Perf) = c("Cumulative Return", "Annual Return","Annualized Sharpe Ratio",
                  "Win %", "Annualized Volatility", "Maximum Drawdown", "Max Length Drawdown")
  return(Perf)
}
cbind(Me=Performance(myReturns),SP500=Performance(bmkReturns))  
sprintf("Training Error: %g",Training.error)

#Conclusion:
##################################################
#w <- 0.7
#costs <- 1
#gammas <- 0.01
#kernel <- "radial"
#tunecontrol = tune.control(sampling = "cross",cross=5)
#formula = Class~CCI+CMF
#Up = 1; Dn = -1
# Me       SP500
# Cumulative Return        5.1163189   0.7593556
# Annual Return            0.5437538   0.1450616
# Annualized Sharpe Ratio  3.6064254   0.9475619
# Win %                    0.5876190   0.5609524
# Annualized Volatility    0.1507736   0.1530893
# Maximum Drawdown        -0.1248292  -0.1938824
# Max Length Drawdown     57.0000000 207.000000
##################################################













