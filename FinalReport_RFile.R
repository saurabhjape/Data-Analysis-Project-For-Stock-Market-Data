#R CODE: STOCK MARKET DATA ANALYSIS
#Obtained stock market data from Yahoo stock market database. Daily Stock market data is taken from March 1, 2015 to March 1, 2016


#####################################################################
########STEP 1: READING THE DATASET (Reading each csv file)##########
#####################################################################

#IT Industry top 3 companies
IT_Accenture <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/accenture.csv", header=TRUE)
IT_HP <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/hp.csv", header=TRUE)
IT_IBM <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/ibm.csv", header=TRUE)

#Banking industry top 3 companies
Banking_JPMorgan <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/jpm.csv", header=TRUE)
Banking_GoldmanSachs <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/goldman.csv", header=TRUE)
Banking_Citigroup <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/citi.csv", header=TRUE)

#Internet Media industry top 3 companies
InternetMedia_Google <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/google.csv", header=TRUE)
InternetMedia_Tencent <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/tencent.csv", header=TRUE)
InternetMedia_Facebook <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/facebook.csv", header=TRUE)

#Reading Financial Constituent Dataset
financialConstituent <- read.csv("/Users/Saurabh/Documents/SYRA_DOCS/SEM2/IST687/Project/Datasets/constituents-financials.csv", header=TRUE)


#####################################################################
############STEP 2: VIEWING STRUCTURE OF EACH DATA FRAME ############
#####################################################################

str(IT_IBM)
str(IT_Accenture)
str(IT_HP)

str(Banking_JPMorgan)
str(Banking_GoldmanSachs)
str(Banking_Citigroup)

str(InternetMedia_Google)
str(InternetMedia_Tencent)
str(InternetMedia_Facebook)

str(financialConstituent)
#################################################################################
##########STEP 3: INSTALLING PACKAGES REQUIRED FOR THE ENTIRE PROJECT ###########
#################################################################################

#install.packages(""dygraphs")
#install.packages('data.table')
#install.packages("forecast")
#install.packages("xts")
#install.packages("caret")
#install.packages("flexclust")
#install.packages("caTools")"


#################################################################################
###########STEP 4: LOADING PACKAGES REQUIRED FOR THE ENTIRE PROJECT #############
#################################################################################

library(data.table)
library(dygraphs)
library(xts)
library(forecast)
library(caret)
library(flexclust)
library(caTools)


#################################################################################
#### STEP 5: PLOTTING TIME SERIES GRAPHS OF TOP 3 COMPANIES OF EACH INDUSTRY ####
#################################################################################
# Plotting Time Series of Top-3 companies from each industry

#A) IT INDUSTRY
# dygraph() needs xts time series objects for IT industry
ibm_xts <- xts(IT_IBM$Close,order.by=as.POSIXct(IT_IBM$Date),frequency=365)
accen_xts <- xts(IT_Accenture$Close,order.by=as.POSIXct(IT_Accenture$Date),frequency=365)
hp_xts <- xts(IT_HP$Close,order.by=as.POSIXct(IT_HP$Date),frequency=365)
# Creating a new vector for IT industry
stocksITService  <- cbind(ibm_xts,accen_xts,hp_xts)
#Creating IT Sector dygraph
dygraph(stocksITService,ylab="Close", 
        main="IBM, Accenture, and HP Closing Stock Prices") %>%
  dySeries("..1",label="IBM") %>%
  dySeries("..2",label="Accenture") %>%
  dySeries("..3",label="HP") %>%
  dyOptions(colors = c("blue","brown","green")) %>%
  dyRangeSelector()

#B) BANKING INDUSTRY
# dygraph() needs xts time series objects for Banking industry
JpMorgan_xts <- xts(Banking_JPMorgan$Close,order.by=as.POSIXct(Banking_JPMorgan$Date),frequency=365)
GoldmanSachs_xts <- xts(Banking_GoldmanSachs$Close,order.by=as.POSIXct(Banking_GoldmanSachs$Date),frequency=365)
Citigroup_xts <- xts(Banking_Citigroup$Close,order.by=as.POSIXct(Banking_Citigroup$Date),frequency=365)
# Creating a new vector for Banking industry
stocksBanking  <- cbind(JpMorgan_xts,GoldmanSachs_xts,Citigroup_xts)
# Creating Banking Sector dygraph
dygraph(stocksBanking ,ylab="Close", 
        main="JPMorgan Chase, GoldmanSachs, and Citigroup Closing Stock Prices") %>%
  dySeries("..1",label="JPMorgan Chase") %>%
  dySeries("..2",label="GoldmanSachs") %>%
  dySeries("..3",label="Citigroup") %>%
  dyOptions(colors = c("blue","brown","green")) %>%
  dyRangeSelector()

#C) INTERNET MEDIA INDUSTRY
# dygraph() needs xts time series objects for Internet Media industry
Google_xts <- xts(InternetMedia_Google$Close,order.by=as.POSIXct(InternetMedia_Google$Date),frequency=365)
Tencent_xts <- xts(InternetMedia_Tencent$Close,order.by=as.POSIXct(InternetMedia_Tencent$Date),frequency=365)
Facebook_xts <- xts(InternetMedia_Facebook$Close,order.by=as.POSIXct(InternetMedia_Facebook$Date),frequency=365)
# Creating a new vector for Internet Media industry
stocksInternetMedia  <- cbind(Google_xts,Tencent_xts,Facebook_xts)
# Creating Internet Media dygraph
dygraph(stocksInternetMedia ,ylab="Close", 
        main="Google, Tencent, and Facebook Closing Stock Prices") %>%
  dySeries("..1",label="Google") %>%
  dySeries("..2",label="Tencent") %>%
  dySeries("..3",label="Facebook") %>%
  dyOptions(colors = c("blue","brown","green")) %>%
  dyRangeSelector()


#####################################################################
#######STEP 6:: VARIOUS MODELS USED (All Major functions) ###########
#####################################################################

################### MODEL 1: POLYNOMIAL MODEL########################
#Function to calculate time series of polynomial trend
polynomial <- function(stockData) {
  train <- window(stockData, end=c(2015, 190))
  tl = seq(1,253,length=length(train))
  tl2=tl^7
  str(stockData)
  polyStock = lm(train ~tl+tl2)
  tsStocktrend1=ts(polyStock$fit,start=c(2015,1),frequency = 365)
  plot(tsStock,lw=2,col='blue')
  lines(tsStocktrend1,lw=2,col='red')
  return(tsStocktrend1)
}
#########################################################################

####################### MODEL2: ARIMA MODEL #############################
#Used to calculate arima model and return total error
arimaModel <- function(polyTrend, stockData){
  fitArima<-auto.arima(polyTrend)
  plot(forecast(fitArima,h=50),xlim=c(2015,2016.2),ylim=c(90,120), lw=2, col="red", xlab="Time", ylab="Stock Price", main="Predictions of the Polynomial trend")
  test <- window(stockData, start=c(2015,191))
  predfitr =  window(forecast(fitArima,h=39)$mean, start=c(2015,191))
  mae = matrix(NA,25,length(test)+1)
  for(i in 1:length(test))
  {
    mae[1,i]<-abs(predfitr[i]-test[i])
  }
  mae[1,]
  return(sum(mae[1,], na.rm = TRUE))
}
######################################################################


############### MODEL 3: MONTE CARLO PREDICTION MODEL ################
#Function to calculate monte carlo and return total error

montCarlo<-function(stockData){
  
  #Dividing Dataset into Test and Train Data
  trainDataStockMC <-stockData[1:(floor(0.75*nrow(stockData))),]
  testDataStockMC <- stockData[(ceiling(0.75*nrow(stockData))):nrow(stockData),]
  
  #Finding Periodic Daily Return
  setDT(trainDataStockMC)[,PeriodicDailyRetun:=log(trainDataStockMC$Adj.Close/shift(trainDataStockMC$Adj.Close,1,type="lead"))]
  trainDataStockMC$PeriodicDailyRetun[is.na(trainDataStockMC$PeriodicDailyRetun)] <- 0
  averageStock <- mean(trainDataStockMC$PeriodicDailyRetun)
  varianceStock<-var(trainDataStockMC$PeriodicDailyRetun)
  stdDeviationStock<- sd(trainDataStockMC$PeriodicDailyRetun)
  #Calculating Drift
  StockDrift<-averageStock-(varianceStock/2)
  #Setting Seed for Random Variable in Drift
  set.seed(123)
  
  previousDayPriceMC=trainDataStockMC$Adj.Close[nrow(trainDataStockMC)]
  predictedList=c()
  for(i in 1:nrow(testDataStockMC)){
    futurePriceMC=previousDayPriceMC*exp(StockDrift+stdDeviationStock*(qnorm(runif(1))))
    previousDayPriceMC= futurePriceMC
    predictedList[i]<-previousDayPriceMC
  }
  
  #For loop to calculate Total error
  errorSum<-0
  for(i in 1:nrow(testDataStockMC)){
    errorSum = errorSum + abs( predictedList[i]-testDataStockMC$Adj.Close[i])  
  }
  return(errorSum)
}
#########################################################################

##### MODEL 4: CLUSTER-THEN PREDICT MODEL (Identify trends in stocks) #####
clusterPredict<-function(stockData){
  
  stockData<-dichotomousCalc(stockData)
  spl <- sample.split(stockData$PositiveChange, SplitRatio = 0.75)
  stocksTrain <- subset(stockData, spl == T)
  stocksTest <- subset(stockData, spl == F)
  limitedTrain <- stocksTrain
  limitedTrain$PositiveChange <- NULL
  limitedTest <- stocksTest
  limitedTest$PositiveChange <- NULL
  preproc <- preProcess(limitedTrain)
  str(preproc)
  normTrain <- predict(preproc, limitedTrain)
  normTest <- predict(preproc, limitedTest)
  
  summary(normTest)
  set.seed(144)
  km <- kmeans(normTrain[,-1], centers = 3)
  str(km)
  
  # CLUSTERING STOCKS
  # test-set observations assigned to Cluster 2
  km.kcca <- as.kcca(km, normTrain[,-1])
  clusterTrain <- predict(km.kcca)
  clusterTest <- predict(km.kcca, newdata = normTest[,-1])
  table(clusterTest)
  length(clusterTrain)
  length(stocksTrain)
  stockTrain1 <- subset(stocksTrain, clusterTrain == 1)
  stockTrain2 <- subset(stocksTrain, clusterTrain == 2)
  stockTrain3 <- subset(stocksTrain, clusterTrain == 3)
  
  stockTest1 <- subset(stocksTest, clusterTest == 1)
  stockTest2 <- subset(stocksTest, clusterTest == 2)
  stockTest3 <- subset(stocksTest, clusterTest == 3)
  
  #Training set data frame that has the highest average value of the dependent variable
  tapply(stocksTrain$PositiveChange, clusterTrain, mean)
  
  #CLUSTER-SPECIFIC PREDICTIONS: Building logistic regression models
  stocksModel1 <- glm(PositiveChange ~ Open+High+Low+Close+Volume+Adj.Close, data = stockTrain1, family = binomial)
  stocksModel2 <- glm(PositiveChange ~Open+High+Low+Close+Volume+Adj.Close, data = stockTrain2, family = binomial)
  stocksModel3 <- glm(PositiveChange ~Open+High+Low+Close+Volume+Adj.Close, data = stockTrain3, family = binomial)
  
  #Using StocksModel, make test-set predictions called PredictTest on the data frame stocksTest
  predictTest1 <- predict(stocksModel1, newdata = stockTest1, type = "response")
  predictTest2 <- predict(stocksModel2, newdata = stockTest2, type = "response")
  predictTest3 <- predict(stocksModel3, newdata = stockTest3, type = "response")
  
  allPredictions <- c(predictTest1, predictTest2)
  allOutcomes <- c(stockTest1$PositiveChange, stockTest2$PositiveChange)
  
  #Calculate number of predictions with more than 0.8 probability
  predictCount1 = sum(predictTest1>=0.8)
  predictCount2 = sum(predictTest2>=0.8)
  predictCount3 = sum(predictTest3>=0.8)
  
  return(predictCount1+predictCount2+predictCount3)
}

dichotomousCalc<-function(stockData){
  stockData$PositiveChange[1] <- T
  for (i in 2:nrow(stockData)) {
    if (stockData$Close[i] > stockData$Close[i-1] ) {
      stockData$PositiveChange[i] <- T
    } else {
      stockData$PositiveChange[i] <- F
    }
  }
  return(stockData)
}
#########################################################################

######## MODEL 5: ARIMA MODEL PREDICTION on COMPLETE DATASET ############

arimaData<-function(stockData){
  
  #Converting date format
  rdate<-as.Date(stockData$Date, "%m%d%y")
  
  #Converting stock data into time series
  tsStock = ts(rev(stockData$Close),start=c(2015,1),frequency = 365)
  
  #polynomial trend generation
  tl = seq(1,253,length=length(tsStock))
  tl2=tl^7
  polyStock = lm(tsStock ~tl+tl2)
  
  tsStockPolyTrend=ts(polyStock$fit,start=c(2015,1),frequency = 365)
  
  #Auto arima function calculation and forecasting future values
  fitArima<-auto.arima(tsStockPolyTrend) 
  return(forecast(fitArima, h=1)$mean)
}
######################################################################


######################################################################
### STEP 7: ANALYZING WHICH MODEL IS BETTER MONTE CARLO OR ARIMA ####
######################################################################
# Question: Finding which Model is better?

#allStocks vector contains vector names of all stocks
allStocks<-c("IT_Accenture","IT_IBM","IT_HP","Banking_Citigroup","Banking_GoldmanSachs","Banking_JPMorgan","InternetMedia_Facebook","InternetMedia_Tencent","InternetMedia_Google")
#namesOfTopStocks contains vector names of all top stocks from top 3 industries
namesOfTopStocks<-c("IT_Accenture","Banking_JPMorgan","InternetMedia_Facebook")

#Decalring an Array errorTable for having data in them
errorTable = array(NA,dim=c(3,2))
j = 1
#For loop for iterating over top stock of each industry
for (i in namesOfTopStocks){

  #Converting date format
  rdate<-as.Date(get(i)$Date, "%m%d%y")
  
  #Converting stock data into time series
  tsStock = ts(rev(get(i)$Close),start=c(2015,1),frequency = 365)
  
  #General Plot of each stock
  plot(tsStock)
  
  #Generating polynomial trend of each stock
  polytrend<-polynomial(tsStock)
  
  #Appending ARIMA error values of each stock one-by-one
  errorTable[j,1] =  arimaModel(polytrend, tsStock)
  #append(arimaError, arimaModel(polytrend, tsStock), after = length(arimaError))
  
  #Appending MonteCarlo error values of each stock one-by-one 
  errorTable[j,2] = montCarlo(get(i))
  j = j+1
}
#Conclusion : ARIMA is better than Monte-Carlo
###########################################################################

###########################################################################
######## STEP 8: Evaluating Stock using Cluster-Predict Method ############
###########################################################################

#Calculating trends of all stocks
trend = matrix(NA,nrow = 9, ncol = 2)
k = 1
#For loop for iterating over each stock
for (i in allStocks){
  
  #Name of the stock stored in matrix
  trend[k,1] = allStocks[k]
  
  #Trend calculation, one which has maximum number of increases or positivity in their trends
  trend[k,2] = clusterPredict(get(i))
  
  k = k+1
}

trend <- trend[order(trend[,2],decreasing=TRUE),]
#CONCLUSION: The TOP 3 companies are Banking_GoldmanSachs (7) , IT_HP (6) , Banking_Citigroup (4)
##############################################################################


################################################################################
######## STEP 9: Forecasting THE TOP 3 STOCKS Prices Using ARIMA Model  ########
################################################################################

## Calculating Forecast values for top 3 best trend stocks using ARIMA model
trendForecast = matrix(NA,nrow = 3, ncol = 3)
colnames(trendForecast) <- c("Stock","Close","Forecast")
k = 1
#For loop for iterating over each stock
for (i in trend){
  #Name of the stock stored in matrix
  trendForecast[k,1] = trend[k]
  
  #Last days closing Stock value
  trendForecast[k,2] = get(i)$Close[1]
  
  #Trend calculation, one which has maximum number of increases or positivity in their trends
  trendForecast[k,3] = arimaData(get(i))
  k = k+1
  if(k == 4){
    break
  }
}
#Displaying the Forecasted values
trendForecast
###########################################################################

############################################################################
### STEP 10: Analyzing Companinies using Financial Constituents Dataset  ###
############################################################################

###Selecting the companies whose stock values we predicted
financialConstituentSelected<- financialConstituent[financialConstituent$Symbol %in% c('ACN','GOOGL','C','FB','GS','HPE','IBM','JPM'),]

#Displaying the Market Capitalization of each company
barplot(financialConstituentSelected$Market.Cap, names.arg = financialConstituentSelected$Name, las = 2, ylab = "Market Capitalization", 
        main = "Market Capitalization according to company", col=c("darkblue","red"))

#Looking at the table, we might theorize that companies with higher Market capitalization could have higher dividends/Yield
plot(financialConstituentSelected$Market.Cap, financialConstituentSelected$Dividend.Yield, main = "Market Capitalization vs. Dividends/Yield", 
     xlab="Market Cap", ylab = "Div/Yield", pch=19, col=c("darkblue","red"))

#################### Validating a good Stock using conventional numbers ####################
#Factor 1: Compare price/earnings ratios to operating performance and financial condition
barplot(financialConstituentSelected$Price.Earnings, names.arg = financialConstituentSelected$Name, las = 2, ylab = "Price/Earning", 
        main = "Comparing Price/Earnings", col=c("darkblue","red"))

#Calculate the number of Outstanding Shares
outstandingShares<-financialConstituentSelected$Market.Cap/financialConstituentSelected$Price*1000000

#Calculate the Net Income
netIncome<-outstandingShares*financialConstituentSelected$Earnings.Share

barplot(netIncome/1000000, names.arg = financialConstituentSelected$Name, las = 2, ylab = "Net Income in million", 
        main = "Comparing operating performance", col=c("darkblue","red"))

#Factor 2: Comparing Profitability
barplot((financialConstituentSelected$Earnings.Share/financialConstituentSelected$Book.Value), names.arg = financialConstituentSelected$Name, las = 2, ylab = "Profitability", 
        main = "Comparing Profitability", col=c("darkblue","red"))

#Factor 3: Comparing Dividend Yield vs Price Earnings
barplot(financialConstituentSelected$Dividend.Yield, names.arg = financialConstituentSelected$Name, las = 2, ylab = "Dividends Yield", 
        main = "Dividends according to company", col=c("darkblue","red"))

#Factor 4: Comparing Price Spread using Stock history
barplot(financialConstituentSelected$X52.week.high/financialConstituentSelected$X52.week.low, names.arg = financialConstituentSelected$Name, las = 2, ylab = "Stock Increase Ratio", 
        main = "Comparing Price Spread", col=c("darkblue","red"))

#Factor 5: Is predicted Price near 52 Week High?
mydf <- data.frame( X1=financialConstituentSelected$X52.week.high, X2=financialConstituentSelected$X52.week.low)
barplot(t(as.matrix(mydf)), names.arg = financialConstituentSelected$Name, las=2, beside=TRUE, col=c("darkblue","red"),main = "Comparing 52 week high/low",legend = c("52 Week High","52 Week Low"))      


############################################################################
#################################### END ###################################
############################################################################