#We build a regression model for prediction of stock price based on technical indicators
#Use the packages TTR and Quantmod

library(quantmod);
library(TTR);

getSymbols("AAPL",src="google") # from google finance 

chartSeries(AAPL, theme="white", subset='last 16 weeks')
addMACD()

barChart(AAPL,theme="white", subset='last 16 weeks') 
candleChart(AAPL,theme="white",subset='last 16 weeks') 

AAPL <- to.weekly(AAPL)
chartSeries(AAPL, theme="white")
addSMA()

data <- cbind(AAPL,BBands(AAPL[,2:4]), MACD(AAPL[,"APPL.Close"]), momentum(AAPL), SMA(AAPL))
data <- data[34:dim(data)[1], ]

lm <-lm(AAPL.Close ~ AAPL.Volume + dn + up + macd + AAPL.High.1 + SMA, data)
 
summary(lm)
