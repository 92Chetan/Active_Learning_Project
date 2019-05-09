# Quantitative Finance on S&P-500 index in US stock exchange.
# Objective : Main objective of this project is to gain insight of S&P index on static data over a 
# period of time and apply the knowledge to perform the time series analysis for multiple assets yearly
# returns and predicting future returns using ARIMA model.


# PART-1
# Connecting the SQL server to Rstudio

library(RODBC)
myconn<-odbcDriverConnect(connection ="Driver={SQL Server Native Client 11.0};server=localhost;database=StockMarket_Dataset;trusted_connection=yes;")
Stock<-sqlFetch(myconn,"cf",colnames = FALSE)
Stock


# Viewing the data
View(Stock)                                   # view -1
# name of all the columns 
colnames(Stock)
# number of rows in the data 
nrow(Stock)
# number of columns in the data
ncol(Stock)
# structure of the data
str(Stock) 
# summary command will tell you about NA's (total of 10 na's)
summary(Stock)

###############################################################################################################################


# PART-2 

# DATA CLEANING

# checking for the na's in the data

View(is.na(Stock)) 
# removing the na's from the data 
Stock_new<-na.omit(Stock)
nrow(Stock_new)
# Checking the numeric columns in the dataset.

# which columns in the dataset have numeric values and don't have numeric values.
sapply(Stock_new, is.numeric)
# taking all the numeric columns of the dataset and saving it in the variable.

my_num_data <- Stock_new[, sapply(Stock_new, is.numeric)]

my_num_data
View(my_num_data)                        # view-3

# converting into matrix format, assigning it to a variable, and saving it by writing it into a csv file.
Stock_mat<-data.matrix(my_num_data)
Stock_mat
View(Stock_mat)                         # view-4
write.csv(as.data.frame(Stock_mat),"STOCK_correlation.csv")
getwd()

################################################################################################################################

# PART-3
# CORRELATION :

# finding the correlation between the parameters
cor(Stock_mat)
# viewing the correlation
View(cor(Stock_mat))                     # view-5
write.csv(as.data.frame(Stock_mat),"CORRELATION.csv")
getwd()
# viewing the correlation in the tableau.


##################################################################################################################################

# PART - 4 
# TIME SERIES ANALYSIS 


# installing the quantmod package 
library(quantmod)
#?quantmod
library(RCurl)
library(dplyr)
library(PerformanceAnalytics)
library(ggplot2)
# stock data for single asset

# Facebook

maxdate<-"2018-04-09"
portfolio_FB<-na.omit(getSymbols.yahoo('FB',from=maxdate,auto.assign=FALSE)[,])
portfolio_FB
View(portfolio_FB)
plot(portfolio_FB,col ='BLUE')

# APPLE

maxdate<-"2018-04-09"
portfolio_AAPL<-na.omit(getSymbols.yahoo('AAPL',from=maxdate,auto.assign=FALSE)[,])
portfolio_AAPL
View(portfolio_AAPL)
plot(portfolio_AAPL,col ='PURPLE')

# IBM

maxdate<-"2018-04-09"
portfolio_IBM<-na.omit(getSymbols.yahoo('IBM',from=maxdate,auto.assign=FALSE)[,])
portfolio_IBM
View(portfolio_IBM)
plot(portfolio_IBM,col ='BROWN')

# GOOGLE

maxdate<-"2018-04-09"
portfolio_GOOGL<-na.omit(getSymbols.yahoo('GOOGL',from=maxdate,auto.assign=FALSE)[,])
portfolio_GOOGL
View(portfolio_GOOGL)
plot(portfolio_GOOGL,col ='BLACK')

# MISCROSOFT

maxdate<-"2018-04-09"
portfolio_MSFT<-na.omit(getSymbols.yahoo('MSFT',from=maxdate,auto.assign=FALSE)[,])
portfolio_MSFT
View(portfolio_MSFT)
plot(portfolio_MSFT,col ='RED')


################################################################################################################################

#PART-5 
#PORTFOLIO OPTIMIZATION

# stock data for protfolio for multiple assets
# creating the vector of ticker
mdate<-'2018-04-09'
tickers <- c("AAPL", "IBM", "GOOGL","MSFT")
portfolioPrices<- NULL
for(ticker in tickers)
  portfolioPrices<- cbind(portfolioPrices,getSymbols(ticker,from=mdate,auto.assign=F)[,4])
portfolioPrices
View(portfolioPrices) 
plot(portfolioPrices)
is.na(portfolioPrices)
View(is.na(portfolioPrices))

# changing the column names of the portfolioPrices
colnames(portfolioPrices)<- tickers
View(portfolioPrices)

write.csv(as.data.frame(portfolioPrices),"PORTFOLIOPRICES_CLOSE.csv")
getwd()

##############################################################################################################################

#PART-6
# Time series analysis of monthly , daily and weekly returns and forecasting using tableau visualization.



library(quantmod)
library(dplyr)

# Method -1     calculating returns 
portfolioReturns<-ROC(portfolioPrices,type = 'discrete')
#(ROC - Rate Of Change)
is.na(portfolioReturns)
Returns<-na.omit(portfolioReturns)
print(Returns)
plot(Returns)

# Monthly Returns
portfolioReturns<-monthlyReturn(portfolioPrices)
print(portfolioReturns)
View(portfolioReturns)

portfolioReturns<-tickers %>% lapply(function(x) getSymbols(x,auto.assign = F)) %>% 
  lapply(function(x) monthlyReturn(Ad(x)))
print(portfolioReturns)


# merge them all together
M_Returns<-do.call(merge,portfolioReturns)
print(M_Returns)
View(M_Returns)


# changing the header names of the dataframe

colnames(M_Returns)<-tickers
colnames(M_Returns)
print(M_Returns)
View(M_Returns)
plot(M_Returns)

#exporting the dataframe into .csv file 

write.csv(as.data.frame(final_Returns),"MONTHLY_Returns.csv")
getwd()

###############################################################################################

# Weekly Returns
portfolioReturns2<-weeklyReturn(portfolioPrices)
print(portfolioReturns2)

portfolioReturns2<-tickers %>% lapply(function(x) getSymbols(x,auto.assign = F)) %>% 
  lapply(function(x) weeklyReturn(Ad(x)))
print(portfolioReturns2)

# merge them all together
W_Returns<-do.call(merge,portfolioReturns2)
print(W_Returns)


# changing the header names of the dataframe

colnames(W_Returns)<-tickers
colnames(W_Returns)
print(W_Returns)
View(W_Returns)
plot(W_Returns)


#exporting the dataframe into .csv file 

write.csv(as.data.frame(final_Returns),"WEEKLY_Returns.csv")


#################################################################################################

# Daily Returns
portfolioReturns<-dailyReturn(portfolioPrices)
print(portfolioReturns)
View(portfolioReturns)

portfolioReturns<-tickers %>% lapply(function(x) getSymbols(x,auto.assign = F)) %>% 
  lapply(function(x) dailyReturn(Ad(x)))
print(portfolioReturns)
# merge them all together
D_Returns<-do.call(merge,portfolioReturns)
print(D_Returns)

# changing the header names of the dataframe

colnames(D_Returns)<-tickers
colnames(D_Returns)
print(D_Returns)
View(D_Returns)
plot(D_Returns)

#exporting the dataframe into .csv file 

write.csv(as.data.frame(final_Returns),"DAILY_Returns.csv")


#############################################################################################################

# PART - 7

# STOCK FORECASTING USING ARIMA MODEL 
# Loading libraries :
library(quantmod)
library(MASS)
library(tseries)
library(forecast)


# APPLE

maxdate<-"2009-06-09"
portfolio_AAPL<-na.omit(getSymbols.yahoo('AAPL',from=maxdate,auto.assign=FALSE,periodicity = 'monthly')[,1])
portfolio_AAPL
View(portfolio_AAPL)
View(is.na(portfolio_AAPL))
plot(portfolio_AAPL,col ='PURPLE')
lnstock<-portfolio_AAPL
lnstock

# Plot and convert to log format

lnstock = log(lnstock[1:95])
lnstock

# Auto-correlation plot (to show the correlation between the lags)

acf(lnstock,lag.max = 20)

# Partial auto-correlation plot 

pacf(lnstock,lag.max = 20)
difflnstock=diff(lnstock,1)
difflnstock
adf.test(lnstock)
adf.test(difflnstock)

# Time series and auto arima 

?ts
pricearima <- ts(lnstock,start = c(2009,07),frequency = 12)
fitlnstock<-auto.arima(pricearima)
fitlnstock
plot(pricearima,type='l')
title("APPLE PRICE")
exp(lnstock)

# Forecasted value from ARIMA 

forecastedvalue_ln=forecast(fitlnstock,h=25)
forecastedvalue_ln
plot(forecastedvalue_ln)

forecastedvaluesextracted = as.numeric(forecastedvalue_ln$mean)
# using exp() to convert back the log value into price value.
finalforecastvalues = exp(forecastedvaluesextracted)
finalforecastvalues

# Percentage error
# taking rest of the data and comparing it with the forecasted values generated by arima model 

df<-data.frame(lnstock$AAPL.Open[96:120],finalforecastvalues)
col_heading<-c("Actual Price","Forecasted Price")
names(df)<-col_heading
attach(df)
df


# Ljung- Box Test 

Box.test(fitlnstock$residuals,lag = 5,type = "Ljung-Box")
Box.test(fitlnstock$residuals,lag = 10,type="Ljung-Box")
Box.test(fitlnstock$residuals,lag = 15,type="Ljung-Box")






