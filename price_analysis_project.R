#-------------------------------------------------------------------------------
#---Project: Exploring the behavior of food prices in Ghana
#---Author: Mark Zingbagba
#---Date: 7 June 2020
#-------------------------------------------------------------------------------

#1 Loading the required libraries
library(tidyverse)
library(imputeTS)
library(urca) 
library(lmtest)
library(vars)
library(tseries)
library(tsDyn)
library(zoo)
library(Hmisc)
library(plm)
library(pco)

#2 Importing food prices data obtained from the World Food Programme
data <- read_csv("food_prices_wfp.csv")
data2 <- as_tibble(data)
#Renaming columns (removing spaces and capital letters)
data3 <- rename(data2, country = Country, region = `Admin 1`, market = Market, commodity = Commodity, 
       price_type = `Price Type`, year = Year, month = Month, price = Price, unit = Unit, currency = Currency)

#3 Dealing with missing data
data4 <- data3 %>% filter(!data3$market %in% c("Bolga","Yendi","Tema","Wa","Koforidua","Ejura","Saboba","Banda","Bimbilla","Bunkprugu",
                                                     "Gushegu","Kpandai","Nalerigu","Zabzugu","Bawku","Bongo","Fumbisi","Garu","Navrongo",
                                                     "Jirapa","Lawra","Nadowli","Tumu","Wichau","Dambai","Kadjebi","Kete Krachi","Kpassa","Nkwanta"))

#Limiting the time to July 2017
data4 <- data4 %>% filter(!(year == 2017 & month > 7 ))
data4 <- data4 %>% filter(!(year == 2018))
#data4 looks good. But imported rice has a lot of missing data. So I remove it
data4 <- data4 %>% filter(!data4$commodity %in% c("Rice (imported)"))

#Joining CPI data
cpi <- read_csv("food_cpi.csv")
cpi <- as_tibble(cpi)
cpi <- cpi %>% filter(!(year == 2017 & month > 7 ))
cpi <- cpi %>% filter(!(year == 2018))

#Both month and year are in separate columns. 
#Combining the two to form a month-year date, which will later help in plotting
data4$date <- as.yearmon(paste(data4$year, data4$month), "%Y%m")
cpi$date <- as.yearmon(paste(cpi$year, cpi$month), "%Y%m")

#Joining two data-frames
data44 <- inner_join(data4, mycpi, by = c("market","commodity", "date"), sort = F)
# Calculating real prices
data44$price <- data44$price/(data44$cpi/100)

data45 <- data44[ , !(colnames(data44) %in% c("unit"))]
data5 <- spread(data45, key = commodity, value = price)

names(data5)[12:16] = c("cassava", "maize", "plantain_apentu", "rice_local", "yam")

#Spreading the data for analysis 2 (Market analysis by commodity prices)
data44$price_kg <- ifelse(data44$unit == "100 KG", data44$price/100, ifelse(data44$unit == "50 KG", data44$price/50, ifelse(data44$unit == "91 KG", data44$price/91, ifelse(data44$unit == "100 Tubers", data44$price/375, data44$price/24.5))) )
data6 <- data44

# Dropping the unit variable and old unconverted prices
data6 <- data6[ , !(colnames(data6) %in% c("unit"))]
data6 <- data6[ , !(colnames(data6) %in% c("price"))]

##Rearranging into a wide form
data7 <- spread(data6, key = commodity, value = price_kg)
names(data7)[12:16] = c("cassava", "maize", "plantain_apentu", "rice_local", "yam")

#4 Treating the problem of missing data
#Percentage of missing data 
sum(is.na(data5))/prod(dim(data5))*100 
sum(is.na(data7))/prod(dim(data7))*100 #1.99 % missing

#Linear interpolation to fill missing data
#data5
data5$cassava <- na.interpolation(data5$cassava, option = "linear")
data5$maize <- na.interpolation(data5$maize, option = "linear")
data5$plantain_apentu <- na.interpolation(data5$plantain_apentu, option = "linear")
data5$rice_local <- na.interpolation(data7$rice_local, option = "linear")
data5$yam <- na.interpolation(data5$yam, option = "linear")

#data7
data7$cassava <- na.interpolation(data7$cassava, option = "linear")
data7$maize <- na.interpolation(data7$maize, option = "linear")
data7$plantain_apentu <- na.interpolation(data7$plantain_apentu, option = "linear")
data7$rice_local <- na.interpolation(data7$rice_local, option = "linear")
data7$yam <- na.interpolation(data7$yam, option = "linear")

sum(is.na(data5))/prod(dim(data5))*100 
sum(is.na(data7))/prod(dim(data7))*100 

#5 Data Visualization
#Plots for analysis 1 (Commodity price analysis by market)
mygraph <- filter(data5, year.x == 2010)
ggplot(data = mygraph) +
  geom_line(mapping = aes(x = date, y = plantain_apentu, color = market))

#Plots for analysis 2 (Market analysis by commodity prices)
graph1 <- filter(data7, market == "Kumasi", year.x == 2008)
graph2 <- filter(data7, market == "Kumasi", year.x == 2009)
graph3 <- filter(data7, market == "Kumasi", year.x == 2010)
graph4 <- filter(data7, market == "Kumasi", year.x == 2011)
dev.new()

ggplot(graph1, aes(date)) + 
  geom_line(aes(y = cassava, colour = "cassava")) + 
  geom_line(aes(y = maize, colour = "maize")) +
  geom_line(aes(y = plantain_apentu, colour = "plantain_apentu")) + 
  geom_line(aes(y = rice_local, colour = "rice_local")) +
  geom_line(aes(y = yam, colour = "yam")) +
  labs(x = "Year", y = "Price per commodity", 
       title = "Monthly average price of selected food crops") + scale_x_yearmon(format = "%b-%Y")
dev.new()
ggplot(graph4, aes(date)) + 
  geom_line(aes(y = cassava, colour = "cassava")) + 
  geom_line(aes(y = maize, colour = "maize")) +
  geom_line(aes(y = plantain_apentu, colour = "plantain_apentu")) + 
  geom_line(aes(y = rice_local, colour = "rice_local")) +
  geom_line(aes(y = yam, colour = "yam")) +
  labs(x = "Year", y = "Price per commodity", 
       title = "Monthly average price of selected food crops") + scale_x_yearmon(format = "%b-%Y")

#6 Summary Statistics
#Analysis 1 (Commodity price analysis by market)
sum1 <- data5[,c("date","year.x","month.x","cpi","cassava","maize", "plantain_apentu", "rice_local", "yam")]
summary(sum1)
#Correlation between prices
res <- data5[,c("cassava","maize", "plantain_apentu", "rice_local", "yam")]
res2 <- rcorr(as.matrix(res))

#Analysis 2 (Market analysis by commodity prices)
sum2 <- data7[,c("date","year.x","month.x","cpi","cassava","maize", "plantain_apentu", "rice_local", "yam")]
summary(sum2)
#7.2.1 Correlation between prices
res3 <- data7[,c("cassava","maize", "plantain_apentu", "rice_local", "yam")]
res4 <- rcorr(as.matrix(res3))

#7 MOdelling
#Panel unit root tests
#Levinlin-Chu
purtest(cassava ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "levinlin")
purtest(maize ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "levinlin")
purtest(plantain_apentu ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "levinlin")
purtest(rice_local ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "levinlin")
purtest(yam ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "levinlin")
#IPS
purtest(cassava ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "ips")
purtest(maize ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "ips")
purtest(plantain_apentu ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "ips")
purtest(rice_local ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "ips")
purtest(yam ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "ips")
#Madwu
purtest(cassava ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "madwu")
purtest(maize ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "madwu")
purtest(plantain_apentu ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "madwu")
purtest(rice_local ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "madwu")
purtest(yam ~ 1, data = data5, index = c("market", "month.x"), pmax = 20,lags = c("AIC"), test = "madwu")

#Analysis: Market analysis by commodity prices
##Kumasi
kumasi <- data5 %>% filter(data5$market %in% c("Kumasi"))
##Obuasi
obuasi <- data5 %>% filter(data5$market %in% c("Obuasi"))
##Sunyani
sunyani <- data5 %>% filter(data5$market %in% c("Sunyani"))
#Techiman
techiman <- data5 %>% filter(data5$market %in% c("Techiman"))
#Cape Coast
cape <- data5 %>% filter(data5$market %in% c("Cape Coast"))
#Mankessim
mankessim <- data5 %>% filter(data5$market %in% c("Mankessim"))
#Accra
accra <- data5 %>% filter(data5$market %in% c("Accra"))
#Tamale
tamale <- data5 %>% filter(data5$market %in% c("Tamale"))
#Ho
ho <- data5 %>% filter(data5$market %in% c("Ho"))
#Sekondi/Takoradi
sekondi <- data5 %>% filter(data5$market %in% c("Sekondi/Takoradi"))

##Examining non-stationarity of data
##Lag length selection for ADF unit root test
#KUMASI MARKET 
VARselect((kumasi$cassava), lag.max = 5, type = "const")
VARselect((kumasi$maize), lag.max = 5, type = "const")
VARselect((kumasi$plantain_apentu), lag.max = 5, type = "const")
VARselect((kumasi$rice_local), lag.max = 5, type = "const")
VARselect((kumasi$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((kumasi$cassava), lag.max = 5, type = "trend")
VARselect((kumasi$maize), lag.max = 5, type = "trend")
VARselect((kumasi$plantain_apentu), lag.max = 5, type = "trend")
VARselect((kumasi$rice_local), lag.max = 5, type = "trend")
VARselect((kumasi$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

#OBUASI MARKET
VARselect((obuasi$cassava), lag.max = 5, type = "const")
VARselect((obuasi$maize), lag.max = 5, type = "const")
VARselect((obuasi$plantain_apentu), lag.max = 5, type = "const")
VARselect((obuasi$rice_local), lag.max = 5, type = "const")
VARselect((obuasi$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((obuasi$cassava), lag.max = 5, type = "trend")
VARselect((obuasi$maize), lag.max = 5, type = "trend")
VARselect((obuasi$plantain_apentu), lag.max = 5, type = "trend")
VARselect((obuasi$rice_local), lag.max = 5, type = "trend")
VARselect((obuasi$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

##SUNYANI MARKET
VARselect((sunyani$cassava), lag.max = 5, type = "const")
VARselect((sunyani$maize), lag.max = 5, type = "const")
VARselect((sunyani$plantain_apentu), lag.max = 5, type = "const")
VARselect((sunyani$rice_local), lag.max = 5, type = "const")
VARselect((sunyani$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((sunyani$cassava), lag.max = 5, type = "trend")
VARselect((sunyani$maize), lag.max = 5, type = "trend")
VARselect((sunyani$plantain_apentu), lag.max = 5, type = "trend")
VARselect((sunyani$rice_local), lag.max = 5, type = "trend")
VARselect((sunyani$yam), lag.max = 5, type = "trend")

##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

##TECHIMAN MARKET
VARselect((techiman$cassava), lag.max = 5, type = "const")
VARselect((techiman$maize), lag.max = 5, type = "const")
VARselect((techiman$plantain_apentu), lag.max = 5, type = "const")
VARselect((techiman$rice_local), lag.max = 5, type = "const")
VARselect((techiman$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((techiman$cassava), lag.max = 5, type = "trend")
VARselect((techiman$maize), lag.max = 5, type = "trend")
VARselect((techiman$plantain_apentu), lag.max = 5, type = "trend")
VARselect((techiman$rice_local), lag.max = 5, type = "trend")
VARselect((techiman$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

#CAPE COAST MARKET
VARselect((cape$cassava), lag.max = 5, type = "const")
VARselect((cape$maize), lag.max = 5, type = "const")
VARselect((cape$plantain_apentu), lag.max = 5, type = "const")
VARselect((cape$rice_local), lag.max = 5, type = "const")
VARselect((cape$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((cape$cassava), lag.max = 5, type = "trend")
VARselect((cape$maize), lag.max = 5, type = "trend")
VARselect((cape$plantain_apentu), lag.max = 5, type = "trend")
VARselect((cape$rice_local), lag.max = 5, type = "trend")
VARselect((cape$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

##MANKESSIM MARKET
VARselect((mankessim$cassava), lag.max = 5, type = "const")
VARselect((mankessim$maize), lag.max = 5, type = "const")
VARselect((mankessim$plantain_apentu), lag.max = 5, type = "const")
VARselect((mankessim$rice_local), lag.max = 5, type = "const")
VARselect((mankessim$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((mankessim$cassava), lag.max = 5, type = "trend")
VARselect((mankessim$maize), lag.max = 5, type = "trend")
VARselect((mankessim$plantain_apentu), lag.max = 5, type = "trend")
VARselect((mankessim$rice_local), lag.max = 5, type = "trend")
VARselect((mankessim$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

##ACCRA MARKET
VARselect((accra$cassava), lag.max = 5, type = "const")
VARselect((accra$maize), lag.max = 5, type = "const")
VARselect((accra$plantain_apentu), lag.max = 5, type = "const")
VARselect((accra$rice_local), lag.max = 5, type = "const")
VARselect((accra$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((accra$cassava), lag.max = 5, type = "trend")
VARselect((accra$maize), lag.max = 5, type = "trend")
VARselect((accra$plantain_apentu), lag.max = 5, type = "trend")
VARselect((accra$rice_local), lag.max = 5, type = "trend")
VARselect((accra$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

##TAMALE MARKET
VARselect((tamale$cassava), lag.max = 5, type = "const")
VARselect((tamale$maize), lag.max = 5, type = "const")
VARselect((tamale$plantain_apentu), lag.max = 5, type = "const")
VARselect((tamale$rice_local), lag.max = 5, type = "const")
VARselect((tamale$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((tamale$cassava), lag.max = 5, type = "trend")
VARselect((tamale$maize), lag.max = 5, type = "trend")
VARselect((tamale$plantain_apentu), lag.max = 5, type = "trend")
VARselect((tamale$rice_local), lag.max = 5, type = "trend")
VARselect((tamale$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

##HO MARKET
VARselect((ho$cassava), lag.max = 5, type = "const")
VARselect((ho$maize), lag.max = 5, type = "const")
VARselect((ho$plantain_apentu), lag.max = 5, type = "const")
VARselect((ho$rice_local), lag.max = 5, type = "const")
VARselect((ho$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((ho$cassava), lag.max = 5, type = "trend")
VARselect((ho$maize), lag.max = 5, type = "trend")
VARselect((ho$plantain_apentu), lag.max = 5, type = "trend")
VARselect((ho$rice_local), lag.max = 5, type = "trend")
VARselect((ho$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)

##SEKONDI/TAKORADI MARKET
VARselect((sekondi$cassava), lag.max = 5, type = "const")
VARselect((sekondi$maize), lag.max = 5, type = "const")
VARselect((sekondi$plantain_apentu), lag.max = 5, type = "const")
VARselect((sekondi$rice_local), lag.max = 5, type = "const")
VARselect((sekondi$yam), lag.max = 5, type = "const")
#With deterministic trend
VARselect((sekondi$cassava), lag.max = 5, type = "trend")
VARselect((sekondi$maize), lag.max = 5, type = "trend")
VARselect((sekondi$plantain_apentu), lag.max = 5, type = "trend")
VARselect((sekondi$rice_local), lag.max = 5, type = "trend")
VARselect((sekondi$yam), lag.max = 5, type = "trend")
##Augmented Dickey-Fuller Tests
adf.test(((kumasi$cassava)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$maize)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$plantain_apentu)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$rice_local)), alternative = c("stationary"), k = 3)
adf.test(((kumasi$yam)), alternative = c("stationary"), k = 3)



