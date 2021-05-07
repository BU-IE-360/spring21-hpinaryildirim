
#HW2
library(tidyverse)
library(lubridate)
library(forecast)
library(zoo)
library(ggplot2)
library(dplyr)
library(data.table)
library(corrplot)


setwd("C:\\Users\\Pýnar YILDIRIM\\Desktop")
house_sales =fread("house_sales.csv")
head(house_sales)
names(house_sales)
house_sales[,UNIXTIME:=NULL]
house_sales <- rename(house_sales, date=Tarih, sales=TP_AKONUTSAT3_TOPLAM,
                      cpibased_exchange_rate=TP_RK_T1_Y,cpi=TP_FG_J0, financial_situation=TP_TG2_Y11,
                      prob_of_saving=TP_TG2_Y12, wage_expectation=TP_TG2_Y16, prob_buy=TP_TG2_Y19, interest=TP_KTF12)
house_sales[,date:=ym(date)]
house_sales=house_sales[order(date)]
head(house_sales)

#visualization of target
ggplot(house_sales,aes(x=date,y=sales))+geom_line()
hist(house_sales$sales)
qqnorm(house_sales$sales)

#correlations
cors<- cor(house_sales[,c(2,3,4,5,6,7,8)], use="pairwise.complete.obs") 
corrplot.mixed(cors, tl.col="black", tl.pos = "lt")


ccf(house_sales$cpibased_exchange_rate,house_sales$sales)
ggplot(house_sales,aes(x=sales,y=cpibased_exchange_rate))+geom_point()

ggplot(house_sales,aes(x=sales,y=cpi))+geom_point()
ccf(house_sales$cpi,house_sales$sales,)

ggplot(house_sales,aes(x=sales,y=financial_situation))+geom_point()
ccf(house_sales$financial_situation,house_sales$sales,)

ggplot(house_sales,aes(x=sales,y=prob_of_saving))+geom_point()
ccf(house_sales$sales,house_sales$prob_of_saving)

ggplot(house_sales,aes(x=sales,y=wage_expectation))+geom_point()
cor(house_sales$sales,house_sales$wage_expectation)
ccf(house_sales$wage_expectation,house_sales$sales,)

ggplot(house_sales,aes(x=sales,y=prob_buy))+geom_point()
ccf(house_sales$sales,house_sales$prob_buy)

ggplot(house_sales,aes(x=sales,y=interest))+geom_point()
ccf(house_sales$interest,house_sales$sales)


#TREND
house_sales[,trend:=1:.N]

hs_reg=lm(sales~trend,data=house_sales)
summary(hs_reg)
house_sales[,trend_constant:=predict(hs_reg,house_sales)]
ggplot(house_sales ,aes(x=date)) +
  geom_line(aes(y=sales,color='real')) + 
  geom_line(aes(y=trend_constant,color='trend'))

checkresiduals(hs_reg)


#months

house_sales[,month:=as.character(lubridate::month(date,label=T))]
head(house_sales)
hs_regm=lm(sales~trend+month,data=house_sales)
summary(hs_regm)
checkresiduals(hs_regm)
house_sales[,trend_constant_month:=predict(hs_regm,house_sales)]
ggplot(house_sales ,aes(x=date)) +
  geom_line(aes(y=sales,color='real')) + 
  geom_line(aes(y=trend_constant_month,color='trend'))

#add year
house_sales[,year:=as.character(lubridate::year(date))]
head(house_sales)
hs_regmy=lm(sales~trend+month+year,data=house_sales)
summary(hs_regmy)
checkresiduals(hs_regmy)

house_sales[,trend_constant_my:=predict(hs_regmy,house_sales)]
ggplot(house_sales ,aes(x=date)) +
  geom_line(aes(y=sales,color='real')) + 
  geom_line(aes(y=trend_constant_my,color='trend'))


#add lagged variables
house_sales[,lag3:=lag(house_sales$sales,3)]
house_sales[,lag1int:=lag(interest,1)]
house_sales[,lag10exchange:=lag(cpibased_exchange_rate,10)]
house_sales[,lag11buy:=lag(prob_buy,11)]

hs_regtry=lm(sales~trend+month+year+lag3+lag1int+lag10exchange+lag11buy,data=house_sales)
summary(hs_regtry)
checkresiduals(hs_regtry)

house_sales[,trend_constant_try:=predict(hs_regtry,house_sales)]
ggplot(house_sales ,aes(x=date)) +
  geom_line(aes(y=sales,color='real')) + 
  geom_line(aes(y=trend_constant_try,color='trend'))


#get forecast
house_sales_pred=rbind(house_sales,data.table(date=ymd("2021-04-01")),fill=T)
house_sales_pred[,trend:=1:.N]
house_sales_pred[,month:=as.character(lubridate::month(date,label=T))]
house_sales_pred[,year:=as.character(lubridate::year(date))]
house_sales_pred[,lag3:=lag(sales,3)]
house_sales_pred[,lag1int:=lag(interest,1)]
house_sales_pred[,lag10exchange:=lag(cpibased_exchange_rate,10)]
house_sales_pred[,lag11buy:=lag(prob_buy,11)]

house_sales_pred[,predicted:=predict(hs_regtry,house_sales_pred)]

house_sales_pred[date=="2021-04-01",]$predicted






