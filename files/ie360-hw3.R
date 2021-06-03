##IE 360
##HW3

library(data.table)
library(ggplot2)
library(lubridate)
library(urca)
library(forecast)
getwd()
setwd("C://Users//Pýnar YILDIRIM//Desktop//IE360")
consumption=read.csv("RealTimeConsumption-01012016-20052021.csv")
str(consumption)
consumption = as.data.table(consumption)

summary(consumption)
setnames(consumption, "Consumption..MWh.", "Consumpt")
consumption$Consumpt=gsub("\\.", "", (consumption$Consumpt))
consumption$Consumpt=gsub("\\,", ".", (consumption$Consumpt))
consumption$Consumpt=as.numeric(consumption$Consumpt)
consumption$Date=as.Date(consumption$Date, format="%d.%m.%Y")
consumption$DateTime = paste(consumption$Date,consumption$Hour)
consumption$DateTime = as_datetime(consumption$DateTime, format="%Y-%m-%d %H:%M")
summary(consumption)

(ggplot(data=consumption, aes(x=DateTime,y=Consumpt))+geom_line()
  +labs(x="Date",y="Consumption (MWh)",title="Hourly Consumption")
  +theme_minimal(base_size=14)
)


try=consumption[consumption$Date>="2016-01-01"&consumption$Date<="2017-01-01"]
try[try$Consumpt==0]

(ggplot(data=consumption, aes(x=DateTime,y=Consumpt))+geom_line()
  +labs(x="Date",y="Consumption (MWh)",title="Hourly Consumption")
  +theme_minimal(base_size=14)
)

(ggplot(data=consumption[consumption$Date>="2017-01-01"&consumption$Date<="2020-01-01"], aes(x=DateTime,y=Consumpt))+geom_line()
  +labs(x="Date",y="Consumption (MWh)",title="Hourly Consumption for 3 years")
  +theme_minimal(base_size=14)
)

(ggplot(data=consumption[consumption$Date>="2017-02-01"&consumption$Date<="2017-03-01"], aes(x=DateTime,y=Consumpt))+geom_line()
  +labs(x="Date",y="Consumption (MWh)",title="Hourly Consumption for 1 Month")
  +theme_minimal(base_size=14)
)

(ggplot(data=consumption[consumption$Date>="2017-02-01"&consumption$Date<="2017-02-03"], aes(x=DateTime,y=Consumpt))+geom_line()
  +labs(x="Date",y="Consumption (MWh)",title="Hourly Consumption for 3 Days ")
  +theme_minimal(base_size=14)
)


#daily
consumption_daily=ts(consumption$Consumpt,frequency = 24)
daily_decompose=decompose(consumption_daily)
plot(daily_decompose)

#weekly
consumption_weekly=ts(consumption$Consumpt,frequency = 24*7)
weekly_decompose=decompose(consumption_weekly)
plot(weekly_decompose)

#yearly
consumption_yearly=ts(consumption$Consumpt,frequency = 24*360)
yearly_decompose=decompose(consumption_yearly)
plot(yearly_decompose)


#additive
consumption_ts=ts(consumption$Consumpt,frequency = 24*7)
consumption_ts_decomp=decompose(consumption_ts,type="additive")
plot(consumption_ts_decomp)

consumption$additive_trend=consumption_ts_decomp$trend
(ggplot(data=consumption, aes(x=DateTime,y=additive_trend))+geom_line()
  +labs(x="Date",y="Trend Cycle",title="Trend Cycle Component")
  +theme_minimal(base_size=14)
)
consumption$additive_seasonal=consumption_ts_decomp$seasonal
(ggplot(data=consumption, aes(x=DateTime,y=additive_seasonal))+geom_line()
  +labs(x="Date",y="Seasonality",title="Seasonal Component")
  +theme_minimal(base_size=14)
)
(ggplot(data=consumption[consumption$Date>="2017-02-01"&consumption$Date<="2017-03-01"], aes(x=DateTime,y=additive_seasonal))+geom_line()
  +labs(x="Date",y="Seasonality",title="Seasonal Component")
  +theme_minimal(base_size=14)
)


#multiplicative
consumption_ts=ts(consumption$Consumpt,frequency = 24*7)
consumption_ts_decomp_m=decompose(consumption_ts,type="multiplicative")
plot(consumption_ts_decomp_m)

consumption$multiplicative_trend=consumption_ts_decomp_m$trend
(ggplot(data=consumption, aes(x=DateTime,y=multiplicative_trend))+geom_line()
  +labs(x="Date",y="Trend Cycle",title="Trend Cycle Component")
  +theme_minimal(base_size=14)
)
consumption$multiplicative_seasonal=consumption_ts_decomp_m$seasonal
(ggplot(data=consumption, aes(x=DateTime,y=multiplicative_seasonal))+geom_line()
  +labs(x="Date",y="Seasonality",title="Seasonal Component")
  +theme_minimal(base_size=14)
)
(ggplot(data=consumption[consumption$Date>="2017-02-01"&consumption$Date<="2017-03-01"], aes(x=DateTime,y=multiplicative_seasonal))+geom_line()
  +labs(x="Date",y="Seasonality",title="Seasonal Component")
  +theme_minimal(base_size=14)
)

consumption$random=consumption_ts_decomp$random
(ggplot(data=consumption, aes(x=DateTime,y=random))+geom_line()
  +labs(x="Date",y="Random Consumption",title="Random Component")
  +theme_minimal(base_size=14)
)
test1=ur.kpss(consumption$random) 
summary(test1)

acf(consumption[complete.cases(consumption)]$random)
pacf(consumption[complete.cases(consumption)]$random)


aic=list()
for(i in 0:5){
    aic[i] = AIC(arima(consumption$random, order=c(i,0,0)))
}
print(aic)

aic2=list()
for(i in 0:5){
  aic2[i] = AIC(arima(consumption$random, order=c(0,0,i)))
}
print(aic2)

arima(consumption$random, order=c(0,0,5))
arima(consumption$random, order=c(3,0,0))
arima(consumption$random, order=c(3,0,5))

AIC(arima(consumption$random, order=c(2,0,5)))
AIC(arima(consumption$random, order=c(3,0,4)))
AIC(arima(consumption$random, order=c(4,0,5)))
AIC(arima(consumption$random, order=c(3,0,6)))


train_set=ts(consumption[DateTime<"2021-05-06"]$random)
model=arima(train_set, order=c(2,0,5))

test_set=consumption[DateTime>"2021-05-06",]


forecasted=forecast(model,h=362)
forecasted

str(test_set)
test_set$forecasts=forecasted$mean
test_set[,prediction:=forecasts+as.numeric(additive_trend)+as.numeric(additive_seasonal)]

(ggplot(data=test_set, aes(x=DateTime, y=Consumpt))+geom_line(aes(color="consumption"))
  +geom_line(aes(y=prediction,color="prediction"))
  +labs(x="Date",y="Consumption",title="Forecasts vs Actual")
  +theme_minimal(base_size=14))

test_set[,error:=abs(Consumpt-prediction)]
test_set[,perc_error:=error/Consumpt]
test_set[,mean_byday:=mean(perc_error),by=Date]
test_set$mean_byday

days=(unique(test_set$Date))
errors=test_set[,mean(mean_byday),by=Date]
print(cbind(days,errors))
