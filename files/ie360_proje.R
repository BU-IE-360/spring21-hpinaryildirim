#Group 11

library(jsonlite)
library(httr)
library(data.table)
library(stats)
library(forecast)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(GGally)
library(corrplot)
library(dplyr)
library(tidyr)
library(readxl)
library(zoo)
library(corrplot)
library(hrbrthemes)
library(viridis)
library(ggridges)

accu=function(actual,forecast){
  n=length(actual)
  error=actual-forecast
  error=complete.cases(error)
  mean=mean(actual)
  #sd=sd(actual)
  #CV=sd/mean
  FBias=sum(error)/sum(actual)
  MAPE=sum(abs(error/actual))/n
  RMSE=sqrt(sum(error^2)/n)
  MAD=sum(abs(error))/n
  MADP=sum(abs(error))/sum(abs(actual))
  WMAPE=MAD/mean
  l=data.frame(n,mean,FBias,MAPE,RMSE,MAD,MADP,WMAPE)
  return(l)
}

get_token <- function(username, password, url_site){
  
  post_body = list(username=username,password=password)
  post_url_string = paste0(url_site,'/token/')
  result = POST(post_url_string, body = post_body)
  
  # error handling (wrong credentials)
  if(result$status_code==400){
    print('Check your credentials')
    return(0)
  }
  else if (result$status_code==201){
    output = content(result)
    token = output$key
  }
  
  return(token)
}

get_data <- function(start_date='2020-03-20', token, url_site){
  
  post_body = list(start_date=start_date,username=username,password=password)
  post_url_string = paste0(url_site,'/dataset/')
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  result = GET(post_url_string, header, body = post_body)
  output = content(result)
  data = data.table::rbindlist(output)
  data[,event_date:=as.Date(event_date)]
  data = data[order(product_content_id,event_date)]
  return(data)
}


send_submission <- function(predictions, token, url_site, submit_now=F){
  
  format_check=check_format(predictions)
  if(!format_check){
    return(FALSE)
  }
  
  post_string="list("
  for(i in 1:nrow(predictions)){
    post_string=sprintf("%s'%s'=%s",post_string,predictions$product_content_id[i],predictions$forecast[i])
    if(i<nrow(predictions)){
      post_string=sprintf("%s,",post_string)
    } else {
      post_string=sprintf("%s)",post_string)
    }
  }
  
  submission = eval(parse(text=post_string))
  json_body = jsonlite::toJSON(submission, auto_unbox = TRUE)
  submission=list(submission=json_body)
  
  print(submission)
  # {"31515569":2.4,"32737302":2.4,"32939029":2.4,"4066298":2.4,"48740784":2.4,"6676673":2.4, "7061886":2.4, "73318567":2.4, "85004":2.4} 
  
  if(!submit_now){
    print("You did not submit.")
    return(FALSE)      
  }
  
  
  header = add_headers(c(Authorization=paste('Token',token,sep=' ')))
  post_url_string = paste0(url_site,'/submission/')
  result = POST(post_url_string, header, body=submission)
  
  if (result$status_code==201){
    print("Successfully submitted. Below you can see the details of your submission")
  } else {
    print("Could not submit. Please check the error message below, contact the assistant if needed.")
  }
  
  print(content(result))
  
}

check_format <- function(predictions){
  
  if(is.data.frame(predictions) | is.data.frame(predictions)){
    if(all(c('product_content_id','forecast') %in% names(predictions))){
      if(is.numeric(predictions$forecast)){
        print("Format OK")
        return(TRUE)
      } else {
        print("forecast information is not numeric")
        return(FALSE)                
      }
    } else {
      print("Wrong column names. Please provide 'product_content_id' and 'forecast' columns")
      return(FALSE)
    }
    
  } else {
    print("Wrong format. Please provide data.frame or data.table object")
    return(FALSE)
  }
  
}

# this part is main code
subm_url = 'http://46.101.163.177'

u_name = "Group11"
p_word = "UcZ2v9SNRuqpQL62"
submit_now = FALSE

username = u_name
password = p_word

token = get_token(username=u_name, password=p_word, url=subm_url)
data = get_data(token=token,url=subm_url)

#BURADAN SONRASI EN SON ÇALIÞTIR

predictions=unique(data[,list(product_content_id)])

forecasts=c(2,2,2,2,2,2,2,2,2)
forecasts[1]=round(prediction_for_1)
forecasts[2]=round(prediction_for_2_new)
forecasts[3]=round(prediction_for_3)
forecasts[4]=round(prediction_for_4)
forecasts[5]=round(prediction_for_5_new)
forecasts[6]=round(prediction_for_6)
forecasts[7]=round(prediction_for_7)
forecasts[8]=round(prediction_for_8_new)
forecasts[9]=round(prediction_for_9)

predictions[,forecast:=forecasts]
predictions

send_submission(predictions, token, url=subm_url, submit_now=F)

###### BURAYA KADAR

data

date_to_forecast="2021-06-26"

#Data download

my_data= read.csv("ProjectRawData.csv")
my_data=as.data.table(my_data)
#Data manipulation
my_data$event_date=ymd(my_data$event_date)


#Data overview
str(my_data)
#str(data)
summary(my_data)

#visualizing the data 3-month intervals
ggplot(data=my_data[event_date>="2020-05-25"&event_date<="2020-08-25",], aes(x=event_date,y=sold_count))+geom_line()+theme_minimal()+facet_wrap(vars(product_content_id),scales="free")+ theme_minimal()+
  labs(title="Time Series Between '2020-05-25' and '2020-08-25' ", x="Date", y="Sold Count")

ggplot(data=my_data[event_date>="2020-08-26"&event_date<="2020-11-25",], aes(x=event_date,y=sold_count))+geom_line()+theme_minimal()+facet_wrap(vars(product_content_id),scales="free")+ theme_minimal()+
  labs(title="Time Series Between '2020-08-26' and '2020-11-25' ", x="Date", y="Sold Count")

ggplot(data=my_data[event_date>="2020-11-26"&event_date<="2021-02-25",], aes(x=event_date,y=sold_count))+geom_line()+theme_minimal()+facet_wrap(vars(product_content_id),scales="free")+ theme_minimal()+
  labs(title="Time Series Between '2021-02-26' and '2021-05-31' ", x="Date", y="Sold Count")

ggplot(data=my_data[event_date>="2021-02-26"&event_date<="2021-05-31",], aes(x=event_date,y=sold_count))+geom_line()+theme_minimal()+facet_wrap(vars(product_content_id),scales="free")+ theme_minimal()+
  labs(title="Time Series Between '2021-02-26' and '2021-05-31' ", x="Date", y="Sold Count")

#visualizing the data alltimes and all products
ggplot(data=my_data, aes(x=event_date,y=sold_count))+geom_line()+theme_minimal()+facet_wrap(vars(product_content_id),scales="free")+theme_minimal()+facet_wrap(vars(product_content_id),scales="free")+ theme_minimal()+
  labs(title="Overall Time Series ", x="Date", y="Sold Count")


###PRODUCT 1

product1= my_data[my_data$product_content_id==31515569,]
#order observations from old to new
product1=(product1[order(product1$event_date),])

#add new data

prd1_add=data[data$product_content_id==31515569,]
prd1_add=prd1_add[event_date>="2021-06-01",]
prd1_add

product1=rbind(product1,prd1_add)


#observations are not normally distributed
hist(product1$sold_count)
qqnorm(product1$sold_count)


#visualizing the data
(ggplot(product1,aes(x=event_date,y=sold_count))+
  geom_line()+
  theme_minimal()+
  labs(title="Product 1",y="Sold Count", x="Event Date")+theme_minimal()+facet_wrap(vars(product_content_id),scales="free")+ theme_minimal()+
    labs(title="Overall Time Series ", x="Date", y="Sold Count")
)

summary(product1)
str(product1)

acf(product1$sold_count,50)


#sold_count is never 0 or NA
#seasonality is not observed by visual inspection

#form a time series object and decompose
#weekly
pr1_ts_w=ts(product1$sold_count, frequency=7,start = min(product1$event_date))
pr1_ts_decompose_wa=decompose(pr1_ts_w)
pr1_ts_decompose_wm=decompose(pr1_ts_w, type="multiplicative")
plot(pr1_ts_decompose_wa)
plot(pr1_ts_decompose_wm)
#multiplicative decomposition is prefered: random term constant variance assumptions

#monthly
pr1_ts_m=ts(product1$sold_count, frequency=30,start = min(product1$event_date))
pr1_ts_decompose_ma=decompose(pr1_ts_m)
pr1_ts_decompose_mm=decompose(pr1_ts_m, type="multiplicative")
plot(pr1_ts_decompose_ma)
plot(pr1_ts_decompose_mm)
#constanr varinace assumption is violated

#multiplicative weekly decomposition
acf(pr1_ts_decompose_wm$random[complete.cases(pr1_ts_decompose_wm$random)],50)
pacf(pr1_ts_decompose_wm$random[complete.cases(pr1_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag4 

#ARIMA
pr1_auto=auto.arima(pr1_ts_decompose_wm$random)
pr1_auto
residuals(pr1_auto)
pr1_fitted = pr1_ts_decompose_wm$random - residuals(pr1_auto)
pr1_fitted_transformed = pr1_fitted*pr1_ts_decompose_wm$seasonal*pr1_ts_decompose_wm$trend

plot(pr1_ts_w, xlab="Date", ylab="Sold Count")
points(pr1_fitted_transformed,type="l", col=2, lty=5)


pr1_predictions=cbind(sold_count=pr1_ts_w,fitted=pr1_fitted_transformed)
pr1_predictions=as.data.table(pr1_predictions)
pr1_predictions$date=product1$event_date

ggplot(pr1_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")+ theme_minimal()+
  labs(title="Product 1 Fitted Values ", x="Date", y="Sold Count")

accu(pr1_predictions$sold_count,pr1_predictions$fitted)
#PREDICTION 
start_date=pr1_predictions[which(is.na(pr1_predictions$fitted)),][4]$date
start_date



pr1_forecast = predict(pr1_auto, n.ahead=4)$pred
pr1_forecast = ts(pr1_forecast, frequency = 7,start = start_date)
pr1_forecast

last_trend=tail(pr1_ts_decompose_wm$trend[!is.na(pr1_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr1_predictions=rbind(pr1_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr1_predictions=cbind(pr1_predictions, seasonality=pr1_ts_decompose_wm$figure)
pr1_predictions=cbind(pr1_predictions, trend=pr1_ts_decompose_wm$trend)

pr1_predictions[date>=start_date, fitted:=pr1_forecast*last_trend]
pr1_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_1=pr1_predictions[date==date_to_forecast, ]$fitted

ggplot(data=pr1_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")
prediction_for_1

###PRODUCT 3

product3= my_data[my_data$product_content_id==32939029,]

#order observations from old to new
product3=(product3[order(product3$event_date),])

#add new data

prd3_add=data[data$product_content_id==32939029,]
prd3_add=prd3_add[event_date>="2021-06-01",]
prd3_add

product3=rbind(product3,prd3_add)


#observations are not normally distributed
hist(product3$sold_count)
qqnorm(product3$sold_count)



#visualizing the data
(ggplot(product3,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 3",y="Sold Count", x="Event Date"))

summary(product3)
str(product3)


acf(product3$sold_count,50)


#form a time series object and decompose
#weekly
pr3_ts_w=ts(product3$sold_count, frequency=7,start = min(product3$event_date))
pr3_ts_decompose_wa=decompose(pr3_ts_w)
pr3_ts_decompose_wm=decompose(pr3_ts_w, type="multiplicative")
plot(pr3_ts_decompose_wa)
plot(pr3_ts_decompose_wm)
# constant variance assumptions violated

#monthly
pr3_ts_m=ts(product3$sold_count, frequency=30,start = min(product3$event_date))
pr3_ts_decompose_ma=decompose(pr3_ts_m)
pr3_ts_decompose_mm=decompose(pr3_ts_m, type="multiplicative")
plot(pr3_ts_decompose_ma)
plot(pr3_ts_decompose_mm)


#multiplicative weekly decomposition
acf(pr3_ts_decompose_wm$random[complete.cases(pr3_ts_decompose_wm$random)],50)
pacf(pr3_ts_decompose_wm$random[complete.cases(pr3_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag30

#ARIMA
pr3_auto=auto.arima(pr3_ts_decompose_wm$random)
pr3_auto
residuals(pr3_auto)
pr3_fitted = pr3_ts_decompose_wm$random - residuals(pr3_auto)
pr3_fitted_transformed = pr3_fitted*pr3_ts_decompose_wm$seasonal*pr3_ts_decompose_wm$trend

plot(pr3_ts_w, xlab="Date", ylab="Sold Count")
points(pr3_fitted_transformed,type="l", col=2, lty=5)

pr3_predictions=cbind(sold_count=pr3_ts_w,fitted=pr3_fitted_transformed)
pr3_predictions=as.data.table(pr3_predictions)
pr3_predictions$date=product3$event_date

ggplot(pr3_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")+ theme_minimal()+
  labs(title="Product 3 Fitted Values ", x="Date", y="Sold Count")
accu(pr3_predictions$sold_count,pr3_predictions$fitted)


#PREDICTIONS

start_date=pr3_predictions[which(is.na(pr3_predictions$fitted)),][4]$date
start_date


pr3_forecast = predict(pr3_auto, n.ahead=4)$pred
pr3_forecast = ts(pr3_forecast, frequency = 7,start = start_date)
pr3_forecast

last_trend=tail(pr3_ts_decompose_wm$trend[!is.na(pr3_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr3_predictions=rbind(pr3_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr3_predictions=cbind(pr3_predictions, seasonality=pr3_ts_decompose_wm$figure)
pr3_predictions=cbind(pr3_predictions, trend=pr3_ts_decompose_wm$trend)

pr3_predictions[date>=start_date, fitted:=pr3_forecast*last_trend]
pr3_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_3=pr3_predictions[date==date_to_forecast, ]$fitted

ggplot(data=pr3_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

###PRODUCT 4

product4= my_data[my_data$product_content_id==4066298,]

#order observations from old to new
product4=(product4[order(product4$event_date),])

#add new data

prd4_add=data[data$product_content_id==4066298,]
prd4_add=prd4_add[event_date>="2021-06-01",]
prd4_add

product4=rbind(product4,prd4_add)


#observations are not normally distributed
hist(product4$sold_count)
qqnorm(product4$sold_count)



#visualizing the data
(ggplot(product4,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 4",y="Sold Count", x="Event Date"))

summary(product4)
str(product4)

acf(product4$sold_count,50)


#form a time series object and decompose
#weekly
pr4_ts_w=ts(product4$sold_count, frequency=7,start = min(product4$event_date))
pr4_ts_decompose_wa=decompose(pr4_ts_w)
pr4_ts_decompose_wm=decompose(pr4_ts_w, type="multiplicative")
plot(pr4_ts_decompose_wa)
plot(pr4_ts_decompose_wm)

# constant variance assumptions violated in additive

#monthly
pr4_ts_m=ts(product4$sold_count, frequency=30,start = min(product4$event_date))
pr4_ts_decompose_ma=decompose(pr4_ts_m)
pr4_ts_decompose_mm=decompose(pr4_ts_m, type="multiplicative")
plot(pr4_ts_decompose_ma)
plot(pr4_ts_decompose_mm)
# constant variance assumptions 

#multiplicative weekly decomposition
acf(pr4_ts_decompose_wm$random[complete.cases(pr4_ts_decompose_wm$random)],50)
pacf(pr4_ts_decompose_wm$random[complete.cases(pr4_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag30

#ARIMA
pr4_auto=auto.arima(pr4_ts_decompose_wm$random)
pr4_auto
residuals(pr4_auto)
pr4_fitted = pr4_ts_decompose_wm$random - residuals(pr4_auto)
pr4_fitted_transformed = pr4_fitted*pr4_ts_decompose_wm$seasonal*pr4_ts_decompose_wm$trend

plot(pr4_ts_w, xlab="Date", ylab="Sold Count")
points(pr4_fitted_transformed,type="l", col=2, lty=5)

pr4_predictions=cbind(sold_count=pr4_ts_w,fitted=pr4_fitted_transformed)
pr4_predictions=as.data.table(pr4_predictions)
pr4_predictions$date=product4$event_date

ggplot(pr4_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")+ theme_minimal()+
  labs(title="Product 4 Fitted Values ", x="Date", y="Sold Count")
accu(pr4_predictions$sold_count,pr4_predictions$fitted)

start_date=pr4_predictions[which(is.na(pr4_predictions$fitted)),][4]$date
start_date

pr4_forecast = predict(pr4_auto, n.ahead=4)$pred
pr4_forecast = ts(pr4_forecast, frequency = 7,start = start_date)
pr4_forecast

last_trend=tail(pr4_ts_decompose_wm$trend[!is.na(pr4_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr4_predictions=rbind(pr4_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr4_predictions=cbind(pr4_predictions, seasonality=pr4_ts_decompose_wm$figure)
pr4_predictions=cbind(pr4_predictions, trend=pr4_ts_decompose_wm$trend)

pr4_predictions[date>=start_date, fitted:=pr4_forecast*last_trend]
pr4_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_4=pr4_predictions[date==date_to_forecast, ]$fitted
prediction_for_4

ggplot(data=pr4_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

###PRODUCT 6

product6= my_data[my_data$product_content_id==6676673,]

#order observations from old to new
product6=(product6[order(product6$event_date),])

#add new data

prd6_add=data[data$product_content_id==6676673,]
prd6_add=prd6_add[event_date>="2021-06-01",]
prd6_add

product6=rbind(product6,prd6_add)


#observations are not normally distributed
hist(product6$sold_count)
qqnorm(product6$sold_count)



#visualizing the data
(ggplot(product6,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 6",y="Sold Count", x="Event Date"))

summary(product6)
str(product6)

acf(product6$sold_count,50)


#form a time series object and decompose
#weekly
pr6_ts_w=ts(product6$sold_count, frequency=7,start = min(product6$event_date))
pr6_ts_decompose_wa=decompose(pr6_ts_w)
pr6_ts_decompose_wm=decompose(pr6_ts_w, type="multiplicative")
plot(pr6_ts_decompose_wa)
plot(pr6_ts_decompose_wm)
# constant variance assumptions violated in additive

#monthly
pr6_ts_m=ts(product6$sold_count, frequency=30,start = min(product6$event_date))
pr6_ts_decompose_ma=decompose(pr6_ts_m)
pr6_ts_decompose_mm=decompose(pr6_ts_m, type="multiplicative")
plot(pr6_ts_decompose_ma)
plot(pr6_ts_decompose_mm)
# constant variance assumptions 

#multiplicative weekly decomposition
acf(pr6_ts_decompose_wm$random[complete.cases(pr6_ts_decompose_wm$random)],50)
pacf(pr6_ts_decompose_wm$random[complete.cases(pr6_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag2

#ARIMA
pr6_auto=auto.arima(pr6_ts_decompose_wm$random)
pr6_auto
residuals(pr6_auto)
pr6_fitted = pr6_ts_decompose_wm$random - residuals(pr6_auto)
pr6_fitted_transformed = pr6_fitted*pr6_ts_decompose_wm$seasonal*pr6_ts_decompose_wm$trend

plot(pr6_ts_w, xlab="Date", ylab="Sold Count")
points(pr6_fitted_transformed,type="l", col=2, lty=5)

pr6_predictions=cbind(sold_count=pr6_ts_w,fitted=pr6_fitted_transformed)
pr6_predictions=as.data.table(pr6_predictions)
pr6_predictions$date=product6$event_date

ggplot(pr6_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")+ theme_minimal()+
  labs(title="Product 6 Fitted Values ", x="Date", y="Sold Count")

accu(pr6_predictions$sold_count,pr6_predictions$fitted)

start_date=pr6_predictions[which(is.na(pr6_predictions$fitted)),][4]$date
start_date

pr6_forecast = predict(pr6_auto, n.ahead=4)$pred
pr6_forecast = ts(pr6_forecast, frequency = 7,start = start_date)
pr6_forecast

last_trend=tail(pr6_ts_decompose_wm$trend[!is.na(pr6_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr6_predictions=rbind(pr6_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr6_predictions=cbind(pr6_predictions, seasonality=pr6_ts_decompose_wm$figure)
pr6_predictions=cbind(pr6_predictions, trend=pr6_ts_decompose_wm$trend)

pr6_predictions[date>=start_date, fitted:=pr6_forecast*last_trend]
pr6_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_6=pr6_predictions[date==date_to_forecast, ]$fitted
prediction_for_6

ggplot(data=pr6_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

###PRODUCT 7

product7= my_data[my_data$product_content_id==7061886,]

#order observations from old to new
product7=(product7[order(product7$event_date),])

#add new data

prd7_add=data[data$product_content_id==7061886,]
prd7_add=prd7_add[event_date>="2021-06-01",]
prd7_add

product7=rbind(product7,prd7_add)


#observations are not normally distributed
hist(product7$sold_count)
qqnorm(product7$sold_count)



#visualizing the data
(ggplot(product7,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 7",y="Sold Count", x="Event Date"))

summary(product7)
str(product7)

acf(product7$sold_count,50)


#form a time series object and decompose
#weekly
pr7_ts_w=ts(product7$sold_count, frequency=7,start = min(product7$event_date))
pr7_ts_decompose_wa=decompose(pr7_ts_w)
pr7_ts_decompose_wm=decompose(pr7_ts_w, type="multiplicative")
plot(pr7_ts_decompose_wa)
plot(pr7_ts_decompose_wm)
# constant variance assumptions violated in additive

#monthly
pr7_ts_m=ts(product7$sold_count, frequency=30,start = min(product7$event_date))
pr7_ts_decompose_ma=decompose(pr7_ts_m)
pr7_ts_decompose_mm=decompose(pr7_ts_m, type="multiplicative")
plot(pr7_ts_decompose_ma)
plot(pr7_ts_decompose_mm)
# constant variance assumptions 

#multiplicative weekly decomposition
acf(pr7_ts_decompose_wm$random[complete.cases(pr7_ts_decompose_wm$random)],50)
pacf(pr7_ts_decompose_wm$random[complete.cases(pr7_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag2

#ARIMA
pr7_auto=auto.arima(pr7_ts_decompose_wm$random)
pr7_auto
residuals(pr7_auto)
pr7_fitted = pr7_ts_decompose_wm$random - residuals(pr7_auto)
pr7_fitted_transformed = pr7_fitted*pr7_ts_decompose_wm$seasonal*pr7_ts_decompose_wm$trend

plot(pr7_ts_w, xlab="Date", ylab="Sold Count")
points(pr7_fitted_transformed,type="l", col=2, lty=5)

pr7_predictions=cbind(sold_count=pr7_ts_w,fitted=pr7_fitted_transformed)
pr7_predictions=as.data.table(pr7_predictions)
pr7_predictions$date=product7$event_date

ggplot(pr7_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")+ theme_minimal()+
  labs(title="Product 7 Fitted Values ", x="Date", y="Sold Count")

accu(pr7_predictions$sold_count,pr7_predictions$fitted)


start_date=pr7_predictions[which(is.na(pr7_predictions$fitted)),][4]$date
start_date

pr7_forecast = predict(pr7_auto, n.ahead=4)$pred
pr7_forecast = ts(pr7_forecast, frequency = 7,start = start_date)
pr7_forecast

last_trend=tail(pr7_ts_decompose_wm$trend[!is.na(pr7_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr7_predictions=rbind(pr7_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr7_predictions=cbind(pr7_predictions, seasonality=pr7_ts_decompose_wm$figure)
pr7_predictions=cbind(pr7_predictions, trend=pr7_ts_decompose_wm$trend)

pr7_predictions[date>=start_date, fitted:=pr7_forecast*last_trend]
pr7_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_7=pr7_predictions[date==date_to_forecast, ]$fitted
prediction_for_7

ggplot(data=pr7_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")


###PRODUCT 9

product9= my_data[my_data$product_content_id==85004,]

#order observations from old to new
product9=(product9[order(product9$event_date),])

#add new data

prd9_add=data[data$product_content_id==85004,]
prd9_add=prd9_add[event_date>="2021-06-01",]
prd9_add

product9=rbind(product9,prd9_add)


#observations are not normally distributed
hist(product9$sold_count)
qqnorm(product9$sold_count)



#visualizing the data
(ggplot(product9,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 9",y="Sold Count", x="Event Date"))

summary(product9)
str(product9)

acf(product9$sold_count,50)

#form a time series object and decompose
#weekly
pr9_ts_w=ts(product9$sold_count, frequency=7,start = min(product9$event_date))
pr9_ts_decompose_wa=decompose(pr9_ts_w)
pr9_ts_decompose_wm=decompose(pr9_ts_w, type="multiplicative")
plot(pr9_ts_decompose_wa)
plot(pr9_ts_decompose_wm)
# constant variance assumptions violated in additive

#monthly
pr9_ts_m=ts(product9$sold_count, frequency=30,start = min(product9$event_date))
pr9_ts_decompose_ma=decompose(pr9_ts_m)
pr9_ts_decompose_mm=decompose(pr9_ts_m, type="multiplicative")
plot(pr9_ts_decompose_ma)
plot(pr9_ts_decompose_mm)
# constant variance assumptions 

#multiplicative weekly decomposition
acf(pr9_ts_decompose_wm$random[complete.cases(pr9_ts_decompose_wm$random)],50)
pacf(pr9_ts_decompose_wm$random[complete.cases(pr9_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag2

#ARIMA
pr9_auto=auto.arima(pr9_ts_decompose_wm$random)
pr9_auto
residuals(pr9_auto)
pr9_fitted = pr9_ts_decompose_wm$random - residuals(pr9_auto)
pr9_fitted_transformed = pr9_fitted*pr9_ts_decompose_wm$seasonal*pr9_ts_decompose_wm$trend

plot(pr9_ts_w, xlab="Date", ylab="Sold Count")
points(pr9_fitted_transformed,type="l", col=2, lty=5)

pr9_predictions=cbind(sold_count=pr9_ts_w,fitted=pr9_fitted_transformed)
pr9_predictions=as.data.table(pr9_predictions)
pr9_predictions$date=product9$event_date

ggplot(pr9_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")+ theme_minimal()+
  labs(title="Product 9 Fitted Values ", x="Date", y="Sold Count")

accu(pr9_predictions$sold_count,pr9_predictions$fitted)

start_date=pr9_predictions[which(is.na(pr9_predictions$fitted)),][4]$date
start_date

pr9_forecast = predict(pr9_auto, n.ahead=4)$pred
pr9_forecast = ts(pr9_forecast, frequency = 7,start = start_date)
pr9_forecast

last_trend=tail(pr9_ts_decompose_wm$trend[!is.na(pr9_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr9_predictions=rbind(pr9_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr9_predictions=cbind(pr9_predictions, seasonality=pr9_ts_decompose_wm$figure)
pr9_predictions=cbind(pr9_predictions, trend=pr9_ts_decompose_wm$trend)

pr9_predictions[date>=start_date, fitted:=pr9_forecast*last_trend]
pr9_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_9=pr9_predictions[date==date_to_forecast, ]$fitted
prediction_for_9
ggplot(data=pr9_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")


###PRODUCT 8

product8= my_data[my_data$product_content_id==73318567,]

#order observations from old to new
product8=(product8[order(product8$event_date),])

#add new data

prd8_add=data[data$product_content_id==73318567,]
prd8_add=prd8_add[event_date>="2021-06-01",]
prd8_add

product8=rbind(product8,prd8_add)

#observations are not normally distributed
hist(product8$sold_count)
qqnorm(product8$sold_count)

#visualizing the data
(ggplot(product8,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 8",y="Sold Count", x="Event Date"))

summary(product8)
str(product8)

#remove 0 sold count period

sum(which(product8$sold_count==0))
product8=product8[event_date>"2021-01-22"]

(ggplot(product8,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 8",y="Sold Count", x="Event Date"))

acf(product8$sold_count,50)

#form a time series object and decompose
#weekly
pr8_ts_w=ts(product8$sold_count, frequency=7,start = min(product8$event_date))
pr8_ts_decompose_wa=decompose(pr8_ts_w)
pr8_ts_decompose_wm=decompose(pr8_ts_w, type="multiplicative")
plot(pr8_ts_decompose_wa)
plot(pr8_ts_decompose_wm)

# constant variance assumption violated 

#monthly
pr8_ts_m=ts(product8$sold_count, frequency=30,start = min(product8$event_date))
pr8_ts_decompose_ma=decompose(pr8_ts_m)
pr8_ts_decompose_mm=decompose(pr8_ts_m, type="multiplicative")
plot(pr8_ts_decompose_ma)
plot(pr8_ts_decompose_mm)
# constant variance and mean assumption violated

#multiplicative weekly decomposition
acf(pr8_ts_decompose_wm$random[complete.cases(pr8_ts_decompose_wm$random)],50)
pacf(pr8_ts_decompose_wm$random[complete.cases(pr8_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag2

#ARIMA
pr8_auto=auto.arima(pr8_ts_decompose_wm$random)
pr8_auto
residuals(pr8_auto)
pr8_fitted = pr8_ts_decompose_wm$random - residuals(pr8_auto)
pr8_fitted_transformed = pr8_fitted*pr8_ts_decompose_wm$seasonal*pr8_ts_decompose_wm$trend

plot(pr8_ts_w, xlab="Date", ylab="Sold Count")
points(pr8_fitted_transformed,type="l", col=2, lty=5)

pr8_predictions=cbind(sold_count=pr8_ts_w,fitted=pr8_fitted_transformed)
pr8_predictions=as.data.table(pr8_predictions)
pr8_predictions$date=product8$event_date

ggplot(pr8_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")+ theme_minimal()+
  labs(title="Product 8 Fitted Values ", x="Date", y="Sold Count")

accu(pr8_predictions$sold_count,pr8_predictions$fitted)

start_date=pr8_predictions[which(is.na(pr8_predictions$fitted)),][22]$date
start_date

pr8_forecast = predict(pr8_auto, n.ahead=4)$pred
pr8_forecast = ts(pr8_forecast, frequency = 7,start = start_date)
pr8_forecast

last_trend=tail(pr8_ts_decompose_wm$trend[!is.na(pr8_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr8_predictions=rbind(pr8_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr8_predictions=cbind(pr8_predictions, seasonality=pr8_ts_decompose_wm$figure)
pr8_predictions=cbind(pr8_predictions, trend=pr8_ts_decompose_wm$trend)


pr8_predictions[date>=start_date, fitted:=pr8_forecast*last_trend]
pr8_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_8=pr8_predictions[date==date_to_forecast, ]$fitted
prediction_for_8

ggplot(data=pr8_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

#use external variables 

#pr8_predictions[,residuals:=sold_count-fitted]
#ggplot(pr8_predictions, aes(x=date, y=residuals))+geom_line()

cors<- cor(product8[,c(3,4,5,6,7,8,9,10,11,12,13)], use="pairwise.complete.obs") 
corrplot.mixed(cors, tl.col="black", tl.pos = "lt")
#basket_count, visit_count, category_sold, #category_favored category_basket favored count

#ggplot(data=pr3_predictions,aes(x=event_date,y=residuals))+geom_line()
ggplot(data=product8,aes(x=event_date,y=basket_count))+geom_line()
ggplot(data=product8,aes(x=event_date,y=visit_count))+geom_line()
ggplot(data=product8,aes(x=event_date,y=category_sold))+geom_line()
ggplot(data=product8,aes(x=event_date,y=category_favored))+geom_line()
ggplot(data=product8,aes(x=event_date,y=category_basket))+geom_line()
ggplot(data=product8,aes(x=event_date,y=favored_count))+geom_line()

#ggplot(data=pr3_predictions,aes(x=event_date,y=category_visits))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=category_brand_sold))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=ty_visits))+geom_line()

# //*
# #normalize
# product8<-product8%>%mutate(
#   N_basket_count=(basket_count-min(basket_count))/(max(basket_count)-min(basket_count)),
#   N_visit_count=(visit_count-min(visit_count))/(max(visit_count)-min(visit_count)),
#   N_category_sold=(category_sold-min(category_sold))/(max(category_sold)-min(category_sold)),
#   N_category_favored=(category_favored-min(category_favored))/(max(category_favored)-min(category_favored)),
#   N_favored_count=(favored_count-min(favored_count))/(max(favored_count)-min(favored_count)),
#   N_category_basket=(category_basket-min(category_basket))/(max(category_basket)-min(category_basket)))
# 
# norm8<-product8%>%pivot_longer(.,cols=c(N_basket_count,N_category_sold,N_category_favored,
#                                                N_visit_count,N_favored_count,N_category_basket),
#                                       names_to="name",
#                                       values_to="values")%>%
#   ggplot(.,aes(x=values,y=factor(name),fill=factor(name)))+
#   geom_density_ridges() +
#   labs(color="Normalized Additional Features",
#        y="Normalized Values")+
#   theme_ipsum()+
#   theme(legend.position = "none")+
#   ggtitle("Distribution of the Normalized Values")
# norm8
# //
  
ccf(product8$basket_count,product8$sold_count) 
ccf(product8$visit_count,product8$sold_count)
ccf(product8$category_sold,product8$sold_count)  
ccf(product8$category_favored,product8$sold_count)
ccf(product8$favored_count,product8$sold_count)
ccf(product8$category_basket,product8$sold_count)

product8=rbind(product8,data.table(event_date=ymd(date_to_forecast)),fill=T)
product8$fitted=pr8_predictions$fitted
#add lagged variables
product8[,lag1_basket_count:=lag(product8$basket_count,1)]
product8[,lag1_visit_count:=lag(product8$visit_count,1)]
product8[,lag1_category_sold:=lag(product8$category_sold,1)]
product8[,lag1_category_favored:=lag(product8$category_favored,1)]
product8[,lag1_favored_count:=lag(product8$favored_count,1)]
product8[,lag1_category_basket:=lag(product8$category_basket,1)]
product8[,lag1_category_favored:=lag(product8$category_favored,1)]


pr8_lm=lm(sold_count~fitted+lag1_basket_count+lag1_visit_count+lag1_category_sold+
            lag1_category_favored+lag1_favored_count+lag1_category_basket+lag1_category_favored,data=product8)
summary(pr8_lm)

#remove non-significant features
pr8_lm1=lm(sold_count~fitted+lag1_basket_count+lag1_category_favored+lag1_favored_count,data=product8)
summary(pr8_lm1)
checkresiduals(pr8_lm1)

product8$fitted
pred = predict(pr8_lm1,data=product8)
product8[event_date>"2021-01-26"&complete.cases(product8$fitted),predictions:=pred]

ggplot(product8 ,aes(x=event_date)) +
  geom_line(aes(y=sold_count)) +
  geom_line(aes(y=predictions,color='red')) + theme_minimal(base_size=14)


ggplot(data=product8[event_date>="2021-05-15"], aes(x=event_date,y=sold_count))+geom_line()+geom_line(aes(y=predictions),col="red")

prediction_for_8
prediction_for_8_new=product8[event_date==date_to_forecast, ]$predictions
prediction_for_8_new

###PRODUCT 5

product5= my_data[my_data$product_content_id==48740784,]

#order observations from old to new
product5=(product5[order(product5$event_date),])

#add new data

prd5_add=data[data$product_content_id==48740784,]
prd5_add=prd5_add[event_date>="2021-06-01",]
prd5_add

product5=rbind(product5,prd5_add)
product5=product5[event_date>"2020-09-28"]


#observations are not normally distributed
hist(product5$sold_count)
qqnorm(product5$sold_count)

#visualizing the data
(ggplot(product5,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 5",y="Sold Count", x="Event Date"))

summary(product5)
str(product5)

acf(product5$sold_count,50)

#form a time series object and decompose
#weekly
pr5_ts_w=ts(product5$sold_count, frequency=7,start = min(product5$event_date))
pr5_ts_decompose_wa=decompose(pr5_ts_w)
pr5_ts_decompose_wm=decompose(pr5_ts_w, type="multiplicative")
plot(pr5_ts_decompose_wa)
plot(pr5_ts_decompose_wm)

# constant variance assumption violated 

#monthly
pr5_ts_m=ts(product5$sold_count, frequency=30,start = min(product5$event_date))
pr5_ts_decompose_ma=decompose(pr5_ts_m)
pr5_ts_decompose_mm=decompose(pr5_ts_m, type="multiplicative")
plot(pr5_ts_decompose_ma)
plot(pr5_ts_decompose_mm)
# constant variance assumption violated

#multiplicative weekly decomposition
acf(pr5_ts_decompose_wm$random[complete.cases(pr5_ts_decompose_wm$random)],50)
pacf(pr5_ts_decompose_wm$random[complete.cases(pr5_ts_decompose_wm$random)],50)
#random term sinusodial acf, pacf has a spike at lag2

#ARIMA
pr5_auto=auto.arima(pr5_ts_decompose_wm$random)
pr5_auto
residuals(pr5_auto)
pr5_fitted = pr5_ts_decompose_wm$random - residuals(pr5_auto)
pr5_fitted_transformed = pr5_fitted*pr5_ts_decompose_wm$seasonal*pr5_ts_decompose_wm$trend

plot(pr5_ts_w, xlab="Date", ylab="Sold Count")
points(pr5_fitted_transformed,type="l", col=2, lty=5)

pr5_predictions=cbind(sold_count=pr5_ts_w,fitted=pr5_fitted_transformed)
pr5_predictions=as.data.table(pr5_predictions)
pr5_predictions$date=product5$event_date

ggplot(pr5_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

accu(pr5_predictions$sold_count,pr5_predictions$fitted)

start_date=pr5_predictions[which(is.na(pr5_predictions$fitted)),][129]$date
start_date

pr5_forecast = predict(pr5_auto, n.ahead=4)$pred
pr5_forecast = ts(pr5_forecast, frequency = 7,start = start_date)
pr5_forecast

last_trend=tail(pr5_ts_decompose_wm$trend[!is.na(pr5_ts_decompose_wm$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr5_predictions=rbind(pr5_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr5_predictions=cbind(pr5_predictions, seasonality=pr5_ts_decompose_wm$figure)
pr5_predictions=cbind(pr5_predictions, trend=pr5_ts_decompose_wm$trend)
start_date
pr5_predictions[date>=start_date, fitted:=pr5_forecast*last_trend]
pr5_predictions[date>=start_date, fitted:=fitted*seasonality]

prediction_for_5=pr5_predictions[date==date_to_forecast, ]$fitted
prediction_for_5

ggplot(data=pr5_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

#use external variables 

cors<- cor(product5[,c(3,4,5,6,7,8,9,10,11,12,13)], use="pairwise.complete.obs") 
corrplot.mixed(cors, tl.col="black", tl.pos = "lt")
#basket_count

ggplot(data=product5,aes(x=event_date,y=basket_count))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=visit_count))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=category_sold))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=category_favored))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=category_basket))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=favored_count))+geom_line()

#ggplot(data=pr3_predictions,aes(x=event_date,y=category_visits))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=category_brand_sold))+geom_line()
#ggplot(data=product8,aes(x=event_date,y=ty_visits))+geom_line()

#normalize
# product5<-product5%>%mutate(
#   N_basket_count=(basket_count-min(basket_count))/(max(basket_count)-min(basket_count)),
#   N_sold_count=(sold_count-min(sold_count))/(max(sold_count)-min(sold_count)))

#product5<-product5%>%mutate(
 # N_basket_count=(basket_count-min(basket_count))/(max(basket_count)-min(basket_count)),
  #N_visit_count=(visit_count-min(visit_count))/(max(visit_count)-min(visit_count)),
  #N_category_sold=(category_sold-min(category_sold))/(max(category_sold)-min(category_sold)),
  #N_category_favored=(category_favored-min(category_favored))/(max(category_favored)-min(category_favored)),
  #N_favored_count=(favored_count-min(favored_count))/(max(favored_count)-min(favored_count)),
  #N_category_basket=(category_basket-min(category_basket))/(max(category_basket)-min(category_basket)))

# norm5<-product5%>%pivot_longer(.,cols=c(N_basket_count,N_category_sold,N_category_favored,
#                                         N_visit_count,N_favored_count,N_category_basket),
#                                names_to="name",
#                                values_to="values")%>%
#   ggplot(.,aes(x=values,y=factor(name),fill=factor(name)))+
#   geom_density_ridges() +
#   labs(color="Normalized Additional Features",
#        y="Normalized Values")+
#   theme_ipsum()+
#   theme(legend.position = "none")+
#   ggtitle("Distribution of the Normalized Values")
# norm5

ccf(product5$basket_count,product5$sold_count) 
# ccf(product8$visit_count,product8$sold_count)
# ccf(product8$category_sold,product8$sold_count)  
# ccf(product8$category_favored,product8$sold_count)
# ccf(product8$favored_count,product8$sold_count)
# ccf(product8$category_basket,product8$sold_count)

product5=rbind(product5,data.table(event_date=ymd(date_to_forecast)),fill=T)
product5=cbind(product5,fitted=pr5_predictions$fitted)
#add lagged variables
product5[,lag1_basket_count:=lag(product5$basket_count,1)]

# product8[,lag1_visit_count:=lag(product8$visit_count,1)]
# product8[,lag1_category_sold:=lag(product8$category_sold,1)]
# product8[,lag1_category_favored:=lag(product8$category_favored,1)]
# product8[,lag1_favored_count:=lag(product8$favored_count,1)]
# product8[,lag1_category_basket:=lag(product8$category_basket,1)]
# product8[,lag1_category_favored:=lag(product8$category_favored,1)]


pr5_lm=lm(sold_count~lag1_basket_count+fitted,data=product5)
summary(pr5_lm)


pred5 = predict(pr5_lm,data=product5)
length(pred5)
product5[event_date>="2020-10-03"&complete.cases(product5$fitted),predictions:=pred5]

ggplot(product5 ,aes(x=event_date)) +
  geom_line(aes(y=sold_count)) +
  geom_line(aes(y=predictions,color='red')) + theme_minimal(base_size=14)+ theme_minimal()+
  labs(title="Product 5 Fitted Values ", x="Date", y="Sold Count")

ggplot(data=product5[event_date>="2021-05-15"], aes(x=event_date,y=sold_count))+geom_line()+geom_line(aes(y=predictions),col="red")

prediction_for_5
prediction_for_5_new=product5[event_date==date_to_forecast, ]$predictions
prediction_for_5_new


###PRODUCT 2

product2= my_data[my_data$product_content_id==32737302,]

#order observations from old to new
product2=(product2[order(product2$event_date),])

#add new data

prd2_add=data[data$product_content_id==32737302,]
prd2_add=prd2_add[event_date>="2021-06-01",]
prd2_add

product2=rbind(product2,prd2_add)
#remove repeated elements
product2=unique(product2)

#observations are not normally distributed
hist(product2$sold_count)
qqnorm(product2$sold_count)

#visualizing the data
(ggplot(product2,aes(x=event_date,y=sold_count))+
    geom_line()+
    theme_minimal()+
    labs(title="Product 2",y="Sold Count", x="Event Date"))

summary(product2)
str(product2)

acf(product2$sold_count,50)

#form a time series object and decompose
#weekly
pr2_ts_w=ts(product2$sold_count, frequency=7,start = min(product2$event_date))
pr2_ts_decompose_wa=decompose(pr2_ts_w)
pr2_ts_decompose_wm=decompose(pr2_ts_w, type="multiplicative")
plot(pr2_ts_decompose_wa)
plot(pr2_ts_decompose_wm)

# constant variance assumption violated 

#monthly
pr2_ts_m=ts(product2$sold_count, frequency=30,start = min(product2$event_date))
pr2_ts_decompose_ma=decompose(pr2_ts_m)
pr2_ts_decompose_mm=decompose(pr2_ts_m, type="multiplicative")
plot(pr2_ts_decompose_ma)
plot(pr2_ts_decompose_mm)
# constant variance assumption violated

#additive weekly decomposition
acf(pr2_ts_decompose_wa$random[complete.cases(pr2_ts_decompose_wa$random)],50)
pacf(pr2_ts_decompose_wa$random[complete.cases(pr2_ts_decompose_wa$random)],50)
#random term sinusodial acf, pacf has a spike at lag2

#ARIMA
pr2_auto=auto.arima(pr2_ts_decompose_wa$random)
pr2_auto
residuals(pr2_auto)
pr2_fitted = pr2_ts_decompose_wa$random - residuals(pr2_auto)
pr2_fitted_transformed = pr2_fitted+pr2_ts_decompose_wa$seasonal+pr2_ts_decompose_wa$trend

plot(pr2_ts_w, xlab="Date", ylab="Sold Count")
points(pr2_fitted_transformed,type="l", col=2, lty=5)

pr2_predictions=cbind(sold_count=pr2_ts_w,fitted=pr2_fitted_transformed)
pr2_predictions=as.data.table(pr2_predictions)
pr2_predictions=cbind(pr2_predictions,date=product2$event_date)

ggplot(pr2_predictions, aes(x=date, y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

accu(pr2_predictions$sold_count,pr2_predictions$fitted)

start_date=pr2_predictions[which(is.na(pr2_predictions$fitted)),][4]$date
start_date

pr2_forecast = predict(pr2_auto, n.ahead=4)$pred
pr2_forecast = ts(pr2_forecast, frequency = 7,start = start_date)
pr2_forecast

last_trend=tail(pr2_ts_decompose_wa$trend[!is.na(pr2_ts_decompose_wa$trend)],1)
last_trend

###!!!!###
#add new day to forecast
pr2_predictions=rbind(pr2_predictions,data.table(date=ymd(date_to_forecast)),fill=T)

pr2_predictions=cbind(pr2_predictions, seasonality=pr2_ts_decompose_wa$figure)
pr2_predictions=cbind(pr2_predictions, trend=pr2_ts_decompose_wa$trend)
start_date
pr2_predictions[date>=start_date, fitted:=pr2_forecast+last_trend]
pr2_predictions[date>=start_date, fitted:=fitted+seasonality]

prediction_for_2=pr2_predictions[date==date_to_forecast, ]$fitted
prediction_for_2

ggplot(data=pr2_predictions[date>="2021-05-15"], aes(x=date,y=sold_count))+geom_line()+geom_line(aes(y=fitted),col="red")

#use external variables 

cors<- cor(product2[,c(3,4,5,6,7,8,9,10,11,12,13)], use="pairwise.complete.obs") 
corrplot.mixed(cors, tl.col="black", tl.pos = "lt")
#basket_count, visit_count, category_sold, ty_visits, favored_count, category_basket,
#category_favored #category_brand_sold

ggplot(data=product2,aes(x=event_date,y=basket_count))+geom_line()
ggplot(data=product2,aes(x=event_date,y=visit_count))+geom_line()
ggplot(data=product2,aes(x=event_date,y=category_sold))+geom_line()
ggplot(data=product2,aes(x=event_date,y=category_favored))+geom_line()
ggplot(data=product2,aes(x=event_date,y=category_basket))+geom_line()
ggplot(data=product2,aes(x=event_date,y=favored_count))+geom_line()
#ggplot(data=pr3_predictions,aes(x=event_date,y=category_visits))+geom_line()
ggplot(data=product2,aes(x=event_date,y=category_brand_sold))+geom_line()
ggplot(data=product2,aes(x=event_date,y=ty_visits))+geom_line()


ccf(product2$basket_count,product2$sold_count) 
ccf(product2$visit_count,product2$sold_count)
ccf(product2$category_sold,product2$sold_count)  
ccf(product2$category_favored,product2$sold_count)
ccf(product2$favored_count,product2$sold_count)
ccf(product2$category_basket,product2$sold_count)
ccf(product2$category_brand_sold,product2$sold_count)
ccf(product2$ty_visits,product2$sold_count)


product2=rbind(product2,data.table(event_date=ymd(date_to_forecast)),fill=T)
product2=cbind(product2,fitted=pr2_predictions$fitted)
#add lagged variables
product2[,lag1_basket_count:=lag(product2$basket_count,1)]
product2[,lag1_visit_count:=lag(product2$visit_count,1)]
product2[,lag1_category_sold:=lag(product2$category_sold,1)]
product2[,lag1_category_favored:=lag(product2$category_favored,1)]
product2[,lag1_favored_count:=lag(product2$favored_count,1)]
product2[,lag1_category_basket:=lag(product2$category_basket,1)]
product2[,lag1_category_brand_sold:=lag(product2$category_brand_sold,1)]
product2[,lag1_ty_visits:=lag(product2$ty_visits,1)]


pr2_lm=lm(sold_count~fitted+lag1_basket_count+lag1_visit_count+lag1_category_sold
          +lag1_category_favored+lag1_favored_count+lag1_category_basket+lag1_category_brand_sold+lag1_ty_visits,data=product2)
summary(pr2_lm)

#remove non significant features

pr2_lm1=lm(sold_count~fitted+lag1_visit_count+lag1_category_sold
          +lag1_category_favored+lag1_favored_count+lag1_category_basket,data=product2)
summary(pr2_lm1)

pr2_lm2=lm(sold_count~fitted+lag1_visit_count+lag1_category_sold
           +lag1_category_favored+lag1_category_basket,data=product2)
summary(pr2_lm2)

pred2 = predict(pr2_lm2,data=product2)
length(pred2)
product2[event_date>"2020-05-28"&complete.cases(product2$fitted),predictions:=pred2]

ggplot(product2 ,aes(x=event_date)) +
  geom_line(aes(y=sold_count)) +
  geom_line(aes(y=predictions),color='red') + theme_minimal()+ theme_minimal()+
  labs(title="Product 2 Fitted Values ", x="Date", y="Sold Count")


ggplot(data=product2[event_date>="2021-05-15"], aes(x=event_date,y=sold_count))+geom_line()+geom_line(aes(y=predictions),col="red")

prediction_for_2
prediction_for_2_new=product2[event_date==date_to_forecast, ]$predictions
prediction_for_2_new

