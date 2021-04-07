install.packages("Metrics")
library(Metrics)

nike<-read.csv(file.choose(),header=T)
View(nike)

plot(nike$Revenue, type="l")

q<-c("Q1","Q2","Q3","Q4");q
rep(q,length=69)
outer(rep(q,length=69),q,"==")
data.frame(outer(rep(q,length=69),q,"==")+0)

X<-data.frame(outer(rep(q,length=69),q,"==")+0)
colnames(X)<-q
View(X)


Niketsf<-cbind(nike,X)
View(Niketsf)

Niketsf["t"]<-1:69
Niketsf["log_revenue"]<-log(Niketsf["Revenue"])
Niketsf["t_squared"]<-Niketsf["t"]*Niketsf["t"]
View(Niketsf)

write.csv(x=Niketsf,file="for_test_data.csv")

train<-Niketsf[1:55,]
test<-Niketsf[56:69,]
    


### LINEAR MODEL ###

linear_model<-lm(Revenue~t,data=train)
summary(linear_model)
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-rmse(test$Revenue,linear_pred$fit)
rmse_linear

##### [RMSE = 987.2625] ###


### EXPONENTIAL ###


expo_model<-lm(log_revenue~t,data=train)
summary(expo_model)
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
View(expo_pred)
rmse_expo<-rmse(test$Revenue,exp(expo_pred$fit))
rmse_expo

###### RMSE = 418.3475 #####


#### quadratic ###


Quad_model<-lm(Revenue~t+t_squared,data=train)
summary(Quad_model)
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-rmse(test$Revenue,Quad_pred$fit)
rmse_Quad

##### RMSE = 556.656 ####


### ADDITIVE SEASONALITY ###

sea_add_model<-lm(Revenue~Q1+Q2+Q3+Q4,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-rmse(test$Revenue,sea_add_pred$fit)
rmse_sea_add

##### RMSE=3807.048 ####


#### Additive seasonality with linear ###

Add_sea_Linear_model<-lm(Revenue~t+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Linear_model)
Add_sea_Linear_pred<-data.frame(predict(Add_sea_Linear_model,interval='predict',newdata=test))
rmse_Add_sea_Linear<-rmse(test$Revenue,Add_sea_Linear_pred$fit)
rmse_Add_sea_Linear

#### RMSE=989.7058 ####


#### Additive Seasonality with quadratic ###

Add_sea_Quad_model<-lm(Revenue~t+t_squared+Q1+Q2+Q3+Q4,data=train)
summary(Add_sea_Quad_model)
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-rmse(test$Revenue,Add_sea_Quad_pred$fit)
rmse_Add_sea_Quad

### RMSE=551.1671 ####



### Multiplicative seasonality ###

multi_sea_model<-lm(log_revenue~Q1+Q2+Q3+Q4,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-rmse(test$Revenue,exp(multi_sea_pred$fit))
rmse_multi_sea

### RMSE=4016.995 ####



### Multiplicative seasonality Linear Trend ###


multi_add_sea_model<-lm(log_revenue~t+Q1+Q2+Q3+Q4,data = train)
summary(multi_add_sea_model)
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_add_sea<-rmse(test$Revenue,exp(multi_add_sea_pred$fit))
rmse_multi_add_sea 

##### RMSE= 253.2826 ####



multi_sea_Quad_model<-lm(log_revenue~t+t_squared+Q1+Q2+Q3+Q4,data=train)
summary(multi_sea_Quad_model)
multi_sea_Quad_pred<-data.frame(predict(multi_sea_Quad_model,interval='predict',newdata=test))
rmse_multi_sea_Quad<-rmse(test$Revenue,multi_sea_Quad_pred$fit)
rmse_multi_sea_Quad

####### RMSE=7837.428 ###

table_rmse<-data.frame('Model'=c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_multi_sea","rmse_multi_add_sea"),'RMSE'=c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_multi_sea,rmse_multi_add_sea))

colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)


### LEAST RMSE IS OBTAINED FOR MULTIPLICATIVE SEASONALITY WITH LINEAR TREND MODEL ####

new_model <- lm(log_revenue~t+Q1+Q2+Q3+Q4,data=Niketsf)
resid <- residuals(new_model)
acf(resid,lag.max = 10)
pacf(resid, lag.max = 10)



#ARIMA

k <- arima(resid, order=c(1,0,0))
pred_res<- predict(arima(resid,order=c(1,0,0)),n.ahead = 14)
str(pred_res)
pred_res$pred
acf(k$residuals)
pacf(k$residuals)


L<-arima(resid, order=c(1,0,1))
L
pred_res1<-predict(arima(resid,order=c(1,0,1)),n.ahead=14)
pred_res1
str(pred_res1)
pred_res1$pred
acf(L$residuals)
pacf(L$residuals)


##### MOVING AVERAGE #####
M<- arima(resid, order=c(0,0,1))
M
pred_resd<-predict(arima(resid, order=c(0,0,1)),n.ahead=14)
pred_resd
str(pred_resd)
pred_resd$pred
plot(acf(M$residuals))
plot(pacf(M$residuals))


N<- arima(resid, order=c(0,0,2))
N
pred_resd1<-predict(arima(resid, order=c(0,0,2)),n.ahead=14)
pred_resd1
str(pred_resd1)
pred_resd1$pred
plot(acf(N$residuals))
plot(pacf(N$residuals))



test_data<-read.csv(file.choose(),header=T)
View(test_data)

pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)
pred_re<-pred_res$pred[1:14]
pred_new$fit <- pred_new$fit+pred_res$pred
View(pred_new)
View(pred_new$fit)


install.packages(c("forecast","fpp","smooth","tseries"))
library(forecast)
library(fpp)
library(smooth)
library(tseries)
windows()
plot(forecast(pred_new$fit,h=4),xaxt="n")
