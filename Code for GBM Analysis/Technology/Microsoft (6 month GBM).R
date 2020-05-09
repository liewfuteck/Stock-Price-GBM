library(ggplot2)
set.seed(4238)
msft = read.csv("C:/Users/futeck/Desktop/ST4238 project/20 Companies/Technology/MSFT.csv", header = TRUE)
msft = msft[c(36:183),]
par(mfrow=c(2,2))
msft["Time Interval"] = c(1:nrow(msft))
par(mfrow=c(2,2))
ggplot(data=msft,aes(x=msft$"Time Interval",y=msft$Close))+
  ggtitle("Microsoft Stock Price Trend")+
  geom_line() +xlab("Time Interval") + ylab("Closing Price")
mu_model = mean(msft$Close[1:127])
S_0 = msft$Close[127]
#We need to calculate annualized volatility for the sigma parameter
msft['Daily Change'] <- 0
for (i in 2:nrow(msft)){
  msft$'Daily Change'[i-1] = (msft$Close[i] - msft$Close[i-1])/msft$Close[i-1]
}
sd_model = sd(msft$'Daily Change'[1:127])
mu_model_output = format(round(mu_model,2), nsmall = 2)
sd_model_output = format(round(sd_model,4), nsmall = 4)


#To carry out simulation under the GBM model
library(sde)
final_stock_price = numeric(100)
S_t <- GBM(S_0,r=0,sigma=sd_model,T=30,N=1000)
final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,1000,by=1),S_t,type="l", main = "Geometric Brownian Motion Simulation of Microsoft Stock Price")
for (i in 2:100){
  S_t <- GBM(S_0,r=0,sigma=sd_model,T=30,N=1000)
  lines(seq(0,1000,by=1),S_t,type="l")
  final_stock_price[i] = S_t[length(S_t)]
}
hist(final_stock_price, main = "GBM Histogram of Microsoft Stock Price with no drift")

#From the histogram, we see that it follows a lognormal distribution. To find the corresponding 95% CI, we first take the log of the final stock price.

log_fsp = log(final_stock_price)
avg_log_fsp = mean(log_fsp)
sd_log_fsp = sd(log_fsp)/sqrt(length(log_fsp))
lower_log_fsp = mean(log_fsp) - qnorm(0.975)*sd_log_fsp
upper_log_fsp = mean(log_fsp) + qnorm(0.975)*sd_log_fsp
lower_fsp = exp(lower_log_fsp)
upper_fsp = exp(upper_log_fsp)
lower_fsp_output = format(round(lower_fsp,2), nsmall = 2)
upper_fsp_output = format(round(upper_fsp,2), nsmall = 2)

#We see that this is not a very good estimate, from actual $157.70. Hence, we can try to input the values of our drift rate and sigma and retry the GBM model simulation.


#Daily change should be normally distributed.
msft['Daily Stock Return'] <- 0
msft$'Daily Stock Return' = (msft$Close - msft$Open)/msft$Open 
hist(msft$'Daily Stock Return'*100, freq =FALSE, main = "Histogram of Microsoft's Daily Return", xlab = "Daily Percentage Change")
xpt <- seq(-4,4,0.1)
ypt <- dnorm(xpt,mean(msft$'Daily Stock Return')*100, sd(msft$'Daily Stock Return')*100)
lines(xpt, ypt)
qqnorm(msft$'Daily Stock Return')
qqline(msft$'Daily Stock Return')


#Given that our test data is over a period of 6 months (0.5 years), our tau = 0.5
#We first find u_i, our log of daily return on stock
msft['u'] <- 0
for (i in 2:127){
  msft$u[i] = log(msft$Close[i]/msft$Close[i-1]) 
}
u_bar = sum(msft$u)/126

v = numeric(126)
for (i in 2:127){
  v[i-1] = msft$u[i] - u_bar
}
v = sqrt(sum(v^2)/125)
sigma_star = v/sqrt(0.5)

drift = u_bar/0.5 + 0.5*(sigma_star)^2

sigma_star_output = format(round(sigma_star,4),nsmall=4)
drift_output = format(round(drift,5),nsmall=5)

#With the sigma_star and drift values, we can then calculate again the corresponding CI.

library(sde)
new_final_stock_price = numeric(100)
S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=30,N=1000)
new_final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,1000,by=1),S_t,type="l", main = "Geometric Brownian Motion Simulation of Microsoft Stock Price with Updated Parameters")
for (i in 2:100){
  S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=30,N=1000)
  lines(seq(0,1000,by=1),S_t,type="l")
  new_final_stock_price[i] = S_t[length(S_t)]
}
hist(new_final_stock_price, main = "GBM Histogram of Microsoft Stock Price with drift")

#Again, this is lognormal so we can normalize it to find the corresponding CI.

new_log_fsp = log(new_final_stock_price)
new_avg_log_fsp = mean(new_log_fsp)
new_sd_log_fsp = sd(new_log_fsp)/sqrt(length(new_log_fsp))
new_lower_log_fsp = mean(new_log_fsp) - qnorm(0.975)*new_sd_log_fsp
new_upper_log_fsp = mean(new_log_fsp) + qnorm(0.975)*new_sd_log_fsp
new_lower_fsp = exp(new_lower_log_fsp)
new_upper_fsp = exp(new_upper_log_fsp)
new_lower_fsp_output = format(round(new_lower_fsp,2),nsmall = 2)
new_upper_fsp_output = format(round(new_upper_fsp,2),nsmall = 2)


