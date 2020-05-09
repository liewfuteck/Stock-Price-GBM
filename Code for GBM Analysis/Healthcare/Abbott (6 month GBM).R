library(ggplot2)
set.seed(4238)
abt = read.csv("C:/Users/futeck/Desktop/ST4238 project/20 Companies/Healthcare/ABT.csv", header = TRUE)
abt = abt[c(30:177),]
par(mfrow=c(2,2))
abt["Time Interval"] = c(1:nrow(abt))
ggplot(data=abt,aes(x=abt$"Time Interval",y=abt$Close))+
  ggtitle("Abbott Stock Price Trend")+
  geom_line() +xlab("Time Interval") + ylab("Closing Price")
mu_model = mean(abt$Close[1:127])
S_0 = abt$Close[127]
#We need to calculate annualized volatility for the sigma parameter
abt['Daily Change'] <- 0
for (i in 2:nrow(abt)){
  abt$'Daily Change'[i-1] = (abt$Close[i] - abt$Close[i-1])/abt$Close[i-1]
}
sd_model = sd(abt$'Daily Change'[1:127])


#To carry out simulation under the GBM model
library(sde)
final_stock_price = numeric(100)
S_t <- GBM(S_0,r=0,sigma=sd_model,T=30,N=1000)
final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,1000,by=1),S_t,type="l", main = "Geometric Brownian Motion Simulation of Abbott Laboratories' Stock Price")
for (i in 2:100){
  S_t <- GBM(S_0,r=0,sigma=sd_model,T=30,N=1000)
  lines(seq(0,1000,by=1),S_t,type="l")
  final_stock_price[i] = S_t[length(S_t)]
}
hist(final_stock_price, main="GBM Histogram of Abbott Stock Price without drift")

#From the histogram, we see that it follows a lognormal distribution. To find the corresponding 95% CI, we first take the log of the final stock price.

log_fsp = log(final_stock_price)
avg_log_fsp = mean(log_fsp)
sd_log_fsp = sd(log_fsp)/sqrt(length(log_fsp))
lower_log_fsp = mean(log_fsp) - qnorm(0.975)*sd_log_fsp
upper_log_fsp = mean(log_fsp) + qnorm(0.975)*sd_log_fsp
lower_fsp = exp(lower_log_fsp)
upper_fsp = exp(upper_log_fsp)

#We see that this is not a very good estimate, from actual $157.70. Hence, we can try to input the values of our drift rate and sigma and retry the GBM model simulation.


#Daily change should be normally distributed.
abt['Daily Stock Return'] <- 0
abt$'Daily Stock Return' = (abt$Close - abt$Open)/abt$Open 
hist(abt$'Daily Stock Return'*100, freq =FALSE, main = "Histogram of Abbott Laboratories' Daily Return", xlab = "Daily Percentage Change")
xpt <- seq(-4,4,by=0.01)
ypt <- dnorm(xpt, mean(amzn$'Daily Stock Return')*100, sd(amzn$'Daily Stock Return')*100)
lines(xpt, ypt)
qqnorm(abt$'Daily Stock Return')
qqline(abt$'Daily Stock Return')


#Given that our test data is over a period of 6 months (0.5 years), our tau = 0.5
#We first find u_i, our log of daily return on stock
abt['u'] <- 0
for (i in 2:127){
  abt$u[i] = log(abt$Close[i]/abt$Close[i-1]) 
}
u_bar = sum(abt$u)/126

v = numeric(126)
for (i in 2:127){
  v[i-1] = abt$u[i] - u_bar
}
v = sqrt(sum(v^2)/125)
sigma_star = v/sqrt(0.5)

drift = u_bar/0.5 + 0.5*(sigma_star)^2

#With the sigma_star and drift values, we can then calculate again the corresponding CI.

library(sde)
new_final_stock_price = numeric(100)
S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=30,N=1000)
new_final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,1000,by=1),S_t,type="l", main = "Geometric Brownian Motion Simulation of Abbott Laboratories' Stock Price with Updated Parameters")
for (i in 2:100){
  S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=30,N=1000)
  lines(seq(0,1000,by=1),S_t,type="l")
  new_final_stock_price[i] = S_t[length(S_t)]
}
hist(new_final_stock_price, main = "GBM Histogram of Stock Price with drift")

#Again, this is lognormal so we can normalize it to find the corresponding CI.

new_log_fsp = log(new_final_stock_price)
new_avg_log_fsp = mean(new_log_fsp)
new_sd_log_fsp = sd(new_log_fsp)/sqrt(length(new_log_fsp))
new_lower_log_fsp = mean(new_log_fsp) - qnorm(0.975)*new_sd_log_fsp
new_upper_log_fsp = mean(new_log_fsp) + qnorm(0.975)*new_sd_log_fsp
new_lower_fsp = exp(new_lower_log_fsp)
new_upper_fsp = exp(new_upper_log_fsp)