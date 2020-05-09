library(ggplot2)
set.seed(4238)
bac = read.csv("C:/Users/futeck/Desktop/ST4238 project/20 Companies/Finance/BAC.csv", header = TRUE)
bac = bac[c(40:187),]
bac["Time Interval"] = c(1:nrow(bac))
par(mfrow=c(2,2))
ggplot(data=bac,aes(x=bac$"Time Interval",y=bac$Close))+
  ggtitle("Bank of America's Stock Price Trend")+
  geom_line() +xlab("Time Interval") + ylab("Closing Price")
mu_model = mean(bac$Close[1:127])
S_0 = bac$Close[127]
#We need to calculate annualized volatility for the sigma parameter
bac['Daily Change'] <- 0
for (i in 2:nrow(bac)){
  bac$'Daily Change'[i-1] = (bac$Close[i] - bac$Close[i-1])/bac$Close[i-1]
}
sd_model = sd(bac$'Daily Change'[1:127])


#To carry out simulation under the GBM model
library(sde)
final_stock_price = numeric(100)
S_t <- GBM(S_0,r=0,sigma=sd_model,T=30,N=1000)
final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,1000,by=1),S_t,type="l", main = "Geometric Brownian Motion Simulation of Bank of America's Stock Price")
for (i in 2:100){
  S_t <- GBM(S_0,r=0,sigma=sd_model,T=30,N=1000)
  lines(seq(0,1000,by=1),S_t,type="l")
  final_stock_price[i] = S_t[length(S_t)]
}
hist(final_stock_price, main = "GBM Histogram of Bank of America Stock Price with no drift")

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
bac['Daily Stock Return'] <- 0
bac$'Daily Stock Return' = (bac$Close - bac$Open)/bac$Open 
hist(bac$'Daily Stock Return'*100, freq =FALSE, main = "Histogram of Bank of America's Daily Return", xlab = "Daily Percentage Change")
xpt <- seq(-3,3,by=0.01)
ypt <- dnorm(xpt, mean(bac$'Daily Stock Return')*100, sd(bac$'Daily Stock Return')*100)
lines(xpt, ypt)
qqnorm(bac$'Daily Stock Return')
qqline(bac$'Daily Stock Return')


#Given that our test data is over a period of 6 months (0.5 years), our tau = 0.5
#We first find u_i, our log of daily return on stock
bac['u'] <- 0
for (i in 2:127){
  bac$u[i] = log(bac$Close[i]/bac$Close[i-1]) 
}
u_bar = sum(bac$u)/126

v = numeric(126)
for (i in 2:127){
  v[i-1] = bac$u[i] - u_bar
}
v = sqrt(sum(v^2)/125)
sigma_star = v/sqrt(0.5)

drift = u_bar/0.5 + 0.5*(sigma_star)^2

#With the sigma_star and drift values, we can then calculate again the corresponding CI.

library(sde)
new_final_stock_price = numeric(100)
S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=30,N=1000)
new_final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,1000,by=1),S_t,type="l", main = "Geometric Brownian Motion Simulation of Bank of America's Stock Price with Updated Parameters")
for (i in 2:100){
  S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=30,N=1000)
  lines(seq(0,1000,by=1),S_t,type="l")
  new_final_stock_price[i] = S_t[length(S_t)]
}
hist(new_final_stock_price, main = "GBM Histogram of Bank of America Stock Price with drift")

#Again, this is lognormal so we can normalize it to find the corresponding CI.

new_log_fsp = log(new_final_stock_price)
new_avg_log_fsp = mean(new_log_fsp)
new_sd_log_fsp = sd(new_log_fsp)/sqrt(length(new_log_fsp))
new_lower_log_fsp = mean(new_log_fsp) - qnorm(0.975)*new_sd_log_fsp
new_upper_log_fsp = mean(new_log_fsp) + qnorm(0.975)*new_sd_log_fsp
new_lower_fsp = exp(new_lower_log_fsp)
new_upper_fsp = exp(new_upper_log_fsp)