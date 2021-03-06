library(ggplot2)
set.seed(4238)
abt = read.csv("C:/Users/futeck/Desktop/ST4238 project/20 Companies/Healthcare/ABT.csv", header = TRUE)
abt = abt[c(27:177),]
S_0 = abt$Close[nrow(abt)-1]
#We need to calculate annualized volatility for the sigma parameter

#Given that our test data is over a period of 6 months (0.5 years), our tau = 0.5
#We first find u_i, our log of daily return on stock
abt['u'] <- 0
for (i in 2:150){
  abt$u[i] = log(abt$Close[i]/abt$Close[i-1]) 
}
u_bar = sum(abt$u)/149

v = numeric(149)
for (i in 2:150){
  v[i-1] = abt$u[i] - u_bar
}
v = sqrt(sum(v^2)/148)
sigma_star = v/sqrt(0.5)

drift = u_bar/0.5 + 0.5*(sigma_star)^2

#With the sigma_star and drift values, we can then calculate again the corresponding CI.

library(sde)
new_final_stock_price = numeric(100)
S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=1,N=20)
new_final_stock_price[1] = S_t[length(S_t)]
plot(seq(0,20,by=1),S_t,type="l", main = "Geometric Brownian Motion Simulation of Abbott Laboratories' Stock Price with Updated Parameters")
for (i in 2:100){
  S_t <- GBM(S_0,r=drift,sigma=sigma_star,T=1,N=20)
  lines(seq(0,20,by=1),S_t,type="l")
  new_final_stock_price[i] = S_t[length(S_t)]
}
hist(new_final_stock_price, main = "1-day GBM Simulation Histogram of Abbott Laboratories' Stock Price with drift")

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
drift = format(round(drift,4),nsmall=4)
drift
sigma_star = format(round(sigma_star,4),nsmall=4)
sigma_star
cat(paste0("(",new_lower_fsp_output,",",new_upper_fsp_output,")"))