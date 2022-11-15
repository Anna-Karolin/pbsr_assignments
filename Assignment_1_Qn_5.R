library(quantmod)
getSymbols('TCS.NS')
tail(TCS.NS)
plot(TCS.NS$TCS.NS.Adjusted)
getSymbols('^NSEI')
tail(NSEI)
plot(NSEI$NSEI.Adjusted)
TCS_rt = diff(log(TCS.NS$TCS.NS.Adjusted))
Nifty_rt = diff(log(NSEI$NSEI.Adjusted))
retrn = cbind.xts(TCS_rt,Nifty_rt)
retrn = na.omit(data.frame(retrn))
plot(retrn$NSEI.Adjusted,retrn$TCS.NS.Adjusted
     ,pch=20
     ,xlab='Market Return'
     ,ylab='TCS Return'
     ,xlim=c(-0.18,0.18)
     ,ylim=c(-0.18,0.18))
grid(col='grey',lty=1)

library("dplyr")
set.seed(1225)
data = merge(TCS_rt,Nifty_rt)
data = na.omit(data)
data <- as.data.frame(data) 
n = nrow(data)
data_s1 = sample_n(data,n,replace = T)
#data_s2 = sample_n(data$NSEI.Adjusted,n,replace = T)
mu_TCS <- mean(data_s1$TCS.NS.Adjusted)
mu_NSEI <- mean(data_s1$NSEI.Adjusted)
sigma_TCS <- sd(data_s1$TCS.NS.Adjusted)
sigma_NSEI <- sd(data_s1$NSEI.Adjusted)
r <- cor(data_s1$TCS.NS.Adjusted, data_s1$NSEI.Adjusted)

# Method of Moments

sigma_hat1 = sigma_NSEI
alpha_hat1 = mu_NSEI - r*(sigma_NSEI/sigma_TCS)*mu_TCS
beta_hat1 = r*(sigma_NSEI/sigma_TCS)

#OLS Method

data3 <- cbind(data_s1, NSEI.Adjusted = data_s1$NSEI.Adjusted )
simple.fit = lm(TCS.NS.Adjusted ~ NSEI.Adjusted, data=data3)
summary(simple.fit)
coef(simple.fit)
errors = residuals(simple.fit)
sigma_hat2 = sqrt((1/length(errors))*sum(errors^2))


Parameters <- c('\u03b1', '\u03b2', '\u03c3')
Method_of_Moments <- c(alpha_hat1, beta_hat1, sigma_hat1)
OLS <- c(coef(simple.fit)[1], coef(simple.fit)[2], sigma_hat2)
df_1 <- data.frame(Parameters, Method_of_Moments, OLS)

x <- exp(log(3200) + coef(simple.fit)[1] + coef(simple.fit)[2]*(log(18200) - log(18000)))
y <- exp(log(3200) + alpha_hat1 + beta_hat1*(log(18200) - log(18000)))

df_1

print(paste("The expect TCS price (by Method of Moments Method) = ", y))
print(paste("The expect TCS price (by OLS Method) = ", x))
