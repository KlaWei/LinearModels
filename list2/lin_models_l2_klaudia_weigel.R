
# Exercise 1
space_palette = c("#005b6e", "#04668c", "#3c6ca7", "#726eb7", "#a86bba", "#da66ac", "#ff6792")
library(ggplot2)
df = read.table("CH01PR20.txt", col.names = c("hours", "copiers"))
df = data.frame(df)
ggplot(data = df, aes(x=copiers, y=hours, color = hours)) +
  geom_point() + 
  scale_color_gradientn(colors = space_palette)

### Exercise 2

alpha = 0.05
reg = lm(hours~copiers, data = df)
reg$coefficients # beta_0 and beta_1
sigma(reg) # sigma
confint(reg)[2,]

Y = df[,1]; X = df[,2]; n = length(Y)
beta_1 = sum((X-mean(X)) %*% (Y - mean(Y)))/sum((X-mean(X))^2)
beta_0 = mean(Y) - beta_1*mean(X)
sigma_squared = (1/(n-2))*sum((Y - beta_0 - beta_1*X)^2)
beta_0; beta_1; sqrt(sigma_squared)

# confidence interval for the slope = beta_1
s_2_beta_1 = sigma_squared/sum((X-mean(X))^2)
conf_beta_1 = beta_1 + c(-qt(1-alpha/2, df = n-2), qt(1-alpha/2, df = n-2))*sqrt(s_2_beta_1)

# the T statistic
T = beta_1/sqrt(sigma_squared/sum((X - mean(X))^2))

# p value
2*(1 - pt(abs(T), df=n-2))

# confidence interval for beta_0
s_2_beta_0 = sigma_squared*(1/n + mean(X^2)/sum((X-mean(X))^2))
conf_beta_0 = beta_0 + c(-qt(1-alpha/2, df = n-2), qt(1-alpha/2, df = n-2))*sqrt(s_2)


### Exercise 3 

predict(reg, data.frame(copiers = c(11)), interval='confidence')
mu_11_pred = beta_0 + 11*beta_1
X_h = 11
s_2_mu_11 = sigma_squared*(1/n + (X_h -mean(X))^2/sum((X-mean(X))^2))
quantile = qt(1-alpha/2, df = n-2)
conf_mu_11 = mu_11_pred + c(-quantile, quantile)*sqrt(s_2_mu_11)

### Exercise 4
predict(reg, data.frame(copiers = c(11)), interval='prediction')
s_2_pred = sigma_squared*(1 + 1/n + (X_h -mean(X))^2/sum((X-mean(X))^2))
conf_pred = mu_11_pred + c(-quantile, quantile)*sqrt(s_2_pred)


### Exercise 5

m2 = X*beta_1 + beta_0 # = predict(reg)
s2 = sqrt(sigma_squared*(1/n + (X - mean(X))^2/sum((X - mean(X))^2))) # predict(reg, se.fit = TRUE)$se.fit
w = sqrt(2*qf(1-alpha,2,n-2))
up = m2 + w*s2
down = m2 - w*s2

dat = data.frame(X, Y, m2, up, down)
pred = predict(reg, interval = "prediction")
conf = predict(reg, interval = "confidence")
dat = cbind(dat, pred[,2:3], conf[,2:3])
colnames(dat) = c("X", "Y", "fit", "up", "down", "lwr_pred", "upr_pred", "lwr_conf", "upr_conf")

# colors = c(palette("ggplot2"))
# ggplot(data = dat, aes(x = X, y = Y)) +
#   geom_point(color = colors[2]) +
#   geom_line(aes(y=up), color = colors[5], size = 1.2) +
#   geom_line(aes(y=down), color = colors[5], size = 1.2) +
#   geom_line(aes(y=lwr_pred), color = colors[4], size = 0.6) +
#   geom_line(aes(y=upr_pred), color = colors[4], size = 0.6) +
#   geom_line(aes(y=lwr_conf), color = colors[6], size = 0.6) +
#   geom_line(aes(y=upr_conf), color = colors[6],  size = 0.6) +
#   geom_line(aes(y = fit), color = colors[8])


colors = c(palette("Set1"))
ggplot(data = dat, aes(x = X, y = Y)) +
  geom_point(aes(color = "points")) +
  geom_line(aes(y=up, color = "95% confidence band"), size = 0.9) +
  geom_line(aes(y=down, color = "95% confidence band"), size = 0.9) +
  geom_line(aes(y=lwr_pred, color = "95% prediction interval"), size = 0.6) +
  geom_line(aes(y=upr_pred, color = "95% prediction interval"), size = 0.6) +
  geom_line(aes(y=lwr_conf, color = "95% confidence interval"), size = 0.6) +
  geom_line(aes(y=upr_conf, color = "95% confidence interval"),  size = 0.6) +
  geom_line(aes(y = fit, color = "regression line")) +
  scale_color_manual(values = c("points" = colors[2], "95% confidence band" = colors[5], 
                                "95% prediction interval" = colors[4], 
                                "95% confidence interval" = colors[3], 
                                "regression line" = colors[8])) +
  theme(legend.position= c(0.83, 0.23))

### Exercise 6
# a)
n = 20; sig2 = 120; ssx = 1000
sig2b1<-sig2/ssx
df=n-2
tc<-qt(1-alpha/2,df)
beta1 = 1
delta = beta1/sqrt(sig2b1)
prob1 = function(delta){pt(tc,df,delta)}
prob2 = function(delta){pt(-tc,df,delta)}
power = 1-prob1(delta)+prob2(delta)
power

# b)

beta1 = seq(from=-2.0, to= 2.0, by= .05)
delta = beta1/sqrt(sig2b1)
power = 1-prob1(delta)+prob2(delta)
ggplot(data = data.frame(beta1, power), aes(x=beta1, y=power)) +
  geom_line(color = space_palette[3]) +
  geom_hline(yintercept = 0.05, color = space_palette[6])


### Exercise 7

library(mvtnorm)
X = rmvnorm(1, mean = rep(0, 200), sigma = 1/200 * diag(200)); num_vectors = 1000

make_y = function(x, beta) {
  epsilon = rmvnorm(1, mean = rep(0, 200), sigma = diag(200))
  Y = 5 + beta*X + epsilon
  Y
}

make_y_b = function(x, beta) {
  epsilon = rexp(200, 1)
  Y = 5 + beta*X + epsilon
  Y
}
test = function(Y, X) {
  alpha = 0.05; n = length(X)
  s_squared = sum((Y - mean(Y))^2)/(n-2)
  beta_1_est = sum((X-mean(X)) %*% t((Y - mean(Y))))/sum((X-mean(X))^2)
  t = beta_1_est/sqrt((s_squared/sum((X-mean(X))^2))) # test statistic
  # calculate p-value, return 1 if we reject the null hypothesis
  return(2*(1 - pt(abs(t), df = n-2)) < alpha)
}

# a)
Y_a = lapply(1:num_vectors, make_y, beta = 0)
test_a = lapply(Y_a, test, X)
sum(unlist(test_a))/length(test_a)

# b)
Y_b = lapply(1:num_vectors, make_y_b, beta = 0)
test_b = lapply(Y_b, test, X)
sum(unlist(test_b))/length(test_b)

power = function(beta1) {
  n = 1000; sig2 = 1; ssx = sum((X-mean(X))^2); sig2b1<-sig2/ssx; df=n-2
  tc = qt(1-alpha/2,df)
  df = n-2
  delta = beta1/sqrt(sig2b1)
  prob1 = function(delta){pt(tc,df,delta)}
  prob2 = function(delta){pt(-tc,df,delta)}
  power = 1-prob1(delta)+prob2(delta)
  power
}

# c)
Y_c = lapply(1:num_vectors, make_y, beta = 1.5)
test_c = lapply(Y_c, test, X)
sum(unlist(test_c))/length(test_c)
power(1.5)

# d)
Y_d = lapply(1:num_vectors, make_y_b, beta = 1.5)
test_c = lapply(Y_c, test, X)
sum(unlist(test_c))/length(test_c)
power(1.5)

# Exercise 8
alpha = 0.05
tc = qt(1-alpha/2, df = 18)
tc
3 + c(-tc, tc)







