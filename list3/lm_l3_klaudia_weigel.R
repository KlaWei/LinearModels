# Exercise 1 

# ---- a) two sided P(|T| > t_c) = alpha => t_c = t*(1 - alpha/2, n-2)

df = 10
alpha = 0.05
t_c = qt(1 - alpha/2, df)

# ---- b) F signifcance test

F_c = qf(1-alpha, 1, df)

# ---- c)
t_c^2
F_c

# Exercise 2
SSM = 100; SSE = 400; dfM = 1; dfE = 20
MSM = SSM/dfM; MSE = SSE/dfE; SST = SSM + SSE

# ---- a) how many observations?
n = dfE + 2
# ---- b) Estimate of sigma
s = sqrt(MSE)

# ---- c) Test if beta_1 = 0

# we reject when F > F_c or P0(Z > F) < alpha Z ~ F(1, n-2)
F_stat = MSM/MSE
F_c = qf(1-alpha, 1, n-2)
# p-val
(1 - pf(abs(F_stat), 1, n-2))

# ---- d) What proportion of the variation of the response variable is explained by your model?
R_2 = SSM/SST

# ---- e) What is the sample correlation coefficient between your response and explanatory variables?

r = sqrt(R_2)


#### Exercise 3
dat = read.table("tabela1_6.txt", col.names = c("id", "gpa", "iq", "gender", "piers_harris"))
model = lm(gpa ~ iq, data = dat)
Y = dat[,2]; X = dat[,3]; n = length(Y)
beta_1 = sum((X-mean(X)) %*% (Y - mean(Y)))/sum((X-mean(X))^2)
beta_0 = mean(Y) - beta_1*mean(X)
sigma_squared = (1/(n-2))*sum((Y - beta_0 - beta_1*X)^2)
T = beta_1/sqrt(sigma_squared/sum((X - mean(X))^2))

# p value => the same results as in the summary
2*(1 - pt(abs(T), df=n-2))

model$coefficients[1]
SST = sum((Y-mean(Y))^2)
SSE = sum(model$residuals^2)
SSM = SST-SSE
R_2 = SSM/SST
summary(model)$r.squared
summary(model)[["coefficients"]][, "t value"][2]
summary(model)[["coefficients"]][, "Pr(>|t|)"][2]


# --- b)
predict(model, data.frame(iq = c(100)), interval='predict', level = 0.9)

# --- c)
dat_plot = data.frame(X, Y)
pred = predict(model, interval = "prediction")
dat_plot = cbind(dat_plot, pred)
library(ggplot2)
ggplot(data = dat_plot, aes(x=X)) +
  geom_point(aes(y = Y), alpha = 0.6) +
  geom_line(aes(y = fit), color = "red") +
  geom_line(aes(y = lwr), color = "blue") +
  geom_line(aes(y = upr), color = "blue")


#### Exercise 4

# ---- a)
model2 = lm(gpa~piers_harris, data = dat)
# E(Y) = 0.091X + 2.2258
model2$coefficients

# ---- b)
# t value for piers_harris = 5.62, p val = 3.01e-07
summary(model2)

# ---- c)
predict(model2, data.frame(piers_harris = c(60)), interval='predict', level = 0.9)

# ---- d)
Y2 = dat$gpa
X2 = dat$piers_harris
dat_plot = data.frame(X2, Y2)
pred = predict(model2, interval = "prediction")
dat_plot = cbind(dat_plot, pred)
#dat_plot = dat_plot[order(dat_plot$X2),]

ggplot(data = dat_plot, aes(x=X2)) +
  geom_point(aes(y = Y2), alpha = 0.6) +
  geom_line(aes(y = fit), color = "red") +
  geom_line(aes(y = lwr), color = "blue") +
  geom_line(aes(y = upr), color = "blue")


#### Exercise 5

dat2 = read.table("ch01pr20.txt", col.names = c("hours", "copiers"))
model3 = lm(hours~copiers, data = dat2)
# ---- a)
sum(model3$residuals)
# ---- b)
plot_dat_ex5 = data.frame(dat2$copiers, model3$residuals)
ggplot(data = plot_dat_ex5, aes(x = dat2.copiers, y= model3.residuals)) +
  geom_point() +  
  geom_hline(yintercept = 0) +
  labs(x="Copiers", y="Residuals")

# ---- c)

plot_dat_ex5_ord = data.frame(1:45, model3$residuals)
colnames(plot_dat_ex5_ord) = c("ord", "res")
ggplot(data = plot_dat_ex5_ord, aes(x = ord, y= res)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x="Order", y="Residuals")

# ---- d)
hist(model3$residuals)
res_df = data.frame(model3$residuals)
colnames(res_df) = c("residuals")
ggplot(data = res_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), binwidth = 5, fill = "blue", color = "black", alpha=0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(res_df$residuals), sd = sd(res_df$residuals))) +
  xlim(-30, 25)


#### Exercise 6

dat2_mod = dat2
dat2_mod$hours[1] = 2000

model3_mod = lm(hours~copiers, data = dat2_mod)
equations = sprintf("E(Time) = %.3f + %.3f*Copiers", 
                    c(model3$coefficients[1], model3_mod$coefficients[1]), 
                    c(model3$coefficients[2], model3_mod$coefficients[2]))

comp_df = rbind(equations, 
                round(c(summary(model3)[["coefficients"]][, "t value"][2], 
                        summary(model3_mod)[["coefficients"]][, "t value"][2]), digits = 3),
                round(c(summary(model3)[["coefficients"]][, "Pr(>|t|)"][2], 
                        summary(model3_mod)[["coefficients"]][, "Pr(>|t|)"][2]), digits = 3),
                round(c(summary(model3)$r.squared, summary(model3_mod)$r.squared), digits = 3),
                round(c(sigma(model3)^2, sigma(model3_mod)^2), digits = 3))

comp_df = data.frame(comp_df)
colnames(comp_df) = c("Original model", "Modified model")
rownames(comp_df) = c("Regression equations", "t value", "Pr(>|t|)", "$R^2$", "$\\sigma^2$")


p1 = ggplot(dat2_mod, aes(x=copiers, y=hours))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)

p2 = ggplot(dat2, aes(x=copiers, y=hours))+
  geom_point()+
  geom_smooth(method=lm, se=TRUE)

plot_dat_ex5 = data.frame(dat2_mod$copiers, model3_mod$residuals)
ggplot(data = plot_dat_ex5, aes(x = dat2_mod.copiers, y= model3_mod.residuals)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x="Copiers", y="Residuals")

plot_dat_ex5_ord = data.frame(1:45, model3_mod$residuals)
colnames(plot_dat_ex5_ord) = c("ord", "res")
ggplot(data = plot_dat_ex5_ord, aes(x = ord, y= res)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(x="Order", y="Residuals")

res_df = data.frame(model3_mod$residuals)
colnames(res_df) = c("residuals")
ggplot(data = res_df, aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), binwidth = 100, fill = "blue", color = "black", alpha=0.5) +
  stat_function(fun = dnorm, args = list(mean = mean(res_df$residuals), sd = sd(res_df$residuals)))


#### Exercise 7
dat3 = read.table("ch03pr15.txt", col.names = c("concentration", "time"))
X = dat3$time
Y = dat3$concentration
model4 = lm(concentration~time, data = dat3)
model4$coefficients
summary(model4)$r.squared
# t value for beta_1 = -7.483 pval = 4.61e-06
summary(model4)


#### Exercise 8

X = dat3$time
dat_plot = data.frame(X, Y)
pred = predict(model4, interval = "prediction")
dat_plot = cbind(dat_plot, pred)
#dat_plot = dat_plot[order(dat_plot$X2),]

p_ex8 = ggplot(data = dat_plot, aes(x=X)) +
  geom_point(aes(y = Y), alpha = 0.6) +
  geom_line(aes(y = fit), color = "red") +
  geom_line(aes(y = lwr), color = "blue") +
  geom_line(aes(y = upr), color = "blue")

p_ex8

ggplot(data.frame(X, model4$residuals), aes(x = X, y = model4.residuals)) + 
  geom_point() +
  geom_hline(yintercept = 0)


# correlation between the predicted and observed values = sqrt(R^2)
sqrt(summary(model4)$r.squared)
A = matrix(c(dat3$concentration, predict(model4)), nrow = 15)
cor(A)

#### Exercise 9

bc = MASS::boxcox(dat3$concentration~dat3$time)

lambda = bc$x[which.max(bc$y)]


#### Exercise 10
model5 = lm(log(concentration)~time, data = dat3)
model5$coefficients
summary(model5)[["coefficients"]][, "t value"][2]
summary(model5)[["coefficients"]][,  "Pr(>|t|)"][2]

X = dat3$time
Y2 = log(dat3$concentration)
dat_plot = data.frame(X, Y2)
pred = predict(model5, interval = "prediction")
dat_plot = cbind(dat_plot, pred)
#dat_plot = dat_plot[order(dat_plot$X2),]

ggplot(data = dat_plot, aes(x=X)) +
  geom_point(aes(y = Y2), alpha = 0.6) +
  geom_line(aes(y = fit), color = "red") +
  geom_line(aes(y = lwr), color = "blue") +
  geom_line(aes(y = upr), color = "blue") +
  labs(x="Time", y="Concentration")


# Exercise 11
dat_plot = data.frame(X, Y2)
pred = predict(model5, interval = "prediction")
dat_plot = cbind(dat_plot, pred)
#dat_plot = dat_plot[order(dat_plot$X2),]

p_ex11 = ggplot(data = dat_plot, aes(x=X)) +
  geom_point(aes(y = exp(Y2)), alpha = 0.6) +
  geom_smooth(aes(y = exp(fit)), color = "red", size = 0.5) +
  geom_smooth(aes(y = exp(lwr)), color = "blue", size = 0.5) +
  geom_smooth(aes(y = exp(upr)), color = "blue", size = 0.5)+
  labs(x="Time", y="Concentration")

sqrt(summary(model5)$r.squared)

#### Exercise 12

T1 = dat3$time^(-1/2)
model6 = lm(log(dat3$concentration)~T1)
model6$coefficients
summary(model6)[["coefficients"]][, "t value"][2]
summary(model6)[["coefficients"]][,  "Pr(>|t|)"][2]
summary(model6)$r.squared

X = T1
Y = log(dat3$concentration)
dat_plot = data.frame(X, Y)
pred = predict(model6, interval = "prediction")
dat_plot = cbind(dat_plot, pred)
#dat_plot = dat_plot[order(dat_plot$X2),]

ggplot(data = dat_plot, aes(x=X)) +
  geom_point(aes(y = Y), alpha = 0.6) +
  geom_line(aes(y = fit), color = "red") +
  geom_line(aes(y = lwr), color = "blue") +
  geom_line(aes(y = upr), color = "blue")+
  labs(x="Time^(-1/2)", y="log(Concentration)")

sqrt(summary(model6)$r.squared)

dat_plot = data.frame(X, Y)
pred = predict(model6, interval = "prediction")
dat_plot = cbind(dat_plot, pred)
#dat_plot = dat_plot[order(dat_plot$X2),]

p_ex12 = ggplot(data = dat_plot, aes(x=X)) +
  geom_point(aes(y = exp(Y)), alpha = 0.6) +
  geom_smooth(aes(y = exp(fit)), color = "red", size = 0.5) +
  geom_smooth(aes(y = exp(lwr)), color = "blue", size = 0.5) +
  geom_smooth(aes(y = exp(upr)), color = "blue", size = 0.5) +
  labs(x="Time^(-1/2)", y="Concentration")



