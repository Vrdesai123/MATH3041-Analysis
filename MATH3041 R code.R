setwd("D:/UNSW/2021 T2/MATH3041/Assignment/Group Assignment")

seasonal = read.table("seasonal_data.txt", header = T)
# attach(seasonal)
seasonal$summer = as.factor(seasonal$summer)
seasonal$autumn = as.factor(seasonal$autumn)
seasonal$winter = as.factor(seasonal$winter)
seasonal$nthqrtsqr = (seasonal$nthqrt)^2
attach(seasonal)

# summer = as.factor(summer)
# autumn = as.factor(autumn)
# winter = as.factor(winter)

plot(nthqrt, concentration, type="l", xlab="Time", ylab="Concentrations")


plot(seasonal)

plot(nthqrt[summer=="1"], concentration[summer=="1"], type="l", xlab="Time", ylab="Concentration")
lines(nthqrt[autumn=="1"], concentration[autumn=="1"], col=2)
lines(nthqrt[winter=="1"], concentration[winter=="1"], col=3)

model = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+winter, data = seasonal)
summary(model)

predct_response = predict(model, newdata = seasonal)

lines(nthqrt, predct_response, col=2)

# model2 = lm(log(concentration)~nthqrt+I(nthqrt^2)+summer+autumn+winter, data = seasonal)
# summary(model2)



# install.packages("ggplot2")
library(ggplot2)

ggplot(seasonal, aes(nthqrt, concentration)) +
  geom_line() +
  labs(x="Time", y="Carbon Dioxide Concentration")


# analysis
index = sample(1:252, size=150)
seasonal1 = seasonal[index,]
seasonal2 = seasonal[-index,]
attach(seasonal1)

# linear trend does not work well
seasonal1.lm = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+winter, data = seasonal1)
seasonal1.lm.pr = predict(seasonal1.lm, newdata = seasonal2)

seasonal1.li = lm(concentration~nthqrt+summer+autumn+winter, data = seasonal1)
seasonal1.li.pr = predict(seasonal1.li, newdata = seasonal2)

# mean square error?
sqrt(mean((seasonal2$concentration-seasonal1.lm.pr)^2)) 

# sqrt(mean((seasonal2$concentration-seasonal1.li.pr)^2)) # 3.95


# OLS
# come up with the matirx
x = model.matrix(model)

## make my response y: dataframe(i.e. risk)$variable(i.e. risk)
y = concentration
solve(t(x) %*% x) %*% t(x) %*% y

col0 = rep(c(1), each=252)
col1 = seasonal$nthqrt
col2 = seasonal$nthqrtsqr
col3 = seasonal$summer
col4 = seasonal$autumn
col5 = seasonal$winter
response = seasonal$concentration

M_temp = cbind(col0, col1, col2, col3, col4, col5)
solve(t(M_temp) %*% M_temp) %*% t(M_temp) %*% response

# Estimation of coefficients
col0 = rep(c(1), each=8)
col1 = c(24,43,13.25,37.25,46.5,51.5,17.75,48.75)
col2 = c(576,1849,175.5625,1387.563,2162.25,2652.25,315.0625,2376.563)
col3 = c(0,0,1,1,0,0,0,0)
col4 = c(0,0,0,0,1,1,0,0)
col5 = c(0,0,0,0,0,0,1,1)

x = cbind(col0, col1, col2, col3, col4, col5)
y = c(343.8367,373.4033,327.12,361.48,375.13,385.2733,331.64,383.08)

solve(t(x) %*% x) %*% t(x) %*% y
