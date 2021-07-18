# install.packages("ggplot2")
library(ggplot2)
set.seed(5)

# Hawaii Data
Hawaii_Seasonal = read.table("seasonal_data.txt", header = T)

# attach(seasonal)
Hawaii_Seasonal$summer = as.factor(Hawaii_Seasonal$summer)
Hawaii_Seasonal$autumn = as.factor(Hawaii_Seasonal$autumn)
Hawaii_Seasonal$winter = as.factor(Hawaii_Seasonal$winter)
Hawaii_Seasonal$nthqrtsqr = (Hawaii_Seasonal$nthqrt)^2
attach(Hawaii_Seasonal)


plot(nthqrt, concentration, type="l", xlab="Time", ylab="Concentrations")

#Seasonal plots (LOG)
xmax = 30 #Maximum time on Plot

plot(nthqrt[summer=="1"], log(concentration[summer=="1"]), type="l", 
     xlab="Time", ylab=" Log Concentration", xlim = c(0, xmax), 
     ylim = c(log(concentration[1]), log(concentration[4*xmax])))
lines(nthqrt[autumn=="1"], log(concentration[autumn=="1"]), col=2)
lines(nthqrt[winter=="1"], log(concentration[winter=="1"]), col=3)
lines(nthqrt[winter=="0" & summer =="0" & autumn == "0"], 
      log(concentration[winter=="0" & summer =="0" & autumn == "0"]), col=4)

model = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+winter, data = Hawaii_Seasonal)
summary(model)

predct_response = predict(model, newdata = Hawaii_Seasonal)

lines(nthqrt, predct_response, col=2)

ggplot(Hawaii_Seasonal, aes(nthqrt, concentration)) +
  geom_line() +
  labs(x="Time", y="Carbon Dioxide Concentration")


# analysis
index = sample(1:252, size=150)
Hawaii_Seasonal1 = Hawaii_Seasonal[index,]
Hawaii_Seasonal2 = Hawaii_Seasonal[-index,]
detach(Hawaii_Seasonal)
attach(Hawaii_Seasonal1)

# linear trend does not work well
#Quadratic
Hawaii_Seasonal1.lm = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+winter, data = Hawaii_Seasonal1)
Hawaii_Seasonal1.lm.pr = predict(Hawaii_Seasonal1.lm, newdata = Hawaii_Seasonal2)

#Non Quadratic
Hawaii_Seasonal1.li = lm(concentration~nthqrt+summer+autumn+winter, data = Hawaii_Seasonal1)
Hawaii_Seasonal1.li.pr = predict(Hawaii_Seasonal1.li, newdata = Hawaii_Seasonal2)

# mean square error?
#quadratic
sqrt(mean((Hawaii_Seasonal2$concentration-Hawaii_Seasonal1.lm.pr)^2))
#Linear
sqrt(mean((Hawaii_Seasonal2$concentration-Hawaii_Seasonal1.li.pr)^2))



# Estimation of coefficients
detach(Hawaii_Seasonal1)
Hawaii_Seasonal_8sample = Hawaii_Seasonal[sample(nrow(Hawaii_Seasonal), 8), ]
attach(Hawaii_Seasonal_8sample)
col0 = rep(c(1), each=8)
col1 = c(nthqrt)
col2 = c(nthqrtsqr)
col3 = c(0,0,1,1,0,0,0,0)
col4 = c(0,0,0,0,1,1,0,0)
col5 = c(0,0,0,0,0,0,1,1)

x = cbind(col0, col1, col2, col3, col4, col5)
y = c(concentration)
solve(t(x) %*% x) %*% t(x) %*% y
