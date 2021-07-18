# install.packages("ggplot2")
library(ggplot2)
set.seed(5)

############ Hawaii Section #################

# Hawaii Data
Hawaii_Seasonal = read.table("seasonal_data.txt", header = T)
Hawaii_Annual = read.csv("co2_annmean_mlo.csv", skip = 55, header = T)
# Hawaii_monthly =


Hawaii_Seasonal$summer = as.factor(Hawaii_Seasonal$summer)
Hawaii_Seasonal$autumn = as.factor(Hawaii_Seasonal$autumn)
Hawaii_Seasonal$winter = as.factor(Hawaii_Seasonal$winter)
Hawaii_Seasonal$nthqrtsqr = (Hawaii_Seasonal$nthqrt)^2
#Decmial year
Hawaii_Seasonal$yeardec = min(Hawaii_Seasonal$year)+Hawaii_Seasonal$nthqrt
attach(Hawaii_Seasonal)

#Seasonal plots (LOG)
xmax = 30 #Maximum time on Plot

plot(yeardec[summer=="1"], log(concentration[summer=="1"]), 
     main = "Seasonal CO2 Plot",
     type="l", xlab="Time", ylab=" Log Concentration", 
     xlim = c(min(yeardec),min(yeardec) + xmax), col="black", 
     ylim = c(log(concentration[1]), log(concentration[4*xmax])))

lines(yeardec[autumn=="1"], log(concentration[autumn=="1"]), col="red")
lines(yeardec[winter=="1"], log(concentration[winter=="1"]), col="green")
lines(yeardec[winter=="0" & summer =="0" & autumn == "0"], 
      log(concentration[winter=="0" & summer =="0" & autumn == "0"]),
      col="blue")
legend("bottomright" ,legend=c("Summer", "Autumn", "Winter", "Spring"),
       col=c("black", "red", "green", "blue"), lty=1:2, cex=0.8)

##### Seasonal Model #####

model = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+winter, data = Hawaii_Seasonal)
summary(model)

predct_response = predict(model, newdata = Hawaii_Seasonal)

#Regular Plots, Add Legends
plot(nthqrt, concentration, type="l", xlab="Time", ylab="Concentrations",
     main = "Actual vs Predicted")
lines(nthqrt, predct_response, col=2)
legend("topleft" ,legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
legend("bottomright", bty="n", 
       legend=paste("Adj R2 is", format(summary(model)$adj.r.squared, digits=4)))

ggplot(Hawaii_Seasonal, aes(yeardec, concentration)) +
  geom_line() +
  ggtitle("Hawaii Carbon Dioxide Seasonal Trend ") +
  labs(x="Time", y="Carbon Dioxide Concentration") #units


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
detach(Hawaii_Seasonal_8sample)

##### Long Term Model #####

############ Australia Section #################
#Australia Data
Australia_Seasonal = read.table("australia_seasonal_data.txt", header = T)

Australia_Seasonal$summer = as.factor(Australia_Seasonal$summer)
Australia_Seasonal$autumn = as.factor(Australia_Seasonal$autumn)
Australia_Seasonal$winter = as.factor(Australia_Seasonal$winter)
Australia_Seasonal$nthqrtsqr = (Australia_Seasonal$nthqrt)^2
#Decmial year
Australia_Seasonal$yeardec = min(Australia_Seasonal$year)+Australia_Seasonal$nthqrt

attach(Australia_Seasonal)

#Seasonal plots (LOG)
xmax = 20 #Maximum time on Plot

plot(yeardec[summer=="1"], log(concentration[summer=="1"]), 
     main = "Seasonal CO2 Plot",
     type="l", xlab="Time", ylab=" Log Concentration", 
     xlim = c(min(yeardec),min(yeardec) + xmax), col="black", 
     ylim = c(log(concentration[1]), log(concentration[4*xmax])))

lines(yeardec[autumn=="1"], log(concentration[autumn=="1"]), col="red")
lines(yeardec[winter=="1"], log(concentration[winter=="1"]), col="green")
lines(yeardec[winter=="0" & summer =="0" & autumn == "0"], 
      log(concentration[winter=="0" & summer =="0" & autumn == "0"]),
      col="blue")
legend("bottomright" ,legend=c("Summer", "Autumn", "Winter", "Spring"),
       col=c("black", "red", "green", "blue"), lty=1:2, cex=0.8)

##### Seasonal Model #####
model = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+winter, 
           data = Australia_Seasonal)
summary(model)

predct_response = predict(model, newdata = Australia_Seasonal)

#Regular Plots, Add Legends
plot(nthqrt, concentration, type="l", xlab="Time", ylab="CO2", 
     main = "Actual vs Predicted")
lines(nthqrt, predct_response, col=2)
legend("topleft" ,legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
legend("bottomright", bty="n", 
       legend=paste("Adj R2 is", format(summary(model)$adj.r.squared, digits=4)))

ggplot(Australia_Seasonal, aes(nthqrt, concentration)) +
  geom_line() +
  ggtitle("Australia Carbon Dioxide Seasonal Trend ") +
  labs(x="Time", y="Carbon Dioxide Concentration")


# analysis
index = sample(1:252, size=150)
Australia_Seasonal1 = Australia_Seasonal[index,]
Australia_Seasonal2 = Australia_Seasonal[-index,]
detach(Australia_Seasonal)
attach(Australia_Seasonal1)

# linear trend does not work well
#Quadratic
Australia_Seasonal1.lm = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+winter, data = Australia_Seasonal1)
Australia_Seasonal1.lm.pr = predict(Australia_Seasonal1.lm, newdata = Australia_Seasonal2)

#Non Quadratic
Australia_Seasonal1.li = lm(concentration~nthqrt+summer+autumn+winter, data = Australia_Seasonal1)
Australia_Seasonal1.li.pr = predict(Australia_Seasonal1.li, newdata = Australia_Seasonal2)

# mean square error?
#quadratic
sqrt(mean((Australia_Seasonal2$concentration-Australia_Seasonal1.lm.pr)^2))
#Linear
sqrt(mean((Australia_Seasonal2$concentration-Australia_Seasonal1.li.pr)^2))



# Estimation of coefficients
detach(Australia_Seasonal1)
Australia_Seasonal_8sample = Australia_Seasonal[sample(nrow(Australia_Seasonal), 8), ]
attach(Australia_Seasonal_8sample)
col0 = rep(c(1), each=8)
col1 = c(nthqrt)
col2 = c(nthqrtsqr)
col3 = c(0,0,1,1,0,0,0,0)
col4 = c(0,0,0,0,1,1,0,0)
col5 = c(0,0,0,0,0,0,1,1)

x = cbind(col0, col1, col2, col3, col4, col5)
y = c(concentration)
solve(t(x) %*% x) %*% t(x) %*% y
detach(Australia_Seasonal_8sample)
