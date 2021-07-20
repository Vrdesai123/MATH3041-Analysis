# install.packages("ggplot2")
#11111
library(ggplot2)
set.seed(5)

################## Hawaii Section #######################

# Hawaii Data
Hawaii_Seasonal = read.table("hawaii_seasonal_data.txt", header = T)
Hawaii_Annual = read.csv("co2_annmean_mlo.csv", skip = 55, header = T)

# MATH2831 lecture notes indicates that we don't need
# to change the type of categorical to factor in R when
# we fit linear model.

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
lines(yeardec[spring=="1"], log(concentration[spring=="1"]), col="blue")
legend("bottomright" ,legend=c("Summer", "Autumn", "Winter", "Spring"),
       col=c("black", "red", "green", "blue"), lty=1:2, cex=0.8)

##### Seasonal Model #####

Hawaii_model = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+spring, data = Hawaii_Seasonal)
summary(Hawaii_model)

predct_response = predict(Hawaii_model, newdata = Hawaii_Seasonal)

#Regular Plots, Add Legends
plot(nthqrt, concentration, type="l", xlab="Time", ylab="Concentrations",
     main = "Actual vs Predicted")
lines(nthqrt, predct_response, col=2)
legend("topleft" ,legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
legend("bottomright", bty="n", 
       legend=paste("Adj R2 is", format(summary(Hawaii_model)$adj.r.squared, digits=4)))

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
Hawaii_Seasonal1.lm = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+spring, data = Hawaii_Seasonal1)
Hawaii_Seasonal1.lm.pr = predict(Hawaii_Seasonal1.lm, newdata = Hawaii_Seasonal2)

#Non Quadratic
Hawaii_Seasonal1.li = lm(concentration~nthqrt+summer+autumn+spring, data = Hawaii_Seasonal1)
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


####################### Australia Section #######################
#Australia Data
Australia_Seasonal = read.table("australia_seasonal_data.txt", header = T)


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
lines(yeardec[spring =="1"], 
      log(concentration[spring =="1"]),
      col="blue")
legend("bottomright" ,legend=c("Summer", "Autumn", "Winter", "Spring"),
       col=c("black", "red", "green", "blue"), lty=1:2, cex=0.8)

##### Seasonal Model #####
AUS_model = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+spring, 
           data = Australia_Seasonal)
summary(AUS_model)

predct_response = predict(AUS_model, newdata = Australia_Seasonal)

#Regular Plots, Add Legends
plot(nthqrt, concentration, type="l", xlab="Time", ylab="CO2", 
     main = "Actual vs Predicted")
lines(nthqrt, predct_response, col=2)
legend("topleft" ,legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
legend("bottomright", bty="n", 
       legend=paste("Adj R2 is", format(summary(AUS_model)$adj.r.squared, digits=4)))

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
Australia_Seasonal1.lm = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+spring, data = Australia_Seasonal1)
Australia_Seasonal1.lm.pr = predict(Australia_Seasonal1.lm, newdata = Australia_Seasonal2)

#Non Quadratic
Australia_Seasonal1.li = lm(concentration~nthqrt+summer+autumn+spring, data = Australia_Seasonal1)
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



################## Global Section #######################
Global_Seasonal = read.table("combined_seasonal_data.txt", header = T)
#attach(global_seasonal)


global_model = lm(concentration~nthqrt+I(nthqrt^2)+spring+summer+autumn, data = Global_Seasonal)
# global_model$fitted.values
summary(global_model)

# diagnostic plot
par(mfrow=c(2,2))
plot(global_model) 
par(mfrow=c(1,1))
# Residual plot shows that linearity assumption is hold?
# Normal Q-Q plot shows that normality assumption is hold?
# Scale-Location shows that no.1,46,52 are potentially considered as outliers


# predict_response = predict(global_model, newdata = global_seasonal)
# lines(global_seasonal$nthqrt, predict_response, col=2)

# plot concentrations against nthqrt
# Global data
plot(Global_Seasonal$nthqrt,Global_Seasonal$concentration, type = "l", xlab = "Time in Quarter", ylab = "Carbon Dioxide Concentration")
# Hawaii seasonal data
lines(Hawaii_Seasonal$nthqrt, Hawaii_Seasonal$concentration, col = 2)
# Australia seasonal data
lines(Australia_Seasonal$nthqrt, Australia_Seasonal$concentration, col = 3)

# Result
# Distribution of global data is in the middle of Hawaii and Australia

# PRESS Statistic (smaller is better)
pr = residuals(global_model)/(1-hatvalues(global_model))
PRESS = sum(pr^2) # PRESS = 63.60526


# CVC: select smallest one
# install.packages("cvTools")
library(cvTools)
CVC = cvFit(global_model, data = Global_Seasonal, y=Global_Seasonal$concentration, K=5, seed=1)
CVC$cv # 0.6146834
# We can do the CVC test for different model and then do the selection


# Checking for influential observation, interpretation is in MATH2831 Wk8 Slide.33
influence.measures(global_model) # no.1,3,4,46,47,52,.... (have a high leverage)

# if we refit the model after the removal of influential 
# observations (haven't removed all the influential observations since full
# results cannot display on the screen)
global_model2 = lm(concentration~nthqrt+I(nthqrt^2)+spring+summer+autumn, data = Global_Seasonal[-c(1,3,4,46,47,52),])
summary(global_model2)

# Compare the fitted value of nthqrt = 21.25
newdata = data.frame(nthqrt = 21.25, spring = 0, summer = 1, autumn = 0)

# model with full observations
predict(global_model, newdata, interval = "confidence")
predict(global_model, newdata, interval = "prediction")

# model with some influential observations removed
predict(global_model2, newdata, interval = "confidence")
predict(global_model2, newdata, interval = "prediction")

# Result: both the CI for the mean response and the PI have reduced in size,
# but not much. This is probably because the goodness-of-fit of the orginial
# model has already been super high.

# diagnostic plot
par(mfrow=c(2,2))
plot(global_model2)
par(mfrow=c(1,1))

# Analysis
index = sample(1:176, 88)
global_seasonal1 = Global_Seasonal[index,]
global_seasonal2 = Global_Seasonal[-index,]
attach(global_seasonal1)

global_seasonal1.lm = lm(concentration~nthqrt+I(nthqrt^2)+spring+summer+autumn, data = global_seasonal1)
global_seasonal1.lm.pr = predict(global_seasonal1.lm, newdata = global_seasonal2)

# mean square error?
sqrt(mean((global_seasonal2$concentration-global_seasonal1.lm.pr)^2))  # 0.6236509
detach(global_seasonal1)


################## Long Term Model #######################
#### Hawaii Data ####
Hawaii_LT_Data = read.table("long_term_data.txt", header = T)
attach(Hawaii_LT_Data)

LT_model = lm(average~indicator+I(indicator^2)+
                sin((pi/6)*indicator)+cos((pi/6)*indicator)
              , data = Hawaii_LT_Data)
summary(LT_model)

#Fit Data
predct_response_LT = predict(LT_model, newdata = Hawaii_LT_Data) 

#Actual vs Predicted graph on Hawaii
plot(indicator, average, type="l", xlab="Time", ylab="Concentrations",
     main = "LT Actual vs Predicted")
lines(indicator, predct_response_LT, col=2)
legend("topleft" ,legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
legend("bottomright", bty="n", 
       legend=paste("Adj R2 is", format(summary(LT_model)$adj.r.squared, digits=4)))
detach(Hawaii_LT_Data)

# Analysis
index = sample(1:176, 88)
Hawaii_LT_Data1 = Hawaii_LT_Data[index,]
Hawaii_LT_Data2 = Hawaii_LT_Data[-index,]
attach(Hawaii_LT_Data1)

Hawaii_LT_Data1.lm = lm(average~indicator+I(indicator^2)+
                          sin((pi/6)*indicator)+cos((pi/6)*indicator)
                        , data = Hawaii_LT_Data1)
Hawaii_LT_Data1.lm.pr = predict(Hawaii_LT_Data1.lm, newdata = Hawaii_LT_Data2)

# mean square error #incorrect currently
mse <- function(lmsum) 
  mean(summary(global_seasonal1.lm)$residuals^2)

detach(Hawaii_LT_Data1)

