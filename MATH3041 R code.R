# install.packages("ggplot2")
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
     main = "Hawaii Seasonal CO2 Plot",
     type="l", xlab="Time (years)", ylab=" Log Concentration", 
     xlim = c(min(yeardec),min(yeardec) + xmax), col="black", 
     ylim = c(log(concentration[1]), log(concentration[4*xmax])))

lines(yeardec[autumn=="1"], log(concentration[autumn=="1"]), col="red")
lines(yeardec[winter=="1"], log(concentration[winter=="1"]), col="green")
lines(yeardec[spring=="1"], log(concentration[spring=="1"]), col="blue")
legend("bottomright" ,legend=c("Summer", "Autumn", "Winter", "Spring"),
       col=c("black", "red", "green", "blue"), lty=1:2, cex=0.8)

##### Seasonal Model #####

Hawaii_model = lm(concentration~nthqrt+I(nthqrt^2)+spring+summer+autumn, data = Hawaii_Seasonal)
summary(Hawaii_model)

predct_response = predict(Hawaii_model, newdata = Hawaii_Seasonal)

#Regular Plots, Add Legends
plot(nthqrt, concentration, type="l", xlab="Time (years)", ylab="CO2 Concentration (ppm)",
     main = "Hawaii Sesaonal Actual vs Predicted")
lines(nthqrt, predct_response, col=2)
legend("topleft" ,legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
legend("bottomright", bty="n", 
       legend=paste("Adj R2 is", format(summary(Hawaii_model)$adj.r.squared, digits=4)))

ggplot(Hawaii_Seasonal, aes(yeardec, concentration)) +
  geom_line() +
  ggtitle("Hawaii Carbon Dioxide Seasonal Trend ") +
  labs(x="Time", y="CO2 Concentration (ppm)") #units

#sensitivity analysis
#a lot of error, and I do not know why the robustness value is sooooooo high
#install.packages("sensemakr")
library(sensemakr)

Hawaii_sense_model = lm(concentration~nthqrt+I(nthqrt^2)+summer+autumn+spring, data = Hawaii_Seasonal)
Hawaiisummer_model.sensitivity<-sensemakr(
  model=Hawaii_sense_model,
  treatment="nthqrt",
  benchmark_covariates = "summer",
  kd=0.6
)
plot(Hawaiisummer_model.sensitivity)
ovb_minimal_reporting(Hawaiisummer_model.sensitivity, format = "latex")#the  robustness value is too high here

Hawaiiautumn_model.sensitivity<-sensemakr(
  model=Hawaii_model,
  treatment="nthqrt",
  benchmark_covariates = "autumn",
  kd=1:3
)
plot(Hawaiiautumn_model.sensitivity)
ovb_minimal_reporting(Hawaiiautumn_model.sensitivity, format = "latex")#these reports show the same result

Hawaiispring_model.sensitivity<-sensemakr(
  model=Hawaii_model,
  treatment="nthqrt",
  benchmark_covariates = "spring",
  kd=0.8
)
plot(Hawaiispring_model.sensitivity)
ovb_minimal_reporting(Hawaiispring_model.sensitivity, format = "latex")
#the confounders influenced results in a large extent

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
     main = "Australia Seasonal CO2 Plot",
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

#Regular Plots
plot(nthqrt, concentration, type="l", xlab="Time (years)", 
     ylab="CO2 Concentration (ppm)", 
     main = "Australia Seasonal Actual vs Predicted")
lines(nthqrt, predct_response, col=2)
legend("topleft" ,legend=c("Actual", "Predicted"),
       col=c("black", "red"), lty=1:2, cex=0.8)
legend("bottomright", bty="n", 
       legend=paste("Adj R2 is", format(summary(AUS_model)$adj.r.squared, digits=4)))

ggplot(Australia_Seasonal, aes(nthqrt, concentration)) +
  geom_line() +
  ggtitle("Australia Carbon Dioxide Seasonal Trend ") +
  labs(x="Time", y="CO2 Concentration (ppm)")


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

library(car)
vif(global_model)

# predict_response = predict(global_model, newdata = global_seasonal)
# lines(global_seasonal$nthqrt, predict_response, col=2)

# plot concentrations against nthqrt
# Global data
plot(Global_Seasonal$nthqrt,Global_Seasonal$concentration, 
     main = "Averaged data with Australian & Hawaiian data" ,
     type = "l", xlab = "Time (years)", ylab = "CO2 Concentration (ppm)")
# Hawaii seasonal data
lines(Hawaii_Seasonal$nthqrt, Hawaii_Seasonal$concentration, col = 2)
# Australia seasonal data
lines(Australia_Seasonal$nthqrt, Australia_Seasonal$concentration, col = 3)
#Create Legend
legend("topleft" ,legend=c("Average", "Hawaiian", "Australian"),
       col=c("black", "red", "green"), lty=1:2, cex=0.8)

# Result
# Distribution of global data is in the middle of Hawaii and Australia


# Analysis: Model Selection (linear, quadratic and exponential trend)
linear_trend = lm(concentration~nthqrt+spring+summer+autumn, data = Global_Seasonal)
quadratic_trend = lm(concentration~nthqrt+I(nthqrt^2)+spring+summer+autumn, data = Global_Seasonal)
exponential_trend = lm(log(concentration)~nthqrt+spring+summer+autumn, data = Global_Seasonal)

# linear.pr = residuals(linear_trend)/(1-hatvalues(linear_trend))
# press.li = sum(linear.pr^2) # 824.2134

# quadratic.pr = residuals(quadratic_trend)/(1-hatvalues(quadratic_trend))
# press.qua = sum(quadratic.pr^2) # 63.60526


index = sample(1:176, size = 88)
Global1 = Global_Seasonal[index,]
Global2 = Global_Seasonal[-index,]
attach(Global2)
Global2$logconcentration = log(concentration)
detach(Global2)
attach(Global1)

global1.li = lm(concentration~nthqrt+spring+summer+autumn, data = Global1)
global1.qua = lm(concentration~nthqrt+I(nthqrt^2)+spring+summer+autumn, data = Global1)
global1.exp = lm(log(concentration)~nthqrt+spring+summer+autumn, data = Global1)

global1.li.pr = predict(global1.li, newdata=Global2)
global1.qua.pr = predict(global1.qua, newdata=Global2)
global1.exp.pr = predict(global1.exp, newdata=Global2)

sqrt(mean((Global2$concentration-global1.li.pr)^2)) # 2.085027
sqrt(mean((Global2$concentration-global1.qua.pr)^2)) # 0.6518892
sqrt(mean((exp(Global2$logconcentration)-exp(global1.exp.pr))^2)) # 1.515733
detach(Global1)



# I DON'T THINK WE HAVE ENOUGH SPACE FOR THIS PART, SO I DECIDE TO EXCLUDE THIS PART
# Checking for influential observation, interpretation is in MATH2831 Wk8 Slide.33
# options(max.print=999999)
# influence.measures(global_model) # no.1,3,4,46,47,52,.... (have a high leverage)

# if we refit the model after the removal of influential 
# observations (haven't removed all the influential observations since full
# results cannot display on the screen)
# global_model2 = lm(concentration~nthqrt+I(nthqrt^2)+spring+summer+autumn, data = Global_Seasonal[-c(1,3,4,46,47,52),])
# summary(global_model2)

# Compare the fitted value of nthqrt = 21.25
# newdata = data.frame(nthqrt = 21.25, spring = 0, summer = 1, autumn = 0)

# model with full observations
# predict(global_model, newdata, interval = "confidence")
# predict(global_model, newdata, interval = "prediction")

# model with some influential observations removed
# predict(global_model2, newdata, interval = "confidence")
# predict(global_model2, newdata, interval = "prediction")

# Result: both the CI for the mean response and the PI have reduced in size,
# but not much. This is probably because the goodness-of-fit of the orginial
# model has already been super high.

# diagnostic plot
# par(mfrow=c(2,2))
# plot(global_model2)
# par(mfrow=c(1,1))



################## Long Term Model #######################
Hawaii_LT_Data = read.table("Hawaii_long_term_data.txt", header = T)
Aus_LT_Data = read.table("Aus_long_term_data.txt", header = T)
Hawaii_LT_Data$indicator = Hawaii_LT_Data$indicator - 216
LT_Data = merge.data.frame(Hawaii_LT_Data, Aus_LT_Data, by = "indicator" )
LT_Data$concentration = (LT_Data$average.x+LT_Data$average.y)/2
attach(LT_Data)
#### Analysis ####


LT_model = lm(concentration~indicator+I(indicator^2)+
                sin((pi/6)*indicator)+cos((pi/6)*indicator)
              , data = LT_Data)
summary(LT_model)
detach(LT_Data)

#Graphs
Graph_fit = function(df, title){
  attach(df)
  #Fit Data
  predct_response_LT = predict(LT_model, newdata = df) 
  R2 = cor(average, predct_response_LT)^2
  
  #Actual vs Predicted graph on Hawaii
  plot(indicator, average, type="l", xlab="Time", ylab="Concentrations",
       main = paste(title, "LT Actual vs Predicted"))
  lines(indicator, predct_response_LT, col=2)
  legend("topleft" ,legend=c("Actual", "Predicted"),
         col=c("black", "red"), lty=1:2, cex=0.8)
  legend("bottomright", bty="n", 
         legend=paste("Adj R2 is", format(R2, digits=4)))
  detach(df)
}

Graph_fit(Hawaii_LT_Data, "Mona Loa")
Graph_fit(Aus_LT_Data, "Cape Grim")

# Analysis
mse_sim = function(df,n){
  index = sample(1:nrow(df), n)
  LT_Data1 = df[index,]
  LT_Data2 = df[-index,]
  attach(LT_Data1)
  
  #Full Model
  LT_Data1.lm = lm(concentration~indicator+I(indicator^2)+
                     sin((pi/6)*indicator)+cos((pi/6)*indicator)
                   , data = LT_Data1)
  LT_Data1.lm.pr = predict(LT_Data1.lm, newdata = LT_Data2)
  
  
  #Model Without Trig
  
  LT_Data1.poly = lm(concentration~indicator+I(indicator^2)
                     , data = LT_Data1)
  LT_Data1.poly.pr = predict(LT_Data1.poly, newdata = LT_Data2)
  
  #Model Without quadratic
  
  LT_Data1.lin = lm(concentration~indicator+
                      sin((pi/6)*indicator)+cos((pi/6)*indicator)
                    , data = LT_Data1)
  LT_Data1.lin.pr = predict(LT_Data1.lin, newdata = LT_Data2)
  
  #Quad Model
  LT_Data1.quad = lm(concentration~I(indicator^2)+
                     sin((pi/6)*indicator)+cos((pi/6)*indicator)
                   , data = LT_Data1)
  LT_Data1.quad.pr = predict(LT_Data1.quad, newdata = LT_Data2)
  
  a = mean((LT_Data2$concentration-LT_Data1.lm.pr)^2)#MSE Full
  b = mean((LT_Data2$concentration-LT_Data1.poly.pr)^2)#MSE Poly
  c = mean((LT_Data2$concentration-LT_Data1.lin.pr)^2)#MSE Lin + Trig
  d = mean((LT_Data2$concentration-LT_Data1.quad.pr)^2)#MSE Quad
  detach(LT_Data1)
  mylist = list(a,b,c,d,
                LT_Data1.lm.pr,
                LT_Data1.poly.pr,
                LT_Data1.lin.pr,
                LT_Data1.quad.pr,
                LT_Data2)
  names(mylist) = c("MSE_FUll", "MSE_POLY", "MSE_LIN", "MSE_QUAD",
                    "Full_Predict", "Poly_Predict", "Lin_Predict", 
                    "Quad_Predict",
                    "Dataset")
  return(mylist)
}

MSE_FUll = c()
MSE_POLY = c()
MSE_LIN = c()
MSE_QUAD = c()

number_of_sims = 50
No_fit = 50
i=1
while (i <= number_of_sims){
Sim = mse_sim(LT_Data, No_fit)

MSE_FUll[i] = Sim$MSE_FUll
MSE_POLY[i] = Sim$MSE_POLY
MSE_LIN[i] = Sim$MSE_LIN
MSE_QUAD[i] = Sim$MSE_QUAD
i = i + 1
  }



attach(Sim$Dataset)
plot(indicator, concentration, type="l", xlab="Time (months)", ylab="CO2 Concentration (ppm)",
     main = "LT Actual vs Predicted",
     xlim = c(300, max(indicator)),
     ylim = c(360, max(concentration)))
lines(indicator, Sim$Full_Predict, col=2)
lines(indicator, Sim$Poly_Predict, col=3)
lines(indicator, Sim$Lin_Predict, col=4)
#lines(indicator, Sim$Quad_Predict, col=5)
legend("topleft" ,legend=c("Actual", "Full Model", "Quadratic", "Linear + Poly"),
       col=c("black", "red", "green", "deepskyblue"), lty=1:2, cex=0.8)
#legend("topleft" ,legend=c("Actual", "Full Model", "Quadratic", "Linear + Poly", "Quad"),
       #=c("black", "red", "green", "deepskyblue", "cadetblue1"), lty=1:2, cex=0.8)
detach(Sim$Dataset)

mean(MSE_FUll)
mean(MSE_POLY)
mean(MSE_LIN)
#mean(MSE_QUAD)

#install.packages("car")
table(car::vif(LT_model))
car::vif(global_model2)

# mean square error
mse <- function(lmsum){ 
  mean(summary(lmsum)$residuals^2)
}
