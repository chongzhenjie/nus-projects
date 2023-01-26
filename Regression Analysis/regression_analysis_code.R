rm(list=ls())

data<- read.table("FEV.csv",sep=",",header=TRUE)
data
data$Sex=as.factor(data$Sex)
data$Smoke=as.factor(data$Smoke)
attach(data)



# Linear independence of the two height variables

model_1<-lm(FEV~Hgt+Hgt_m+Age+Sex+Smoke,data=data)
summary(model_1)

model_2<-lm(FEV~Hgt+Age+Sex+Smoke,data=data)
summary(model_2)



# Residual analysis on Model 2

qqnorm(rstandard(model_2),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", 
main = "Normal Q-Q Plot of FEV in Model 2",pch=20)
qqline(rstandard(model_2),datax = TRUE, col='red')

plot(model_2$fitted.values,rstandard(model_2), xlab = "Fitted values", 
ylab = "Stadardized Residuals",main = "Residual Plot of FEV in Model 2")
abline(h = 0)



# We perform Box-Cox method on Model 2.

library(MASS)

boxcox(model_2, lambda=seq(-2, 2, by=0.5),optimize=TRUE,plotit = TRUE)



# Refit model with transformation on response log(FEV)

model_3<-lm(log(FEV)~Hgt+Age+Sex+Smoke,data=data)
summary(model_3)

qqnorm(rstandard(model_3),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", 
main = "Normal Q-Q Plot of log(FEV) in Model 3",pch=20)
qqline(rstandard(model_3),datax = TRUE, col='red')

plot(model_3$fitted.values,rstandard(model_3), xlab = "Fitted values", 
ylab = "Stadardized Residuals",main = "Residual Plot of log(FEV) in Model 3")
with(data, lines(loess.smooth(model_3$fitted.values, rstandard(model_3)), col = "red"))
abline(h = 0)
abline(h = 3)
abline(h = -3)



# Check if there is a need for interaction terms

plot(log(FEV)[data$Sex=="0"]~Age[data$Sex=="0"],
pch = 1,col="red",xlim=c(0,20),ylim=c(0,2), xlab="Age",ylab="log(FEV)")
abline(lm(log(FEV)[data$Sex=="0"]~Age[data$Sex=="0"],data=data),col="red")
par(new=T)
plot(log(FEV)[data$Sex=="1"]~Age[data$Sex=="1"],
pch = 2,col="blue", xlim=c(0,20),ylim=c(0,2), xlab="",ylab="")
abline(lm(log(FEV)[data$Sex=="1"]~Age[data$Sex=="1"],data=data),col="blue")
par(new=F)
legend(14,.5,legend=c("Female", "Male"),
col=c("red", "blue"), pch=1:2, cex=1.2)

plot(log(FEV)[data$Sex=="0"]~Hgt[data$Sex=="0"],
pch = 1,col="red",xlim=c(40,80),ylim=c(0,2), xlab="Hgt",ylab="log(FEV)")
abline(lm(log(FEV)[data$Sex=="0"]~Hgt[data$Sex=="0"],data=data),col="red")
par(new=T)
plot(log(FEV)[data$Sex=="1"]~Hgt[data$Sex=="1"],
pch = 2,col="blue", xlim=c(40,80),ylim=c(0,2), xlab="",ylab="")
abline(lm(log(FEV)[data$Sex=="1"]~Hgt[data$Sex=="1"],data=data),col="blue")
par(new=F)
legend(67,0.4,legend=c("Female", "Male"),
col=c("red", "blue"), pch=1:2, cex=1.2)

plot(log(FEV)[data$Smoke=="0"]~Age[data$Smoke=="0"],
pch = 1,col="purple",xlim=c(0,20),ylim=c(0,2), xlab="Age",ylab="log(FEV)")
abline(lm(log(FEV)[data$Smoke=="0"]~Age[data$Smoke=="0"],data=data),col="purple")
par(new=T)
plot(log(FEV)[data$Smoke=="1"]~Age[data$Smoke=="1"],
pch = 2,col="green", xlim=c(0,20),ylim=c(0,2), xlab="",ylab="")
abline(lm(log(FEV)[data$Smoke=="1"]~Age[data$Smoke=="1"],data=data),col="green")
par(new=F)
legend(12,0.4,legend=c("Non-smoker", "Smoker"),
col=c("purple", "green"), pch=1:2, cex=1.2)

plot(log(FEV)[data$Smoke=="0"]~Hgt[data$Smoke=="0"],
pch = 1,col="purple",xlim=c(40,80),ylim=c(0,2), xlab="Hgt",ylab="log(FEV)")
abline(lm(log(FEV)[data$Smoke=="0"]~Hgt[data$Smoke=="0"],data=data),col="purple")
par(new=T)
plot(log(FEV)[data$Smoke=="1"]~Hgt[data$Smoke=="1"],
pch = 2,col="green", xlim=c(40,80),ylim=c(0,2), xlab="",ylab="")
abline(lm(log(FEV)[data$Smoke=="1"]~Hgt[data$Smoke=="1"],data=data),col="green")
par(new=F)
legend(65,0.4,legend=c("Non-smoker", "Smoker"),
col=c("purple", "green"), pch=1:2, cex=1.2)



# Residual analysis on Model 4

model_4<-lm(log(FEV)~Hgt+Age+Sex+Smoke+Age*Sex+Age*Smoke,data=data)

qqnorm(rstandard(model_4),datax = TRUE, ylab = "Standardized Residuals", xlab = "Z scores", 
main = "Normal Q-Q Plot of log(FEV) in Model 4",pch=20)
qqline(rstandard(model_4),datax = TRUE, col='red')

plot(model_4$fitted.values,rstandard(model_4), xlab = "Fitted values", 
ylab = "Stadardized Residuals",main = "Residual Plot of log(FEV) in Model 4")
with(data, lines(loess.smooth(model_4$fitted.values, rstandard(model_4)), col = "red"))
abline(h = 0)
abline(h = 3)
abline(h = -3)

plot(Hgt,rstandard(model_4), xlab = "Hgt", 
ylab = "Stadardized Residuals",main = "Residual Plot of Hgt in Model 4")
with(data, lines(loess.smooth(Hgt, rstandard(model_4)), col = "red"))
abline(h = 0)
abline(h = 3)
abline(h = -3)

plot(Age,rstandard(model_4), xlab = "Age", 
ylab = "Stadardized Residuals",main = "Residual Plot of Age in Model 4")
with(data, lines(loess.smooth(Age, rstandard(model_4)), col = "red"))
abline(h = 0)
abline(h = 3)
abline(h = -3)

plot(Sex, xlab="Sex",ylab="Frequency",main="Bar Chart of Sex")
plot(Smoke, xlab="Smoke",ylab="Frequency",main="Bar Chart of Smoke")



# Test for multicollinearity using VIF

X<-cbind(Age, Hgt)
X<-cor(X)
C<-solve(X)
VIF <- diag(C)
VIF



# Evaluating all possible regressions

library(olsrr)
model_full<-lm(log(FEV)~Hgt+Age+Sex+Smoke+Age*Sex+Age*Smoke,data=data)
models<-ols_step_all_possible(model_full)
models_cleaned<-models[-c(2:3,
7:8,12:17,19,
22:26,28:29,32:36,38:39,
42:48,52:53,55,57:58,60),] #delete rows where the interaction terms that do not have the first order terms present

model_data<-data.frame(models_cleaned)[,c(1:5)]

names(model_data)[1] = 'Index'
names(model_data)[2] = 'k'
names(model_data)[3] = 'Regressors in Model'
names(model_data)[4] = 'R-squared'
names(model_data)[5] = 'Adjusted R-squared'
model_data["Index"] <- 1:nrow(model_data)
row.names(model_data) <- 1:nrow(model_data)

model_data # 25 possible regressions
head(model_data)

library(gplots)
plot(space(model_data$k,model_data$"R-squared"),
xlab="Number of regressors",ylab="R-squared",
main="R-squared Plot with 'space'")

bestsubsets<-model_data[c(1,5,11,17,22,25),]
bestsubsets



# Variable selection using stepwise regression

model_s<-lm(log(FEV)~ 1, data = data)


model_e<-stepAIC(model_s, direction = "both", 
scope = log(FEV)~Hgt+Age+Sex+Smoke+Age*Sex+Age*Smoke,data=data)

summary(model_e)

summary(model_4)
anova(model_4)
# F-statistic is 1.071429.
1-pf(1.071429,2,647)

anova(model_3,model_4)



# Residual analysis on regressor in Model 3

plot(Hgt,rstandard(model_3), xlab = "Hgt", 
ylab = "Stadardized Residuals",main = "Residual Plot of Hgt in Model 3")
with(data, lines(loess.smooth(Hgt, rstandard(model_3)), col = "red"))
abline(h = 0)
abline(h = 3)
abline(h = -3)

plot(Age,rstandard(model_3), xlab = "Age", 
ylab = "Stadardized Residuals",main = "Residual Plot of Age in Model 3")
with(data, lines(loess.smooth(Age, rstandard(model_3)), col = "red"))
abline(h = 0)
abline(h = 3)
abline(h = -3)



# Test for influential points

cook<-cooks.distance(model_3)
plot(cook, main="Measure of Influence by Cook's Distance")
abline(h = 1)
order(rstandard(model_3)) #the three possible outliers are data points 2,140,473



# Refit model, discarding the three possible outliers

#data_2<-data[-c(2,140,473),] 
#attach(data_2) #reattach data
#model_5<-lm(log(FEV)~Hgt+Age+Sex+Smoke,data=data_2)
#summary(model_5)



# Confidence intervals for categorical terms

qt(0.975,649)
CI_Sex<-cbind(CIlower = 0.028735 - qt(0.975,649) * 0.011755,
CIupper = 0.028735 + qt(0.975,649) * 0.011755)
CI_Sex
CI_Smoke<-cbind(CIlower = -0.047056 - qt(0.975,649) * 0.020962,
CIupper = -0.047056 + qt(0.975,649) * 0.020962)
CI_Smoke
