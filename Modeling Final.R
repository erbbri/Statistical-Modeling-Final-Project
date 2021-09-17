#Bridget Erb
#June 10th, 2021
#MAT3333 Final

#Data Exploration
cor(fuel[,2:9])
library(car)
plot(fuel[,2:9], pch=20 , cex=1.5)
boxplot(MPG ~ Transmission, data = fuel, xlab="Transmission",
        ylab="MPG")
boxplot(MPG ~ DriveSystem, data = fuel, xlab="Drive System",
        ylab="MPG")
boxplot(MPG ~ FuelType, data = fuel, xlab="Fuel Type",
        ylab="MPG")
#Transformations
fuel$logMPG <- log(fuel$MPG)
fuel$logWeight <- log(fuel$Weight) #not used
fuel$logCylinders <- log(fuel$Cylinders)
fuel$logGears <- log(fuel$Gears) #not used
fuel$logDisplacement <- log(fuel$Displacement)
fuel$logHorsepower <- log(fuel$Horsepower)

#Assessing Plots & Correlations
cor(fuel$Weight,fuel$logMPG)
cor(fuel$Weight,fuel$MPG)
cor(fuel$logWeight,fuel$logMPG)
plot(logMPG ~ Weight, data = fuel)
plot(MPG ~ Weight, data = fuel)

cor(fuel$logCylinders,fuel$logMPG)
cor(fuel$Cylinders,fuel$MPG)
cor(fuel$Cylinders,fuel$logMPG)
plot(logMPG ~ log(Cylinders), data = fuel)
plot(MPG ~ Cylinders, data = fuel)

cor(fuel$logGears,fuel$logMPG)
cor(fuel$Gears,fuel$MPG)
cor(fuel$Gears,fuel$logMPG)
plot(logMPG ~ Gears, data = fuel)
plot(MPG ~ Gears, data = fuel)

cor(fuel$logDisplacement,fuel$logMPG)
cor(fuel$Displacement,fuel$MPG)
cor(fuel$Displacement,fuel$logMPG)
plot(logMPG ~ log(Displacement), data = fuel)
plot(MPG ~ Displacement, data = fuel)

cor(fuel$logHorsepower,fuel$logMPG)
cor(fuel$Horsepower,fuel$MPG)
cor(fuel$Horsepower,fuel$logMPG)
plot(logMPG ~ log(Horsepower), data = fuel)
plot(MPG ~ Horsepower, data = fuel)

#Diagnostic Plots
wm1 <- lm(logMPG ~ Weight, data = fuel)
plot(rstandard(wm1) ~ fitted(wm1))
abline(h=0, lwd=2)
abline(h=c(-3,-2,2,3), col="red", lty=2)
library(car)
qqPlot(resid(wm1))
hist(resid(wm1))
library(ggplot2)
fuel$FuelTypeNum <- as.numeric(fuel$FuelType == "Unleaded")

#Plots for looking for interaction variables
plot(logMPG ~ Gears, data=fuel, pch=1, col=c("blue", "red")[FuelTypeNum+1])
plot(logMPG ~ Weight, data = fuel, col=c("black", "blue", "red", "purple", "green")[fuel$DriveSystem])
ggplot(fuel, aes(x = Weight, y = logMPG, color = Transmission)) +
  geom_point()
ggplot(fuel, aes(x = Weight, y = logMPG, color = DriveSystem)) +
  geom_point()
ggplot(fuel, aes(x = Gears, y = logMPG, color = Transmission)) +
  geom_point()
ggplot(fuel, aes(x = Gears, y = logMPG, color = DriveSystem)) +
  geom_point()

#Modifying categorical variables for interactions
fuel$DriveF <- as.numeric(fuel$DriveSystem == "F")
fuel$TransCVT <- as.numeric(fuel$Transmission == "CVT")

#variable selection using best subsets
library(leaps)
all <- regsubsets(logMPG ~ logDisplacement + 
                  logHorsepower + Weight + TransCVT * Gears, data = fuel)
round(summary(all)$adjr2, 3)
(plot(all, scale="adjr2"))
round(summary(all)$cp,2)
plot(all, scale="Cp")
library(HH)
summaryHH(all)

#final model
finalModel <- lm(logMPG ~ logDisplacement + logHorsepower + Weight + 
                   (TransCVT * Gears), data = fuel)
summary(finalModel)
anova(finalModel)

#diagnostic plots
plot(rstandard(finalModel) ~ fitted(finalModel))
abline(h=0, lwd=2)
abline(h=c(-3,-2,2,3), col="red", lty=2)
qqPlot(resid(finalModel))
hist(resid(finalModel))

#Application: Prediction/Confidence Intervals
predict(finalModel, data.frame(logDisplacement=0.396, logHorsepower=2.245, Weight=3875, TransCVT=0, Gears=6))
predict(finalModel, data.frame(logDisplacement=0.396, logHorsepower=2.245, Weight=3875, TransCVT=0, Gears=6), interval="confidence")
predict(finalModel, data.frame(logDisplacement=0.396, logHorsepower=2.245, Weight=3875, TransCVT=0, Gears=6), interval="prediction")