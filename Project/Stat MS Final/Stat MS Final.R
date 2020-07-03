library(tidyverse)
library(tidyr)
library(lme4)
library(readxl)
library(pwr)
library(MuMIn)
library(ggplot2)
library(ggthemes)
library(stats)
library(corpcor)
library(GGally)
library(mctest)
library(car)
library(olsrr)
options(digits = 2)

#Q1
Informational <- rnorm(50,7.2,0.3)
Colloquial <- rnorm(50,6.8,0.5)
Message_Strategy <- cbind(Informational,Colloquial)
colnames(Message_Strategy) <- c("Informational","Colloquial")


boxplot.matrix(Message_Strategy,col = c("pink4", "orange2"),xlab = "Message Strategy",ylab = "Ratings",
               main = "Message Strategy Effect",cex.main = 1.6,cex.axis = 1.2,cex.lab = 1.4)

x<-read.csv(file = "C:\\Users\\victo\\Desktop\\Q1.csv", header = T)

interaction.plot(x$Message_Relatability,x$Message_Strategy,x$Measurement,
                 xlab = "Message Relatability", ylab = "Ratings", trace.label = "Message Strategy",col = c("blue4", "red4"),
                 cex.axis = 1.5,cex.lab = 1.5,lty = 1,lwd = 3,fixed = TRUE,main = "Interaction Plot of Messaage Strategy & Message Relatability")

ggplot(x, aes(x=Message_Strategy, y=Measurement, fill=Message_Relatability)) + 
  geom_boxplot() + labs(title = "Main Effect of Message Strategy") + ylab("Ratings") + xlab("Message Strategy") + 
  theme(axis.text=element_text(size=15),
        axis.title=element_text(size=18,face="bold")) +
  theme(plot.title = element_text(size=22,face="bold"))

#Q2
ab <- read.csv(file = "C:\\Users\\victo\\Desktop\\abalone.csv", header = T)

#Normality analysis
par(mfrow=c(3,3))
qqnorm(ab$Length, pch = 1, frame = FALSE,main = "Length")
qqline(ab$Length, col = "steelblue", lwd = 1)
qqnorm(ab$Diameter, pch = 1, frame = FALSE,main = "Diameter")
qqline(ab$Diameter, col = "steelblue", lwd = 1)
qqnorm(ab$Height, pch = 1, frame = FALSE,main = "Height")
qqline(ab$Height, col = "steelblue", lwd = 1)
qqnorm(ab$Whole, pch = 1, frame = FALSE,main = "Whole")
qqline(ab$Whole, col = "steelblue", lwd = 1)
qqnorm(ab$Shucked, pch = 1, frame = FALSE,main = "Shucked")
qqline(ab$Shucked, col = "steelblue", lwd = 1)
qqnorm(ab$Viscera, pch = 1, frame = FALSE,main = "Viscera")
qqline(ab$Viscera, col = "steelblue", lwd = 1)
qqnorm(ab$Shell, pch = 1, frame = FALSE,main = "Shell")
qqline(ab$Shell, col = "steelblue", lwd = 1)
qqnorm(ab$Rings, pch = 1, frame = FALSE,main = "Rings")
qqline(ab$Rings, col = "steelblue", lwd = 1)

#Delete Outliers
ab <- ab[-c(2052,1418),]
par(mfrow=c(3,3))
qqnorm(ab$Length, pch = 1, frame = FALSE,main = "Length")
qqline(ab$Length, col = "steelblue", lwd = 1)
qqnorm(ab$Diameter, pch = 1, frame = FALSE,main = "Diameter")
qqline(ab$Diameter, col = "steelblue", lwd = 1)
qqnorm(ab$Height, pch = 1, frame = FALSE,main = "Height")
qqline(ab$Height, col = "steelblue", lwd = 1)
qqnorm(ab$Whole, pch = 1, frame = FALSE,main = "Whole")
qqline(ab$Whole, col = "steelblue", lwd = 1)
qqnorm(ab$Shucked, pch = 1, frame = FALSE,main = "Shucked")
qqline(ab$Shucked, col = "steelblue", lwd = 1)
qqnorm(ab$Viscera, pch = 1, frame = FALSE,main = "Viscera")
qqline(ab$Viscera, col = "steelblue", lwd = 1)
qqnorm(ab$Shell, pch = 1, frame = FALSE,main = "Shell")
qqline(ab$Shell, col = "steelblue", lwd = 1)
qqnorm(ab$Rings, pch = 1, frame = FALSE,main = "Rings")
qqline(ab$Rings, col = "steelblue", lwd = 1)


#Complete model
mean(ab$Height)
max(ab$Height)
m1 <- glm(Rings ~ .,data=ab[2:9])
summary(m1)

#VIF check
vif(lm(Rings ~Length+Diameter+Height+Whole+Shucked+Viscera+Shell,ab))
vif(lm(Rings ~Length+Diameter+Height+Shucked+Viscera+Shell,ab))
vif(lm(Rings ~Length+Height+Shucked+Viscera+Shell,ab))
vif(lm(Rings ~Length+Height+Shucked+Shell,ab))
vif(lm(Rings ~Height+Shucked+Shell,ab))

#Signicicance Check
m4 <- glm(Rings ~ (Length+Height+Shucked+Shell)^2,data=ab)
summary(m4)

#Correlation Matrix
cor_mat <- cor2pcor(cov(cbind(ab$Height,ab$Shucked,ab$Shell)))
colnames(cor_mat) <- c("Height","Shucked","Shell")
rownames(cor_mat) <- c("Height","Shucked","Shell")
cor_mat

#Models
lm1<-lm(Rings~.,ab[2:9])
summary(lm1)

lm2<-lm(log(Rings) ~ Length + Height + Shucked + Shell + Length:Shucked + Length:Shell + (1|ID), ab)
summary(lm2)

#Plot the model
par(mfrow=c(2,2))
plot(lm1)
plot(lm2)

#Predict for all sexs
par(mfrow=c(1,2))
new <- data.frame(
  Height = ab$Height, Shucked = ab$Shucked, Shell = ab$Shell, ID = ab$ID,
  Whole = ab$Whole, Length = ab$Length, Diameter = ab$Diameter, Viscera = ab$Viscera
)

plot(exp(predict(lm2,new)),ab$Rings,
     xlab="predicted",ylab="actual",main = "Predict for All Sexs before Log Transform")
abline(a=0,b=1)

plot(predict(lm2,new),log(ab$Rings),
     xlab="predicted",ylab="actual",main = "Predict for All Sexs after Log Transform")
?abline
#Divide by sex
ab.M <- ab[which(ab$Sex=="M"),]
ab.I <- ab[which(ab$Sex=="I"),]
ab.F <- ab[which(ab$Sex=="F"),]
qqnorm(ab.M$Rings, pch = 1, frame = FALSE,main = "Rings")
qqline(ab.M$Rings, col = "steelblue", lwd = 1)
qqnorm(ab.I$Rings, pch = 1, frame = FALSE,main = "Rings")
qqline(ab.I$Rings, col = "steelblue", lwd = 1)
qqnorm(ab.F$Rings, pch = 1, frame = FALSE,main = "Rings")
qqline(ab.F$Rings, col = "steelblue", lwd = 1)
#Predict for M
new.M <- data.frame(
  Height = ab.M$Height, Shucked = ab.M$Shucked, Shell = ab.M$Shell, ID = ab.M$ID,
  Whole = ab.M$Whole, Length = ab.M$Length, Diameter = ab.M$Diameter, Viscera = ab.M$Viscera
)

plot(exp(predict(lm2,new.M)),ab.M$Rings,
     xlab="predicted",ylab="actual",main = "Predict for Sex M before Log Transform")
abline(a=0,b=1)

plot(predict(lm2,new.M),log(ab.M$Rings),
     xlab="predicted",ylab="actual",main = "Predict for Sex M after Log Transform")
abline(a=0,b=1)

#Predict for I
new.I <- data.frame(
  Height = ab.I$Height, Shucked = ab.I$Shucked, Shell = ab.I$Shell, ID = ab.I$ID,
  Whole = ab.I$Whole, Length = ab.I$Length, Diameter = ab.I$Diameter, Viscera = ab.I$Viscera
)

plot(exp(predict(lm2,new.I)),ab.I$Rings,
     xlab="predicted",ylab="actual",main = "Predict for Sex I before Log Transform")
abline(a=0,b=1)

plot(predict(lm2,new.I),log(ab.I$Rings),
     xlab="predicted",ylab="actual",main = "Predict for Sex I after Log Transform")
abline(a=0,b=1)

#Predict for F
new.F <- data.frame(
  Height = ab.F$Height, Shucked = ab.F$Shucked, Shell = ab.F$Shell, ID = ab.F$ID,
  Whole = ab.F$Whole, Length = ab.F$Length, Diameter = ab.F$Diameter, Viscera = ab.F$Viscera
)

plot(exp(predict(lm2,new.F)),ab.F$Rings,
     xlab="predicted",ylab="actual",main = "Predict for Sex F before Log Transform")
abline(a=0,b=1)

plot(predict(lm2,new.F),log(ab.F$Rings),
     xlab="predicted",ylab="actual",main = "Predict for Sex F after Log Transform")
abline(a=0,b=1)







