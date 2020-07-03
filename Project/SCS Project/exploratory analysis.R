setwd("/Users/yunke/Desktop/SCS Meeting")

tensile <- read.csv("Tensile.csv", head = T)
attach(tensile)
class(tensile)
hist(tensile[,4],breaks = 60)
obs <- subset(tensile, tensile$study==1)
expri <- subset(tensile, tensile$study==2)
hist(tensile[,4],breaks=60,main = "Histogram of Elong")

cor(obs[,4:7])
#       Elong        ROA        UTS         YS
#Elong  1.0000000  0.9862429 -0.9770468 -0.9777118
#ROA    0.9862429  1.0000000 -0.9916662 -0.9919909
#UTS   -0.9770468 -0.9916662  1.0000000  0.9996126
#YS    -0.9777118 -0.9919909  0.9996126  1.0000000
cor(expri[,4:7])
#           Elong        ROA        UTS         YS
#Elong  1.0000000  0.2510089 -0.9544293 -0.9559476
#ROA    0.2510089  1.0000000 -0.1878162 -0.1868848
#UTS   -0.9544293 -0.1878162  1.0000000  0.9994518
#YS    -0.9559476 -0.1868848  0.9994518  1.0000000

obs1 <- subset(obs, obs$condition==1) #RTT
obs2 <- subset(obs, obs$condition==2) #ETT
expri1 <- subset(expri, expri$condition==1) #RTT
expri2 <- subset(expri, expri$condition==2) #ETT

cor(obs1[,4:7])
cor(obs2[,4:7])
cor(expri1[,4:7])
cor(expri2[,4:7])

abs(cor(obs1[,4:7]))<abs(cor(obs[,4:7]))
abs(cor(obs2[,4:7]))<abs(cor(obs[,4:7]))
abs(cor(expri1[,4:7]))<abs(cor(expri[,4:7]))
abs(cor(expri2[,4:7]))<abs(cor(expri[,4:7]))

library(ggplot2)
library(plyr)
mu1Elong <- ddply(obs1 , "Machining", summarise, grp.mean=mean(Elong))
mu1ROA <- ddply(obs1 , "Machining", summarise, grp.mean=mean(ROA))
mu1UTS<- ddply(obs1 , "Machining", summarise, grp.mean=mean(UTS))
mu1YS <- ddply(obs1 , "Machining", summarise, grp.mean=mean(YS))

ggplot(obs1, aes(x=Elong, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu1Elong, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")

ggplot(obs1, aes(x=ROA, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu1ROA, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")

ggplot(obs1, aes(x=UTS, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu1UTS, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")

ggplot(obs1, aes(x=YS, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu1YS, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")


mu2Elong <- ddply(obs2 , "Machining", summarise, grp.mean=mean(Elong))
mu2ROA <- ddply(obs2 , "Machining", summarise, grp.mean=mean(ROA))
mu2UTS<- ddply(obs2 , "Machining", summarise, grp.mean=mean(UTS))
mu2YS <- ddply(obs2 , "Machining", summarise, grp.mean=mean(YS))

ggplot(obs2, aes(x=Elong, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu2Elong, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")

ggplot(obs2, aes(x=ROA, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu2ROA, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")

ggplot(obs2, aes(x=UTS, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu2UTS, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")

ggplot(obs2, aes(x=YS, color=Machining)) +
  geom_histogram(fill="white", position="dodge") +
  geom_vline(data=mu2YS, aes(xintercept=grp.mean, color=Machining), linetype="dashed")+
  theme(legend.position="top")

