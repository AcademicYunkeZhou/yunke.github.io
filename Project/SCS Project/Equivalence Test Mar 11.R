setwd("/Users/yunke/Desktop/SCS Meeting") #please set the workpath to the folder including the data files

#You can delete the "#" in the following lines to make them run
#install.packages("TOSTER")
#install.packages("lme4")
#install.packages("Matrix")
#install.packages("stats")
#install.packages("multcomp")

rm(list=ls())  #run this line without the first "#" to clear all the data in the environment

library(TOSTER)
library(Matrix)
library(lme4)
library(ggplot2)
library(stats)


#Import Data
previous <- data.frame(read.csv("previous.csv"),header=TRUE)
new <- data.frame(read.csv("new.csv",header = TRUE))
previous_size <- as.numeric(previous$size)
previous_heat <- as.factor(previous$HEAT)
previous_method <- as.factor(previous$method)
previous_elong <- as.numeric(previous$Elong)
previous_ROA <- as.numeric(previous$ROA)
previous_UTS <- as.numeric(previous$UTS)
previous_YS <- as.numeric(previous$YS)


new_size <- as.numeric(new$size)
new_heat <- as.factor(new$HEAT)
new_method <- as.factor(new$method)
new_elong <- as.numeric(new$Elong)
new_ROA <- as.numeric(new$ROA)
new_UTS <- as.numeric(new$UTS)
new_YS <- as.numeric(new$YS)

# CI - 90%,95%,99%
m1=mean(previous_elong)
m2=mean(new_elong)
sd1=sd(previous_elong)
sd2=sd(new_elong)
n1=length(previous_elong)
n2=length(new_elong)
low_eqbound=--0.1 # use the desired bound
high_eqbound=0.1

#Using elong as example(The other parts are at the end of the script)

#New Equivalence Test function
eq.test <- function(m1,m2,sd1,sd2,n1,n2,low_eqbound,high_eqbound,var.equal)
  {var.equal = TRUE #use FALSE if two sample variance are NOT equal

  #define alpha
  alpha=.05
  #calculate sd pooled and degree of freedom.
  #NOTE: it depends on whether the variances are equal or not
  if(var.equal==TRUE) {
    sdpooled<-sqrt((((n1 - 1)*(sd1^2)) + (n2 - 1)*(sd2^2))/((n1+n2)-2))
    degree_f<-n1+n2-2
  }else{
    sdpooled<-sqrt((sd1^2 + sd2^2)/2) 
    degree_f<-(sd1^2/n1+sd2^2/n2)^2/(((sd1^2/n1)^2/(n1-1))+((sd2^2/n2)^2/(n2-1)))
  }
  #difference between means
  dif<-(m1-m2)
  #90% CI
  LL90<-(m1-m2)-qt(1-alpha, degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
  UL90<-(m1-m2)+qt(1-alpha, degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
  #95% CI
  LL95<-(m1-m2)-qt(1-(alpha/2), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
  UL95<-(m1-m2)+qt(1-(alpha/2), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
  #99% CI
  LL99<-(m1-m2)-qt(1-(alpha/10), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
  UL99<-(m1-m2)+qt(1-(alpha/10), degree_f)*(sdpooled*sqrt(1/n1 + 1/n2))
  
  #plot the result
  plot(NA, ylim=c(0,1), xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/10, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/10), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
  points(x=dif, y=0.5, pch=15, cex=2)
  abline(v=0, lty=2, col="grey")
  abline(v=high_eqbound, lty=2)
  abline(v=low_eqbound, lty=2)
  segments(LL99,0.5,UL99,0.5, lwd=3, col=gray(0.8))
  segments(LL95,0.5,UL95,0.5, lwd=4, col=gray(0.5))
  segments(LL90,0.5,UL90,0.5, lwd=5, col=gray(0.0))
  title(main=paste("90%, 95%, and 99% CI", " Mean difference = ",round(dif,digits=3),
                   "\n 90% CI [",round(LL90,digits=3),",",round(UL90,digits=3),"]" ,
                   "\n 95% CI [",round(LL95,digits=3),",",round(UL95,digits=3),"]",
                   "\n 99% CI [",round(LL99,digits=3),",",round(UL99,digits=3),"]",
                   "\n Equivalence bound [",round(low_eqbound,digits=3),",",round(high_eqbound,digits=3),"]",sep=""),cex.main=0.8)
  #NOTE: the 90% CI is the narrowest one while the 99% CI is the widest one.
}

#Try the function
eq.test(m1 = 11.15898,m2 = 11.1069,
        sd1 = 0.7538519,sd2 = 0.8044879,
        n1 = 176,n2 = 29,
        low_eqbound = -0.1,
        high_eqbound = 0.1,
        var.equal = FALSE)
#you can plot about the other factors by change the variable names above





#Belowing is the original version of the equivalence test function, it's 
#not that clear than the new one.
#it generates the 3 intervals and plot them on the graph, which is probably
#chaos for showing
eq.original.test <- function(m1,m2,sd1,sd2,n1,n2,low_eqbound,high_eqbound,var.equal)
  {
  CL.90.elong <- TOSTER::TOSTtwo.raw(m1=mean(previous_elong),
                                     m2=mean(new_elong),
                                     sd1=sd(previous_elong),
                                     sd2=sd(new_elong),
                                     n1=length(previous_elong),
                                     n2=length(new_elong),
                                     low_eqbound=-0.1,
                                     high_eqbound=0.1,
                                     alpha = 0.1, var.equal=FALSE,
                                     plot = FALSE)
  CL.95.elong <- TOSTER::TOSTtwo.raw(m1=mean(previous_elong),
                                     m2=mean(new_elong),
                                     sd1=sd(previous_elong),
                                     sd2=sd(new_elong),
                                     n1=length(previous_elong),
                                     n2=length(new_elong),
                                     low_eqbound=-0.1,
                                     high_eqbound=0.1,
                                     alpha = 0.05, var.equal=FALSE,
                                     plot = FALSE)
  CL.99.elong <- TOSTER::TOSTtwo.raw(m1=mean(previous_elong),
                                     m2=mean(new_elong),
                                     sd1=sd(previous_elong),
                                     sd2=sd(new_elong),
                                     n1=length(previous_elong),
                                     n2=length(new_elong),
                                     low_eqbound=-0.1,
                                     high_eqbound=0.1,
                                     alpha = 0.01, var.equal=FALSE,
                                     plot = TRUE)
  #Sketch the Confidence Interval
  abline(v=CL.90.elong[12],lty=3)
  abline(v=CL.90.elong[13],lty=3)
  abline(v=CL.95.elong[12],lty=4)
  abline(v=CL.95.elong[13],lty=4)
  abline(v=CL.99.elong[12],lty=5)
  abline(v=CL.99.elong[13],lty=5)  
  
  legend("topleft", c("90% CL","95% CL","99% CL"), 
         col = "black",lty = 3:5,
         box.col = "black",pt.cex = 1,cex = 0.8
  )
}

#Try the function
eq.original.test(m1 = 11.15898,m2 = 11.1069,
                 sd1 = 0.7538519,sd2 = 0.8044879,
                 n1 = 176,n2 = 29,
                 low_eqbound = -0.1,
                 high_eqbound = 0.1)

#you can plot about the other factors by change the variable names above
?TOSTtwo.raw
