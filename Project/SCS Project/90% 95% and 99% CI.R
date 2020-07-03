#3 CI - 90%,95%,99%
m1=mean(previous_elong)
m2=mean(new_elong)
sd1=sd(previous_elong)
sd2=sd(new_elong)
n1=length(previous_elong)
n2=length(new_elong)
low_eqbound=-abs(mean(previous_elong)-mean(new_elong))/2
high_eqbound=abs(mean(new_elong)-mean(previous_elong))/2
var.equal = TRUE #use FALSE if two sample variance are NOT equal
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

plot(NA, ylim=c(0,1), xlim=c(min(LL90,low_eqbound)-max(UL90-LL90, high_eqbound-low_eqbound)/10, max(UL90,high_eqbound)+max(UL90-LL90, high_eqbound-low_eqbound)/10), bty="l", yaxt="n", ylab="",xlab="Mean Difference")
points(x=dif, y=0.5, pch=15, cex=2)
abline(v=0, lty=2, col="grey")
abline(v=high_eqbound, lty=2)
abline(v=low_eqbound, lty=2)
segments(LL99,0.5,UL99,0.5, lwd=3, col=gray(0.8))
segments(LL95,0.5,UL95,0.5, lwd=4, col=gray(0.5))
segments(LL90,0.5,UL90,0.5, lwd=5, col=gray(0.0))
title(main=paste("90%, 95%, and 99% CI", "\n Mean difference = ",round(dif,digits=3),"\n 90% CI [",round(LL90,digits=3),";",round(UL90,digits=3),"]" , "\n 95% CI [", round(LL95,digits=3),";",round(UL95,digits=3),"]", "\n 99% CI [",round(LL99,digits=3),";",round(UL99,digits=3),"]",sep=""),cex.main=0.8)
#NOTE: the 90% CI is the narrowest one while the 99% CI is the widest one.