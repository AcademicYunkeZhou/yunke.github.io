# Calculate the lengths of different color stripes on the side of the png
rm(list = ls())

install.packages("png")
install.packages("rlist")
install.packages("pracma")
install.packages("base")
install.packages("imager")
install.packages("grDevices")
install.packages("readr")
install.packages("bmp")

library(png)
library(rlist)
library(pracma)
library(base)
library(imager)
library(grDevices)
library(readr)
library(bmp)

setwd("I:\\Pixel Calculator")

# Functions from doBy
which.maxn <- function(x,n=1){
  if (n==1)
    which.max(x)
  else
  {
    if (n>1){
      ii <- order(x,decreasing=TRUE)[1:min(n,length(x))]
      ii[!is.na(x[ii])]
    }
    else {
      stop("n must be >=1")
    }
  }
}



# Data Import Functions
output.mat <- function(){
  my.length <- 100
  my.pixels <- 252
  want.length <- 100
  my.length <- as.numeric(my.length)
  my.pixels <- as.numeric(my.pixels)
  want.length <- as.numeric(want.length)
  i=1
  i1=1
  k=1
  result <- matrix(c(NA,NA,NA,NA))
  row.names(result) <- c("pixels","your length","client length","index")
  f <- function(a){
    repeat{
      if(i1==dim(a)[1]){
        result <- cbind(result,c(i1-i,round((i1-i)*my.length/my.pixels,1),round((i1-i)*want.length/my.pixels,1),1))
        break
      }
      if(all(as.matrix(a[i:i1,i:i1,3])==as.matrix(a[i:i1,i:i1,3])[1,1])==FALSE){
        result <- cbind(result,c(i1-i,round((i1-i)*my.length/my.pixels,1),round((i1-i)*want.length/my.pixels,1),1))
        i <- i1
      }else{ 
        i1 = i1+1
      }
      
    }
    return(result[-4,-1])  
  }
  f(a)
  
}
s <- readPNG("sample.PNG")
whole_plot <- function(result){
  k=1
  whole_result <- matrix(nrow = 3,ncol = 252)
  for (k in 1:length(result[1,])) {
    whole_result[,sum(result[1,1:k-1]):sum(result[1,1:k])] <- result[,k]
  }
  if(is.na(whole_result[1,length(whole_result[1,])])==TRUE){
    whole_result[,length(whole_result[1,])] <- c(1.0,0.4,0.4)
    result <-cbind(result, c(1.0,0.4,0.4))
  }
  return(result)
}

# Color Recognization Functions
t_fun <- function(result){
  t1 <- result[1,]
  t2 <- cumsum(result[1,])
  t <- t2 - 0.5*t1
  t <- t*251/252
  return(t)
}
color_rec <- function(result){
  t1 <- result[1,]
  t2 <- cumsum(result[1,])
  t <- t2 - 0.5*t1
  a1 <- matrix(NA,nrow = length(t2),ncol = 3)
  for(j in 1:length(t2)){
    a1[j,] <- a[t2[j],t2[j],-4]
    
  }
  
  s <- unique(a1)
  a1 <- cbind(a1,NA)
  for (i in 1:length(a1[,1])) {
    v = which(s[,3:3]==a1[i,3])[1]
    a1[i,4] <- LETTERS[v]
  }
  return(a1)
}
color_num <- function(result){
  t1 <- result[1,]
  t2 <- cumsum(result[1,])
  t <- t2 - 0.5*t1
  a1 <- matrix(NA,nrow = length(t2),ncol = 3)
  for(j in 1:length(t2)){
    a1[j,] <- a[t2[j],t2[j],-4]
    
  }
  s <- unique(a1)
  return(s)
}

# Data Import
a <- readPNG("sample.PNG",native = FALSE)
c <- load.image("sample.PNG")
result <- whole_plot(output.mat())

result
# Clean the data
if(result[1,length(result[1,])]==1){
  temp <- sum(result[1,])-result[1,length(result[1,])-1]
  o <- length(a[,1,3])
  z <- length(result[1,])
  
  if(all(as.matrix(a[temp:o,temp:o,3])==as.matrix(a[temp:o,temp:o,3])[1,1])){
  result[,z-1] <- result[,z-1] + result[,z]
  result <- result[,-z]
  }
}
w <- 90
if(sum(result[3,])-w!=0){
  dif <- w-sum(result[3,])
  if(dif<0){
    result[3,which.maxn(result[3,],(abs(dif)/0.1))]=result[3,which.maxn(result[3,],(abs(dif)/0.1))]-0.1
  }else{
    result[3,which.maxn(result[3,],(abs(dif)/0.1))]=result[3,which.maxn(result[3,],(abs(dif)/0.1))]+0.1
  }
  
}


result
sum(result[3,])


# Deal with data
t <- t_fun(result)
a1 <- color_rec(result)
l <- color_num(result)
l <- cbind(l,LETTERS[1:length(l[,1])])
y <- rgb(unique(a1)[,1:3])
line <- round(as.numeric(a1[,1])*100,0)
line <- as.numeric(rep(c(10,20,30),length(line)/3+1))[1:length(line)]

# Check the results
a1
l
y
line





#Plot!
png("RESULT-1.png",width=10,height=10,units="in",res=1000)
layout(matrix(c(1,1,1,1, 1,1,1,1, 1,1,1,1, 2,2,2,2), nrow = 4, ncol = 4, byrow = TRUE))

# Backgroud Color Setting
# par(bg=rgb(0.2,0.8,0.9))
# par(bg="white")

plot(c,axes=FALSE)
segments(x0=-line,y0=t+1,x1=-1,y1=t+1,col="black")
text(x=-line/1-2,y = t-2.1,xpd = T,label = result[3,], pos = 1, cex = 0.58, col = "blue")
text(x=-line/1-5,5,y = t-2.1,xpd = T,label = a1[,4], pos = 1, cex = 0.58, col = "black")

# R暂时无法输出中文字符，此条待改进
text(x=120,y = 230,xpd = T,label = paste0("90mm      ","\n","横竖格距相同      ","\n","2020版潘通色卡      "),
     pos = 1, cex = 1.5, col = "black")

# Color legend


# Edit the color specs here
barplot(rep(1,(length(l[,1]))),
        names.arg = c("A 11-1404 TPG",
                      "B 18-1629 TPG",
                      "C 16-5412 TPG",
                      "D 14-4318 TPG",
                      "E 18-1652 TPG"), 
        col=y, axes = FALSE)

dev.off()

closeAllConnections()


