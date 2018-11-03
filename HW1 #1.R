#Problem 1, 1.

function_1<-function(){
  x<-runif(1000)
  eps<-rnorm(1000)
  y<-1+2*x+eps
  lm(y~x)
}
library("rsq")
x2<-replicate(1000,rsq(function_1()))
hist_1<-hist(x2,main="Histogram of R^2",xlab="R^2")

x2<-replicate(1000,deviance(function_1()))
hist_2<-hist(x2,main="Histogram of RSS",xlab="RSS")

#Problem 1, 2.
#Notation: n-number of observations per simulation,
# w-number of simulations 
# z-the variance of ϵi

histogram_function<-function(n,w,z,mr,xr,mrss,xrss){
  x<-runif(n)
  eps<-rnorm(n,sd=sqrt(z))
  y<-1+2*x+eps
  lm(y~x)
  hist(replicate(w,rsq(function_1())),main=mr,xlab=xr)
  hist(replicate(w,deviance(function_1())),main=mrss,xlab=xrss)
}
#Example,showing that it works
histogram_function(10,1000,3,"Choose what you want","R^2","Histogram of RSS","Your option")

#Problem 2
setwd("/Users/karenaslanyan/Desktop")
data_1<-read.csv("hw1p22/x1.csv",stringsAsFactors=FALSE)
data_2<-read.csv("hw1p22/x2.csv",stringsAsFactors=FALSE)
data_3<-read.csv("hw1p22/x3.csv",stringsAsFactors=FALSE)
data_4<-read.csv("hw1p22/x4.csv",stringsAsFactors=FALSE)
data_5<-read.csv("hw1p22/x5.csv",stringsAsFactors=FALSE)
data_6<-read.csv("hw1p22/x6.csv",stringsAsFactors=FALSE)
data_7<-read.csv("hw1p22/x7.csv",stringsAsFactors=FALSE)
data_8<-read.csv("hw1p22/x8.csv",stringsAsFactors=FALSE)
data_9<-read.csv("hw1p22/y.csv",stringsAsFactors=FALSE)

  data_11<-merge(data_1,data_2,by=c("id"),all=T)
  data_12<-merge(data_11,data_3,by=c("id"),all=T)
  data_13<-merge(data_12,data_4,by=c("id"),all=T)
  data_14<-merge(data_13,data_5,by=c("id"),all=T)
  data_15<-merge(data_14,data_6 ,by=c("id"),all=T)
  data_16<-merge(data_15,data_7,by=c("id"),all=T)
  data_17<-merge(data_16,data_8,by=c("id"),all=T)
  data<-merge(data_17,data_9,by=c("id"),all=T)  

 rm(data_1,data_2,data_3,data_4,data_5,data_6,data_7,data_8,data_9,data_11,data_12,data_13,data_14,data_15,data_16,data_17)
library(utils)

function_2<-function(){
  comb<-combn(data$x1,data$x2,data$x3,data$x4,data$x5,data$x6,data$x7,data$x8,m=4,FUN=NULL)
  for (i in 1:4){
    x<-comb[i,]
  l<-lm(data$y~x)
  paste(min(rsq(l)),collapse="")
  }
}
comb
data$y 
function_2()

#Problem 3
setwd("/Users/karenaslanyan/Desktop")
data_3<-read.csv("hw1p3.csv", stringsAsFactors = FALSE)
for(i in 1:ncol(data_3)){
  na<-sum(is.na(data_3[,i]))
  na<-as.data.frame(na)
  na$name<-colnames(data_3)[i]
  print(na)
}
sapply(data_3,function(x) sum(is.na(x)))
data_3<-data.table(data_3)
Data_3<-table(is.na(data_3$z))
Data_3
##!!!!!!!!!
#2
data_32<-sample(data_3,)
