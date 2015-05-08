library(psych)
data<-c(3.73,0.63, 5.81, 5.13, 0.25, 4.07,NA)
mean(data,na.rm=TRUE)
median(data,na.rm=TRUE)
# REQUIRES psych package
geometric.mean(data,na.rm=TRUE) #not useful if zero in data
harmonic.mean(data,na.rm=TRUE)
