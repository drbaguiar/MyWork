##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

##The data
yrs <-c(5.8,15,21.5,27.5,33.5,39.5,46,51.5)
cases <-c(0,1,3,8,9,8,10,5)
miners <-c(98,54,43,48,51,38,28,11)

##Make the data frames
df <-data.frame(cbind(yrs,cases,miners))
##or 
#df<-data.frame(yrs=yrs, cases=cases, miners=miners)

cw <-cbind(cases,noncases=miners-cases)

##Log regression
model<-glm(cw~yrs,family=binomial)

##Plotmo
plot(yrs,fitted.values(model))
points(yrs,cases/miners, col="red")

##Trellis plot and scatter diagram
library(lattice)
yrsgroup=equal.count(df$yrs,number=3,overlap=.05)
xyplot(df$cases~df$miners|yrsgroup,data=df)

plot(df$cases~df$miners)