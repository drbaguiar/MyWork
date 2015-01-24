##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
        source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
        source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')   
}

call("plotrix")

##R Base

plot(1:6,ylab="",main="Line Types", xlab="lty 1:6")
ablineclip(v=1, lty=1,col="sienna2", lwd=2)
ablineclip(v=2, lty=2,col="sienna2", lwd=2)
ablineclip(v=3, lty=3,col="sienna2", lwd=2)
ablineclip(v=4, lty=4,col="sienna2", lwd=2)
ablineclip(v=5, lty=5,col="sienna2", lwd=2)
ablineclip(v=6, lty=6,col="sienna2", lwd=2)

plot(lynx)
par(mar=c(5,2,2,5)) ##bottom, left, top, right (default is 5.1 4.1 4.1 2.1)
plot(lynx)

#New - lay a plot over another plot
plot(lynx)
par(new=T)
plot(rivers)
par(new=F)## Default

x<-1:10
plot(x, pch="h")
plot(x,pch=3) ##?points pch values 1 to 25

##Text point size
plot(x, main="Plot of X")
par(ps=5)
plot(x, main="Plot of X")
par(ps=16)

##See the colors
colors()
plot(x, main="Plot of X")

#tics tck=1 turn on grids
plot(lynx,tck=1)

#Xaxs, yaxs axis (r,i,e,s,d)
plot(lynx, xaxs="r")

#xaxt yaxt set to n to not pplot axis
plot(lynx, xaxt="n")

plot(lynx, xlog=T)