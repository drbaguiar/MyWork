#########################################################################################
#   A simple analysis of the Leekasso vs. the Lasso for prediction.
#   R function Copyright (C) 2011 Simply Statistics and Jeff Leek
#   (http://simplystatistics.tumblr.com, http://twitter.com/leekgroup)
#
#   This program is free software: you can redistribute it and/or modify
#   it under the terms of the GNU General Public License as published by
#   the Free Software Foundation, either version 3 of the License, or
#   (at your option) any later version.
#
#   This program is distributed in the hope that it will be useful,
#   but WITHOUT ANY WARRANTY; without even the implied warranty of
#   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#   GNU General Public License for more details, see <http://www.gnu.org/licenses/>.
#    
#   Depends: lars, fields, sva (Bioconductor)
#  
########################################################################################



require(lars)
require(fields)
require(sva)

lassoAccuracy <- function(y,x,x2){
        l1 = lars(t(x),y)
        cv1 = cv.lars(t(x),y)
        sval = max(0.05,cv1$index[which.min(cv1$cv)])
        p1 = predict.lars(l1,t(x2),s=sval,mode="fraction")
        p2 = predict.lars(l1,t(x2),s=sval,mode="fraction",type="coefficients")
        return(list(acc=mean((p1$fit > 0.5)==y),nonZero=sum(p2$coefficients!=0)))
}

leekassoAccuracy <- function(y,x,x2){
        mod = cbind(rep(1,100),y)
        mod0 = cbind(rep(1,100))
        pValues = f.pvalue(x,mod,mod0)
        index = which(rank(pValues) <= 10)
        
        leekX = t(x[index,])
        leekX2 = t(x2[index,])
        colnames(leekX) = colnames(leekX2) = paste("Column",1:10)
        lm1 = lm(y ~ leekX)
        p2 = predict.lm(lm1,as.data.frame(leekX2))
        return(mean((p2 > 0.5) == y))
}

lassoAcc = leekassoAcc = nonZero = array(NA,dim=c(10,10,10))

set.seed(121202015)
for(n in 1:10){
        for(s in 1:10){
                for(k in 1:10){
                        # Generated the data
                        effects =  rnorm(n*5,sd=s/10)
                        y = rep(c(0,1),each=50)
                        
                        x = matrix(rnorm(500*100),nrow=500)
                        x[1:(n*5),] = x[1:(n*5),] + effects %*% t(y)
                        
                        
                        x2 = matrix(rnorm(500*100),nrow=500)
                        x2[1:(n*5),] = x2[1:(n*5),] +  effects %*% t(y)
                        
                        tmp = lassoAccuracy(y,x,x2)
                        
                        nonZero[k,s,n] = tmp$nonZero
                        lassoAcc[k,s,n] = tmp$acc
                        leekassoAcc[k,s,n] = leekassoAccuracy(y,x,x2)
                        cat(k)
                }
                print(paste("Parameter combination", n, s, "done"))
        }
}


save(lassoAcc,leekassoAcc,nonZero,file="lassodata.rda")


### Create the plots
brks = seq(0.4,1,length=65)
cols = tim.colors(64)

png(file="accuracy-plot.png",height=480,width=2*480)
tmpLasso = apply(lassoAcc,c(2,3),mean)
tmpLeekasso = apply(leekassoAcc,c(2,3),mean)
par(mfrow=c(1,2),mar=c(5,5,5,5))
image(tmpLeekasso,breaks=brks,col=cols,ylab="# of features increasing bottom to top",xlab="Signal increasing left to right",cex.lab=1.5,main="Leekasso Test Accuracy")
image.plot(tmpLasso,breaks=brks,col=cols,ylab="# of features increasing bottom to top",xlab="Signal increasing left to right",cex.lab=1.5,main="Lasso Test Accuracy")
dev.off()