##Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')

call("UsingR")

data(father.son)

x <- father.son$sheight
n <-length(x)
b <- 10000 ##Bootstrap number of samples 

resamples <- matrix(sample(x,n*b,replace=TRUE),b,n)

medians <-apply (resamples,1,median)

hist(medians,main = paste("Histogram of Medians of \n", b, "Bootstraps" ))
rug(medians)

abline(v=median(x),col="red")

sd(medians)

quantile(medians, c(.025,.975))

abline(v=quantile(medians, c(.025)),col="blue")
abline(v=quantile(medians, c(.975)),col="blue")
