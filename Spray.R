# Comparison and transformation of batches
# This is for R version 1.3
# with comments for Splus

source('C:/GitHub/MyWork/StdOpen.R')

# this dataset is built into R
data(OrchardSprays)
# for Splus:
# OrchardSprays <- read.table("OrchardSprays.dat")
# data is at www.stat.cmu.edu/~minka/courses/36-350/data/

# randomize the sprays to make it more interesting
t <- levels(OrchardSprays$treatment)
levels(OrchardSprays$treatment) <- sample(t)

# f = unsorted sprays
f <- factor(OrchardSprays$treatment,levels=sample(t))

# x = response variable
x <- OrchardSprays$decrease

stripchart(x ~ f, method="jitter",pch=1,vert=TRUE,col="brown",
           xlab="Spray",ylab="Loss")
# Splus:
# stripplot(f ~ x, jitter=TRUE)   or  plot(f,x)

boxplot(x ~ f, col="bisque",xlab="Spray",ylab="Loss")
# Splus:
# boxplot(split(x, f))

# sort by medians
meds <- tapply(x, f, median)
sf <- factor(f, levels=levels(f)[order(meds)])

boxplot(x ~ sf, col="bisque",xlab="Spray",ylab="Loss")
stripchart(x ~ sf, method="jitter",pch=1,vert=TRUE,col="brown",
           xlab="Spray",ylab="Loss")

# apply transformation
boxplot(log(x) ~ sf, col="bisque",xlab="Spray",ylab="log(Loss)")
# Splus:
# boxplot(split(log(x), sf))
stripchart(log(x) ~ sf, method="jitter",pch=1,vert=TRUE,col="brown", xlab="Spray",ylab="log(Loss)")