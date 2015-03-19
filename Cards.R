##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

#Build a deck of cards
clubs <- (1:13)
diamonds <- (1:13)
hearts <- (1:13)
spades <- (1:13)
deck <-cbind(spades,clubs,hearts,diamonds)

#Average k samples of n size
#number of times to run
k <- 50000
#sample size
n <- 5
cardsamplemeans <-replicate(k,mean(sample(deck,n,replace=TRUE)))

#Summary
summary(cardsamplemeans)
summary(deck)

#Plots
par(mfrow=c(2,2))
boxplot(cardsamplemeans)
boxplot(deck)
hist(cardsamplemeans)
abline(v=mean(cardsamplemeans),col = "red")
hist(deck)
abline(v=mean(deck),col = "red")

#Make a confidence interval
se <- (sd(deck)/sqrt(n))
error <- qt(0.975,df=n-1)*se
left <- mean(cardsamplemeans)-error
right <- mean(cardsamplemeans)+error
left
right
