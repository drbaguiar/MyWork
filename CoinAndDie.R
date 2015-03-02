##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}

Flip1Coin <-function(n) {
  sample(0:1,n,rep=T)
}

Roll1Die <-function(n){
  sample(1:6,n,rep=T)
}

n <- 100
OneCoin <- Flip1Coin(n)
TwoCoin <- Flip1Coin(n) + Flip1Coin(n)
ThreeCoin <- Flip1Coin(n) + Flip1Coin(n)+  Flip1Coin(n)
FourCoin <- Flip1Coin(n) + Flip1Coin(n)+  Flip1Coin(n) + Flip1Coin(n)
FiveCoin <- Flip1Coin(n) + Flip1Coin(n)+Flip1Coin(n) + Flip1Coin(n)+ Flip1Coin(n)

table(OneCoin)
table(TwoCoin)
table(ThreeCoin)
table(FourCoin)
table(FiveCoin)

hist(OneCoin)
hist(TwoCoin)
hist(ThreeCoin)
hist(FourCoin)
hist(FiveCoin)

OneCoinRun <-rle(OneCoin)
tapply(OneCoinRun$lengths, OneCoinRun$values, max)
TwoCoinRun <-rle(TwoCoin)
tapply(TwoCoinRun$lengths, TwoCoinRun$values, max)
ThreeCoinRun <-rle(ThreeCoin)
tapply(ThreeCoinRun$lengths, ThreeCoinRun$values, max)
FourCoinRun <-rle(FourCoin)
tapply(FourCoinRun$lengths, FourCoinRun$values, max)
FiveCoinRun <-rle(FiveCoin)
tapply(FiveCoinRun$lengths, FiveCoinRun$values, max)



OneDie <- Roll1Die(n)
TwoDie <- Roll1Die(n) +Roll1Die(n)
ThreeDie <- Roll1Die(n)+Roll1Die(n)+Roll1Die(n)
FourDie <-Roll1Die(n)+Roll1Die(n)+Roll1Die(n)+Roll1Die(n)
FiveDie <-Roll1Die(n)+Roll1Die(n)+Roll1Die(n)+Roll1Die(n)+Roll1Die(n)
SixDie <-Roll1Die(n)+Roll1Die(n)+Roll1Die(n)+Roll1Die(n)+Roll1Die(n)+Roll1Die(n)


table(OneDie)
table(TwoDie)
table(ThreeDie)
table(FourDie)
table(FiveDie)
table(SixDie)

hist(OneDie)
hist(TwoDie)
hist(ThreeDie)
hist(FourDie)
hist(FiveDie)
hist(SixDie)

##Fast running loop
bar = seq(1,200000, by=2)
bar.squared = rep(NA, 200000)

for (i in 1:length(bar) ) {
  bar.squared[i] = bar[i]^2
}

#get rid of excess NAs
bar.squared = bar.squared[!is.na(bar.squared)]
summary(bar.squared)