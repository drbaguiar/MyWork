##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
        source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
        source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')   
}

call("MASS")
call("car")
datafile <- paste(datadir,"house-prices.csv",sep = "")
houseprices <-read.csv(file=datafile)

##Prep data
houseprices$brick_d<-ifelse(houseprices$Brick=="Yes",1,0)
houseprices$east<-ifelse(houseprices$Neighborhood=="East",1,0)
houseprices$north<-ifelse(houseprices$Neighborhood=="North",1,0)

##Split Data
set.seed(110)
sub <- sample(nrow(houseprices), floor(nrow(houseprices) * 0.6))
training_data <- houseprices[sub,]
validation_data <- houseprices[-sub,]

##Linear model 1
lm.1 <- lm(Price ~ SqFt, data=training_data)
summary(lm.1)
plot(houseprices$SqFt, houseprices$Price, main="Scatter plot", xlab="Square feet", ylab="Price")
abline(lm.1,col="red",lwd=3)

##Linear Model 2
lm.fit1 <- lm(Price ~ SqFt+Bathrooms+Bedrooms+Offers+north+east+brick_d, data=training_data)
summary(lm.fit1)

##Can we drop
lm.fit1.step <- stepAIC(lm.fit1)
summary(lm.fit1.step)

##Check for multicollinearity LOOK for VIF being less than 10
vif(lm.fit1)

##Predict values on training set
training_data$predict.price <- predict(lm.fit1)
training_data$error <- residuals(lm.fit1)

##Predict values on validation set
validation_data$predict.price <- predict(lm.fit1,newdata=validation_data)
validation_data$error <- validation_data$predict.price - validation_data$Price

##Check residual plots
hist(training_data$error)
hist(validation_data$error)

##Correlation
a<-cor(training_data$Price,training_data$predict.price)
b<-cor(validation_data$Price,validation_data$predict.price)
a*a
b*b
