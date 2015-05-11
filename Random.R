# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

# generate 100 random numbers from a uniform distribution from 1 to 100
# runif(n, min, max)
x <- runif(100,1,100)

# generate 100 random numbers from a norm distribution with a mean and sd
# rnorm(n, mean, sd)
y <- rnorm(100,0,1)

# generate 100 random numbers from a poisson distribution
# rpois(n, lambda)
z <- rpois(100,2)

#From the 100 numbers pick every tenth number
x [seq(from=10,by=10,length=10)]
y [seq(from=10,by=10,length=10)]
z [seq(from=10,by=10,length=10)]
