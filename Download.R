##Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')

##Set destination file for dowload 
datafile <-paste(datadir, "evals.RData",sep = "")

#Check for file and download if needed
if(!file.exists(datafile)){
        download.file("http://www.openintro.org/stat/data/evals.RData",destfile = datafile)
        
}

load("evals.RData")

##sPLIT THE DATA
splits <-dfsplit(evals)
df.train <-splits$trainset
df.test <-splits$testset