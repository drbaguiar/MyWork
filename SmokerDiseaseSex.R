##Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')

call("epicalc")
call("survival")
call("graphics")

studydata <- read.csv("smokerdisease.csv")
df <- ftable(table(studydata$smoker, studydata$disease,studydata$female))
##Nonsmokers - Nondisease
df[1,1] #Male Nonsmokers Nondisease
df[1,2] #Female Nonsmokers Nondisease

##Nonsmokers - disease
df[2,1] #Male Nonsmokers Disease
df[2,2] #Female Nonsmokers Disease

##Smokers - Nondisease
df[3,1] #Male Smokers Nondisease
df[3,2] #Female Smokers Nondisease

##Smokers - Disease
df[4,1] #Male Smokers Disease
df[4,2] #Female Smokers Disease

##OVerall results no breakdown for Sex
##csi(exposed-positive,exposed-negative,nonexposed-positive,nonexposed-negative)
csi(df[4,1]+df[4,2],df[3,1]+df[3,2],df[2,1]+df[2,2],df[1,1]+df[1,2])
## Risk Ratio = .93
## Risk difference = -.02

cci(df[4,1]+df[4,2],df[3,1]+df[3,2],df[2,1]+df[2,2],df[1,1]+df[1,2])
## OR = .91

##results for males only
csi(df[4,1],df[3,1],df[2,1],df[1,1])
## Risk Ratio = 1.81
## Risk difference = .08

cci(df[4,1],df[3,1],df[2,1],df[1,1])
## OR = 1.99

##results for females only
csi(df[4,2],df[3,2],df[2,2],df[1,2])
## Risk Ratio = 1.53
## Risk difference = .16

cci(df[4,2],df[3,2],df[2,2],df[1,2])
## OR = 1.99