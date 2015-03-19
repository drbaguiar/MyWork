##Use my standard openning including call function
if (Sys.info()["sysname"]=="Linux"){
  source('/home/bryan/GitHub/MyWork/StdOpen.R')     
}else{
  source('C:/GitHub/MyWork/StdOpen.R')   
}
#datafile <-paste(datadir,"gradebook-export.txt",sep = "")
datafile <-paste(datadir,"gradebook-export.CSV",sep = "")
tbl <-read.csv(datafile)

#Clean the tbl
tbl <- clean(tbl)

# remove ungraded work 
df <- subset(tbl, gradingcategory != "Ungraded", 
             select=c(coursename, coursecode,sectionname,assignmenttitle,maxpoints,grade,gradingcategory))


#Tables
##aggregate(response ~ factor1 + factor2, dat, function)
#table(df$coursename,df$gradingcategory)
aggregate(grade~gradingcategory,df,summary)
aggregate(grade~coursecode,df,summary)


