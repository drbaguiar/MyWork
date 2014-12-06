##Use my standard openning including call function
source('C:/Users/bryan_000/Documents/GitHub/MyWork/StdOpen.R')

#Set name of datafile
datafile <- paste(datadir,"marks.dat",sep = "")

#Check for the File. If not there, download the data 
if (!file.exists(datafile)) {
        url<- "http://astrostatistics.psu.edu/su09/lecturenotes/marks.dat"
        download.file(url, destfile = datafile)
}

dat = read.table(datafile,head=T)

dim(dat)
names(dat)
pc = princomp(~Stat+Phys,dat)
pc$loading
pc

covar = cov(dat)
eig = eigen(covar)
val = eig$values
sqrt(val)
pc

#Set name of datafile
datafile <- paste(datadir,"SDSS_quasar.dat",sep = "")

#Check for the File. If not there, download the data 
if (!file.exists(datafile)) {
        url<- "http://astrostatistics.psu.edu/su09/lecturenotes/SDSS_quasar.dat"
        download.file(url, destfile = datafile)
}

quas = read.table(datafile,head=T)
dim(quas)
names(quas)
quas = na.omit(quas)
dim(quas)

pc2 = princomp(quas[,-1],scores=T)
pc2
plot(pc2)

screeplot(pc2)

screeplot(pc2,type="lines")

pc2$loading[,1:2]

M = pc2$loading[,1:2]
t(M) %*% M #should ideally produce the 2 by 2 identity matrix

plot(pc2$scores[,1],pc2$scores[,2],pch=".")