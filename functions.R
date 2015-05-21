# Clear the environment
rm(list=ls())

# Turn off scientific notations for numbers
options(scipen = 999)  
options(digits=8)

# Set locale
Sys.setlocale("LC_ALL", "English") 

# Set seed for reproducibility
set.seed(2345)

##Outliers for regression
outliers <- function(x) {
  e <- (length(x) - 1) / sqrt(length(x)) 
  mad <- function (x, center=median(x), constant=1.4826,
                   low=FALSE, high=FALSE) {
    n <- length(x)
    constant * if ((low || high) && n%%2 == 0) {
      if (low && high) 
        stop("'low' and 'high' cannot be both TRUE")
      n2 <- n%/%2 + as.integer(high)
      sort(abs(x - center), partial = n2)[n2]
    }
    else median(abs(x - center))
  } 
  z <- ( (0.6745 * (x - median(x))) / mad(x) )
  if ( (max(z) > (length(x) - 1) / sqrt(length(x))) == TRUE ) { 
    print(paste( "OUTLIERS DETECTED - ", "Expected-Z: ", round(e,digits=2) , 
                 " Observed-Z: ", round(max(z), digits=2), sep="" )) 
  } else { 
    print(paste( "NO OUTLIERS DETECTED - ", "Expected-Z: ", round(e,digits=2) , 
                 " Observed-Z: ", round(max(z), digits=2), sep="" ))
  }
  ( list(Z=z, Expected=e, Observed=max(z)) )
}

# Std Error
st.err <- function(x, na.rm=FALSE) {
  if(na.rm==TRUE) x <- na.omit(x)
  sd(x)/sqrt(length(x))
}

# clean the data names and data
# Use: df<-cleanit(df)
cleanit <-function(df){
  names(df) <-tolower(names(df))
  names(df) <- gsub("\\(","",names(df))
  names(df) <- gsub("\\)","",names(df))
  names(df) <- gsub("\\.","",names(df))
  names(df) <- gsub("_","",names(df))
  names(df) <- gsub("-","",names(df))
  names(df) <- gsub(",","",names(df))
  return(df)
}

