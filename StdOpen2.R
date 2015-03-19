#Function to clean the data frame
clean <- function(df){
  #Clean the data names
  names(df) <-tolower(names(df))  
  names(df) <- gsub("\\.","",names(df))
  names(df) <- gsub("\\(","",names(df))
  names(df) <- gsub("\\)","",names(df))
  names(df) <- gsub("-","",names(df))
  names(df) <- gsub("_","",names(df))
  names(df) <- gsub(",","",names(df))
  return(df)
}

permwo = function(n, x) {
  ##Ordered without replacement
  return(factorial(n) / factorial(n-x))
}

permw = function(n, x) {
  ##Ordered with replacement
  return (n**x)
}

comb = function(n, x) {
  ##Unordered (sample or subset) Can use choose(n,x)
  return(factorial(n) / (factorial(x) * factorial(n-x)))
}

bday <- function(n) {
  #Birthday probability 1 match birthday
  b <-exp((lfactorial(365) - lfactorial(365-n)) - n*log(365))
  return (1-b)
}

