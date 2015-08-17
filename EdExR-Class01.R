# Load functions
source('functions.R')

# Load the data
df1 = read.csv("D:/Data/msleep_ggplot2.csv")
class(df1)
dim(df1)
df1$sleep_total

#Vectors can be combined using the c function. For example, we can add a final number, 1000, to the sleep totals:
c(df1$sleep_total, 1000)

#Let's go ahead and make a plot of the brain weight and the total sleep, to see what the data look like:
plot(df1$brainwt, df1$sleep_total)

#Once more, with a logarithmic scale x-axis:
plot(df1$brainwt, df1$sleep_total, log="x")
summary(df1)

# There are a number of ways to subset a dataframe or a vector. We can use numbers to indicate the rows we want, 
# or a logical vector which is as long as the number of rows of the dataframe, or as long as the vector. We use the 
# square brackets [] and inside give the rows and columns we want to index, separated by a comma. If we leave out the 
# number before or after the comma, it means, give us all the rows or all the columns.
# Subsetting a dataframe to the first two rows:
df1[ c(1,2), ]

# The rows where the total sleep is greater than 18 hours:
df1[df1$sleep_total > 18, ]

# Subsetting a vector looks very similar, but we just remove the comma (because there are no columns now). The first two elements can be subset like so:
df1$sleep_total[ c(1,2) ]

# What is the average total sleep, using the function mean() and vector subsetting, for the animals with total sleep greater than 18 hours?
lapply(df1[df1$sleep_total > 18, ],mean)

# The function which() gives us the numeric index that satisfies a logical question:
which(df1$sleep_total > 18)

# This can be useful in certain circumstances when we want a number from a logical. For example, if we want to get the 
# first value where the total sleep was more than 18 hours. This combines three operations: 
# which() gives the number of values which have total sleep more than 18 hours, then on the right side, we index this vector with [1] to get the first number. 
# Then we index the original vector with that number. Take a while to look over this and take the command apart to understand what is going on:
df1$sleep_total[ which(df1$sleep_total > 18)[1] ]

#We can also combine two logical vectors and use which to see the rows that satisfy both criteria. 
# Logical vectors are combined using the ampersand symbol: &
# What is the row number of the animal which has more than 18 hours of total sleep and less than 3 hours of REM sleep?
which(df1$sleep_total > 18 & df1$sleep_rem < 3)

# There are three functions which are very important to understand for basic R manipulation of vectors. 
# Trying them out on simple examples will help you understand what each does. sort() is the easiest, so let's start with this one. 
# sort() simply gives back the list of numeric values after sorting them:
sort(df1$sleep_total)

# order() is a bit more complex. order() gives back the index, in the original vector, of the smallest value, then the next smallest, etc. 
# So the last number given back by order() is the index of the largest value in the original vector.
# What is the index of the animal (so the row number) with the least total sleep?
order(df1$sleep_total)
df1$sleep_total[ order(df1$sleep_total) ]

# Finally, let's try rank(). This function takes in numeric values, and turns the smallest value into 1, the second smallest value into 2, etc., and returns 
# the ranks in the same order as the input vector. Ties are resolved by giving the numbers the average of their ranks.
rank(c(1,2,2,3))

# Let's apply rank() to the total sleep column.
# What's the rank of the animal in the first row of the table in terms of total sleep?
rank(df1$sleep_total)

# The match() function in R is useful to find the index of the first match of a vector in a second vector. We can give match() a number of queries at once:
match(c("Cow","Owl monkey","Cheetah"), df1$name)

# This can then be used to rearrange an object. For example, we can rearrange the tab dataframe to an order we specify. Let's reorder the tab dataframe to give 
# the rows for Cow, Owl monkey and Cheetah, using the vector returned by match() as a row index:
idx = match(c("Cow","Owl monkey","Cheetah"), df1$name)
df1[idx,]

# What is the row number for "Cotton rat" in the dataframe?
idx = match(c("Cow","Owl monkey","Cheetah","Cotton rat"), df1$name)
df1[idx,]

# Factors in R are a way to turn character vectors with repeating values into a class of object that recognizes the repeated values. What R is doing internally is 
# keeping track of character values using integers, where the integer refers to the unique values, or "levels" of the factor. But R shows you the character vector when you print the factor. 
# The levels of a factor and their order can be seen using the levels() function. Let's see an example:

vec = c("red","blue","red","green","green","yellow","orange")
fac = factor(vec)
fac
levels(fac)

# You would use character vectors to test for a match, for example, with a factor "vec", if you wanted to find matches for the level "blue", you would write:
vec == "blue"

# The levels are chosen alphabetically, unless we say otherwise:
fac2 = factor(vec, levels=c("blue","green","yellow","orange","red"))
fac2
levels(fac2)

# Some of the columns of our mammal sleep data are factors, and we can use functions like table() on these columns, in order to count the number of repetitions.
# How many rodents (Rodentia) are in the table?
table(df1$order)

# split() is a function which takes a vector and splits it into a list, by grouping the vector according to a factor. Let's use our mammal sleep data again to try this out. 
# Split the total sleep column by the mammals Order (here Order means the biological taxonomy, above Family and below Class)
s = split(df1$sleep_total, df1$order)
s

# We can pull out a single vector from the list using the name of the Order or the number that it occurs in the list (note: this is where the level occurs in the levels of the factor).
# Lists are indexed with double square brackets [[]], instead of a single square bracket []:
s[[17]]
s[["Rodentia"]]

# What is the mean hours of total sleep of the rodents?
mean(s[[17]])

# lapply() and sapply() are useful functions for applying a function repeatedly to a vector or list. lapply() returns a list, while sapply() tries to "simplify", returning a vector 
# if possible (if there is only one element returned by the function for each element of the input. Let's use lapply() to get the average total sleep for each Order:
lapply(s, mean)
sapply(s, mean)

# A shortcut for using split() and sapply() is to straightaway use tapply(), providing these 3 arguments: the vector of values, a factor to group by, and the function to apply:
tapply(df1$sleep_total, df1$order, mean)

# What is the standard deviation of total hours of sleep for the Primates Order? (The standard deviation function is sd() in R.)
tapply(df1$sleep_total, df1$order, sd)












