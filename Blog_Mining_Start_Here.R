############################################

### Before you are able to use the app   
### you need to do a little housekeeping.

############################################



###################################################
# STEP 1
# Specify which folder you have extracted the zip
# file you downloaded
#
# Example: 
# working_directory <- "D:/R/practice"

###################################################

working_directory <- "C:/Users/bryan_000/Documents/GitHub/Data"


setwd(working_directory)

source("C:/Users/bryan_000/Documents/GitHub/MyWork/blog_mining.R")


###################################################
# STEP 2
# Execute the code
#
# Highlight the 3 lines in STEP 1 and
# press Ctrl + Enter

###################################################





###################################################
# STEP 3
# Specify which tools you want to use
#
# Remove the "#" symbol on the lines you are
# interested in running
#
# To execute, highlight the line you want and
# press Ctrl + Enter

###################################################

###################################################
# Remove the '#' symbol from the next line to create 
# a summary of text documents imported

summarize_documents()

###################################################
# Remove the '#' symbol from the next line to plot
# a wordlcloud

plot_wordcloud()

###################################################
# Remove the '#' symbol to print a report of 5 most
# frequent terms in the two sets of documents

frequent_words()

###################################################
# Remove the '#' symbol to print a report of words that
# are ranked higher in one document than the other

difference_in_words()

###################################################
# Remove the '#' symbol and enter a number in the ()
# to print a report of words that occurred at least "N" times

appeared_n_times(20)


