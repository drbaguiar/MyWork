# Load libraries
call <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}
call("tm")
call("wordcloud")
call("Rstem")
text_dir_1 <- "Set 1"
text_dir_2 <- "Set 2"


# Import Data
raw_data_a<-""
raw_data_b<-""
fun_1 <- function(){
  a<-Corpus(DirSource(paste("Blog_Data/",text_dir_1,sep="")))
  b<-Corpus(DirSource(paste("BLog_Data/",text_dir_2,sep="")))
  assign(deparse(substitute(raw_data_a)), a, envir = .GlobalEnv)
  assign(deparse(substitute(raw_data_b)), b, envir = .GlobalEnv)
}
fun_1()

# Clean data
clean_data<-function(){
  rd2a <- tm_map(raw_data_a, tolower)
  rd2b <- tm_map(raw_data_b, tolower)
  rd2a <- tm_map(rd2a, PlainTextDocument) ## ADDED BMA
  rd2b <- tm_map(rd2b, PlainTextDocument) ##ADDED BMA
  rd2a <- tm_map(rd2a, stripWhitespace)
  rd2b <- tm_map(rd2b, stripWhitespace)
  rd2a <- tm_map(rd2a, removePunctuation)
  rd2b <- tm_map(rd2b, removePunctuation)
  rd2a <- tm_map(rd2a, removeNumbers)
  rd2b <- tm_map(rd2b, removeNumbers)
  rd2a <- tm_map(rd2a, removeWords, stopwords("english"))
  rd2b <- tm_map(rd2b, removeWords, stopwords("english"))
  return(list(rd2a,rd2b))  
}

cleaned_data <- clean_data()

# Term Document matrix

tdm_convert<-function(input_data){
  tdm_a<-TermDocumentMatrix(input_data[[1]])
  tdm_b<-TermDocumentMatrix(input_data[[2]])
  return(list(tdm_a,tdm_b))
}


dtm_convert<-function(input_data){
  dtm_a<-DocumentTermMatrix(input_data[[1]])
  dtm_b<-DocumentTermMatrix(input_data[[2]])
  return(list(dtm_a,dtm_b))
}


word_frequencies <- tdm_convert(cleaned_data)
word_dtm <-dtm_convert(cleaned_data)


m1 <- as.matrix(word_frequencies[[1]])
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)
e1 <- d1[1:30,]
m2 <- as.matrix(word_frequencies[[2]])
v2 <- sort(rowSums(m2),decreasing=TRUE)
d2 <- data.frame(word = names(v2),freq=v2)
e2 <- d2[1:30,]

plot_wordcloud<-function(){
  if(require(RColorBrewer)){
          
    pal <- brewer.pal(6,"Dark2")
    pal <- pal[-(1)]
    #wordcloud(e1$word,e1$freq,c(4,.3),2,,random.order=F,,.15,pal)
    wordcloud(e1$word,e1$freq,pal)
    pal <- brewer.pal(6,"Dark2")
    pal <- pal[-(1)]
    wordcloud(e2$word,e2$freq,c(4,.3),2,,random.order=F,,.15,pal)
    
  }  
}

frequent_words_gen<-function(indata,msg){
  indata$stem <- wordStem(row.names(indata), language = "english")
  
  # anindata put worindatas to column, otherwise they woulindata be lost when aggregating
  indata$worindata <- row.names(indata)
  
  # remove web aindataindataress (very long string):
  indata <- indata[nchar(row.names(indata)) < 20, ]
  
  # aggregate freqeuncy by worindata stem anindata
  # keep first worindatas..
  agg_freq <- aggregate(freq ~ stem, data = indata, sum)
  agg_word <- aggregate(word ~ stem, data = indata, function(x) x[1])
  
  indata <- cbind(freq = agg_freq[, 2], agg_word)
  
  # sort by frequency
  indata <- indata[order(indata$freq, decreasing = T), ]
  
  e<-indata[indata$freq>1,]
  print(msg)
  print(head(e))
  
}

frequent_words<-function(){
  frequent_words_gen(d1,"Set 1 frequently occuring words")
  frequent_words_gen(d2,"Set 2 frequently occuring words")
}

difference_in_words <-function(){
  d1$rank<-d1$freq/sum(d1$freq)
  d2$rank<-d2$freq/sum(d2$freq)
  merge_file <- merge(d1,d2,by.x="word",by.y="word")
  merge_file$diff <- merge_file$rank.x - merge_file$rank.y
  merge_file <- merge_file[with(merge_file, order(diff)), ]
  b<-as.character(head(merge_file[,1],10))
  a<-as.character(tail(merge_file[,1],10))
  cat("Words that occur in set 1 more often\n\n")  
  for(i in 1:10){
    print(a[i])  
  }
  
  cat("\nWords that occur in set 2 more often\n\n")  
  for(i in 1:10){
    print(b[i]) 
  }
  
}


appeared_n_times<-function(N){
  a<-as.character(d1[d1$freq>=N,"word"])
  b<-as.character(d2[d2$freq>=N,"word"])
  cat(paste("Words that occurred at least",N,"times in set 1\n"))
  for(i in 1:N){
    if(!is.na(a[i])){
      print(a[i])   
    }
  }
  cat(paste("Words that occurred at least",N,"times in set 2\n"))
  for(i in 1:N){
    if(!is.na(b[i])){
      print(b[i])   
    }
  }  
}

word_appeared_with <- function(in_word){
  cat("Words associated in Set 1\n")
  findAssocs(word_frequencies[[1]], in_word, 0.6)
  cat("Words associated in Set 2\n")
  findAssocs(word_frequencies[[2]], in_word, 0.6)
  
}

summarize_documents <- function(){
  cat("SET 1 CONTAINS:\n\n")
  print(raw_data_a)
  print(word_frequencies[[1]])
  cat("\nSET 2 CONTAINS:\n\n")
  print(raw_data_b)
  print(word_frequencies[[2]])
}


