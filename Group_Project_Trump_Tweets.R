install.packages('tm')
install.packages("SnowballC")
install.packages("wordcloud")
install.packages("wordcloud2")
install.packages('RGraphics')
install.packages("BiocManager")
BiocManager::install("Rgraphviz")
install.packages('tidyverse')
install.packages('tidytext')
install.packages('textclean')
install.packages('tokenzers')
install.packages('markovchain')

library(tm)
library(SnowballC)
library(ggplot2)
library(wordcloud)
library(wordcloud2)
library(RGraphics)
library(Rgraphviz)
library(factoextra)
library(tidyverse)
library(tidytext)
library(textclean)
library(tokenzers)
library(markovchain)
library(dplyr)


setwd('~/Documents/UMGC/DATA630/Group Project')

text <- read.csv('Donald-Tweets!.csv', stringsAsFactors = T)

# removing links
my_text <- subset(text, text$Type != 'link')

# combining into a long string
tweet_text <- paste(my_text$Tweet_Text, collapse = " ")
# vectorizing
tweetVector <- VectorSource(tweet_text)

# creating a corpus
corpus <- Corpus(tweetVector)

# cleaning up text

#convert corpus into lowercase
corpus <- tm_map(corpus, content_transformer(tolower))

# #remove punctuation
# corpus <- tm_map(corpus, removePunctuation)
# 
# #remove numbers
# corpus <- tm_map(corpus, removeNumbers)

Textprocessing <- function(x)
{ gsub("http[[:alnum:]]*",'', x)
  gsub('http\\S+\\s*', '', x) ## Remove URLs
  gsub('\\b+RT', '', x) ## Remove RT
  gsub('#\\S+', '', x) ## Remove Hash tags
  gsub('@\\S+', '', x) ## Remove Mentions
  gsub('[[:cntrl:]]', '', x) ## Remove Controls and special characters
  gsub("\\d", '', x) ## Remove Controls and special characters
  gsub('[[:punct:]]', '', x) ## Remove Punctuation
  gsub("^[[:space:]]*","",x) ## Remove leading white spaces
  gsub("[[:space:]]*$","",x) ## Remove trailing white spaces
  gsub(' +',' ',x) ## Remove extra white spaces
  # gsub(" ?(f|ht)(tp)(s?)(://)(.)[.|/](.)", '', x)
  # gsub("(?:\\s*#\\w+)+\\s*$", "", x)
}

Processing <- function(x) {
  gsub("&amp", "", x)
  gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", x)
  gsub("@\\w+", "", x)
  gsub("[[:punct:]]", "", x)
  gsub("[[:digit:]]", "", x)
  gsub("http\\w+", "", x)
  gsub("[ \t]{2,}", "", x)
  gsub("^\\s+|\\s+$", "", x)
}


 
# corpus <- tm_map(corpus, Processing)
corpus <- tm_map(corpus, Textprocessing)

# remove extra whitespace
corpus <- tm_map(corpus, stripWhitespace)


mystopwords <- c(stopwords("english"),"rt","íí","get",
                 "like","just","yes","know","will","good","day","people", "trump",
                 "realdonaldtrump", "makeamericagreatagain",
                 "great", "america", "make", "again", "thank", "amp", 'donald')



#remove stopwords
corpus <- tm_map(corpus,removeWords,mystopwords)

corpus <- tm_map(corpus, stripWhitespace)

#copy of corpus
corpus_copy <- corpus
#stem words
corpus_copy <- tm_map(corpus,stemDocument)


corpus_copy <- tm_map(corpus, stemCompletion, dictionary=corpus_copy)
corpus <- Corpus(VectorSource(corpus))
corpus <- iconv(x = corpus,"latin1","ASCII",sub = "")

tdm <- TermDocumentMatrix(corpus_copy,control = list(wordlengths = c(1,Inf)))
tdm

freq.terms <- findFreqTerms(tdm, lowfreq =  50)


termFreq <- rowSums(as.matrix(tdm))
termFreq <- subset(termFreq, termFreq >=200)

df <- data.frame(term = names(termFreq), freq = termFreq)

ggplot(df,aes(x = reorder(df$term, +df$freq), y = freq, fill=df$freq)) + geom_bar(stat = "identity") +
  scale_colour_gradientn(colors = terrain.colors(10)) + xlab("Terms") + ylab("Count") + coord_flip()

m <- as.matrix(tdm)


#calculate the frequency of words as sort it by frequency
word.freq <- sort(rowSums(m), decreasing = F)

wordcloud2(df, color = "random-dark", backgroundColor = "white", shape = 'circle')



dtm <- TermDocumentMatrix(corpus)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v1)

dtm1 <- TermDocumentMatrix(corpus_copy)
m1 <- as.matrix(dtm1)
v1 <- sort(rowSums(m1),decreasing=TRUE)
d1 <- data.frame(word = names(v1),freq=v1)



textplot::textplot_bar(d1, top = 10, main = "Top 10 Most Frequent Words", xlab = 'Frequency', ylab = 'Word')
textplot::textplot_bar(d1, top = 15, main = "Top 15 Most Frequent Words", xlab = 'Frequency', ylab = 'Word')
textplot::textplot_bar(d1, top = 20, main = "Top 20 Most Frequent Words", xlab = 'Frequency', ylab = 'Word')

textplot::textplot_bar(d, top = 10, main = "Top 10 Most Frequent Words", xlab = 'Frequency', ylab = 'Word')
textplot::textplot_bar(d, top = 15, main = "Top 15 Most Frequent Words", xlab = 'Frequency', ylab = 'Word')
textplot::textplot_bar(d, top = 20, main = "Top 20 Most Frequent Words", xlab = 'Frequency', ylab = 'Word')


set.seed(1234)
# cardioid
wordcloud2(d1, color = "random-light", backgroundColor="black", shape = 'cardioid')

# circle
wordcloud2(d1, color = "random-light", backgroundColor="black", shape = 'circle')

# triangles
wordcloud2(d1, color = "random-light", backgroundColor="black", shape = 'triangle-forward')
wordcloud2(d1, color = "random-light", backgroundColor="black", shape = 'triangle')

# square
wordcloud2(d1, color = "random-light", backgroundColor="black", shape = 'square')

# pentagon
wordcloud2(d1, color = "random-light", backgroundColor="black", shape = 'pentagon')

# star
wordcloud2(d1, color = "random-light", backgroundColor="black", shape = 'star')



set.seed(1234)
wordcloud(words = d1$word, freq = d1$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))


# markov

words <- d1$word

n_distinct(words)

head(words, 30)

fit_markov <- markovchainFit(words, method='laplace')

set.seed(123)
plot(head(fit_markov$estimate))
plot(head(fit_markov))

for (i in 2:10) {
  
  set.seed(27*i)
  markovchainSequence(n = 10, markovchain = fit_markov$estimate) %>%
    paste(collapse = " ") %>%
    str_to_sentence() %>%
    print()
}

predictive_text <- function(text, num_word){
  
  text <- strsplit(text, " ") %>% unlist() %>% tail(1)
  
  # exclude punctuation
  punctuation <- which(fit_markov$estimate[ tolower(text), ] %>% names() %>% str_detect("[:punct:]"))
  
  suggest <- fit_markov$estimate[ tolower(text), -punctuation] %>%
    sort(decreasing = T) %>% 
    head(num_word) 
  
  suggest <- suggest[suggest > 0] %>% 
    names()
  
  return(suggest)
}

predictive_text("trump", 10)


# delete the code below testing



