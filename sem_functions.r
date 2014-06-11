##################################################################################
#  
# Authors : Alfonso Crisci & Valentina Grasso
# IBIMET CNR Institute of Biometeorology Firenze via caproni 8,50145,Italia                              
# mail: a.crisci@ibimet.cnr.it
# file: sem_functions.r
# github: https://github.com/alfcrisci/socialgeosensing 
# Semantics function
##################################################################################



# source("http://gsubfn.googlecode.com/svn/trunk/R/gsubfn.R")

mrip <- function(..., install = TRUE){
    reqFun <- function(pack) {
        if(!suppressWarnings(suppressMessages(require(pack, character.only = TRUE)))) {
            message(paste0("unable to load package ", pack,
                           ": attempting to download & then load"))
            install.packages(pack)
            require(pack, character.only = TRUE)
        }
    }
    lapply(..., reqFun)
}


require(tm)
require(wordcloud)
require(wordcloud)

clean.text = function(x)
{
   # tolower
   x = tolower(x)
   # remove rt
   x = gsub("rt", "", x)
   # remove at
   x = gsub("@\\w+", "", x)
   # remove punctuation
   x = gsub("[[:punct:]]", "", x)
   # remove numbers
   x = gsub("[[:digit:]]", "", x)
   # remove links http
   x = gsub("http\\w+", "", x)
   # remove tabs
   x = gsub("[ |\t]{2,}", "", x)
   # remove blank spaces at the beginning
   x = gsub("^ ", "", x)
   # remove blank spaces at the end
   x = gsub(" $", "", x)
   return(x)
}


RemoveAtPeople <- function(text2wc) {
  
  gsub("@\\w+","", text2wc,perl = T)
}

remove_char_code <- function(text2wc) {
  
  text2wc=gsub("<.+>","", iconv(text2wc,"UTF-8"),perl = T)
   return(text2wc)
}

#Then for example, remove @d names

#text2wcs <- as.vector(sapply(oritext, RemoveAtPeople))

generateCorpus_ita= function(df,my.stopwords=c()){
  require(tm)
  require(wordcloud)
  text2.corpus= Corpus(VectorSource(df))
  #text2.corpus = tm_map(text2.corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  text2.corpus = tm_map(text2.corpus, removePunctuation)
  text2.corpus = tm_map(text2.corpus, tolower)
  text2.corpus = tm_map(text2.corpus, removeWords, stopwords('italian'))
  text2.corpus = tm_map(text2.corpus, removeWords, my.stopwords)
  text2.corpus
}

generateCorpus_eng= function(df,my.stopwords=c()){
  require(tm)
  require(wordcloud)
  text2.corpus= Corpus(VectorSource(df))
  #text2.corpus = tm_map(text2.corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  text2.corpus = tm_map(text2.corpus, removePunctuation)
  text2.corpus = tm_map(text2.corpus, tolower)
  #text2.corpus = tm_map(text2.corpus, removeWords, stopwords('english'))
  text2.corpus = tm_map(text2.corpus, removeWords, my.stopwords)
  text2.corpus
}

generateCorpus_hashtag= function(df,my.stopwords=c()){
  require(tm)
  require(wordcloud)
  text2.corpus= Corpus(VectorSource(df))
  #text2.corpus = tm_map(text2.corpus, function(x) iconv(enc2utf8(x), sub = "byte"))
  #text2.corpus = tm_map(text2.corpus, removePunctuation)
  #text2.corpus = tm_map(text2.corpus, tolower)
  #text2.corpus = tm_map(text2.corpus, removeWords, stopwords('english'))
  #text2.corpus = tm_map(text2.corpus, removeWords, my.stopwords)
  text2.corpus
}

wordcloud.generate=function(corpus,min.freq=2,palette){
  require(tm)
  require(wordcloud)
  doc.m = TermDocumentMatrix(corpus, control = list(minWordLength = 1))
  dm = as.matrix(doc.m)
  # calculate the frequency of words
  v = sort(rowSums(dm), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  #Generate the wordcloud
  wc=wordcloud(d$word, d$freq, min.freq=min.freq, colors=palette)
  wc
}
wordcloud.osm=function(corpus,min.freq=2,nsat=4,palette){
  require(tm)
  require(wordcloud)
  doc.m = TermDocumentMatrix(corpus, control = list(minWordLength = 1))
  dm = as.matrix(doc.m)
  # calculate the frequency of words
  v = sort(rowSums(dm), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  d[1:nsat,2]=d[nsat+1,2]
  #Generate the wordcloud
  wc=wordcloud(d$word, d$freq, min.freq=min.freq, colors=palette)
  wc
}
wordcloud.dtm=function(dm,min.freq=2,palette){
  
  dm=dm = as.matrix(dm)
  # calculate the frequency of words
  v = sort(rowSums(dm), decreasing=TRUE)
  d = data.frame(word=names(v), freq=v)
  #Generate the wordcloud
  wc=wordcloud(d$word, d$freq, min.freq=min.freq,random.order=T, colors=palette)
  wc
}