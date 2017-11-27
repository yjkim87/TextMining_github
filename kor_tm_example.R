library(tm)
library(KoNLP)
library(stringr)
library(Rtextrankr)
library(slam)
install.packages("topicmodels")
library(topicmodels)
library(lda)
library(wordcloud)

rm(list=ls())
setwd("C:/Users/YJK/Documents/TM_Analysis/2014/")
txt <- file("00.txt", encoding="UTF-8")
txt <- readLines(txt)
txt
word <- sapply(txt, extractNoun, USE.NAMES = F)
word
tdm<-TermDocumentMatrix(Corpus(VectorSource(word)),
                         control=list(removeNumbers=T,
                                      removePunctuation=T,
                                      wordLength=c(1,Inf)))



word <- sentence2table(txt)
as.matrix(word)
############################################################
# ********************************************
# -- Topic analysis
# ********************************************
library(rJava)
#
#
facebook = file("facebook_bigdata.txt", encoding="UTF-8")
facebook = file("3.txt", encoding="UTF-8")

facebook_data = readLines(facebook)                             
head(facebook_data)                                             
str(facebook_data)                                              

#
#
facebook_corpus = Corpus(VectorSource(facebook_data))
facebook_corpus

inspect(facebook_corpus) 

#
#
facebook_corpus[is.na(facebook_corpus)] = " "
facebook_corpus

#
#
# useSejongDic()                                                  

exNouns = function(x) {
  paste(extractNoun(as.character(x)), collapse=" ")
}


facebook_nouns = sapply(facebook_corpus, exNouns)               

class(facebook_nouns)                                           
facebook_nouns[1]                                              
facebook_nouns[2]

#
#
myCorputfacebook = Corpus(VectorSource(facebook_nouns))

myCorputfacebook = tm_map(myCorputfacebook, removePunctuation)                 
myCorputfacebook = tm_map(myCorputfacebook, removeNumbers)                     
myCorputfacebook = tm_map(myCorputfacebook, tolower)                           
myCorputfacebook = tm_map(myCorputfacebook, removeWords, stopwords('english')) 

stopwords('english') # 174

inspect(myCorputfacebook[1:5])                          

myCorputfacebook_txt = tm_map(myCorputfacebook, PlainTextDocument)
myCorputfacebook_txt
myCorputfacebook_txt = TermDocumentMatrix(myCorputfacebook, control=list(wordLengths=c(2,Inf)))
myCorputfacebook_txt

myTermfacebook.df = as.data.frame(as.matrix(myCorputfacebook_txt))
dim(myTermfacebook.df)                                                    # [1] 876  76

wordResult = sort(rowSums(myTermfacebook.df), decreasing=TRUE)
wordResult[1:100]
as.matrix(wordResult[1:100])

par(family="AppleGothic")
install.packages("extrafont")
library(extrafont)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts() 

library(wordcloud)
wordcloud(c("A", "B"), c(10, 5))
myName = names(wordResult)                                                
wordcloud(myName, wordResult)                                       

word.df = data.frame(word=myName, freq=wordResult)
str(word.df)                                                              


pal = brewer.pal(12,"Paired")                                             #pal = brewer.pal(9,"Set1") # Set1~ Set3

windowsFonts(malgun=windowsFont("#FONT NAME#"))                             # windows

x11( )

wordcloud(word.df$word, word.df$freq,
          scale=c(5,1), min.freq=3, random.order=F,
          rot.per=.1, colors=pal, family="malgun")


topWord = head(sort(wordResult, decreasing=T), 10)                        

pie(topWord, col=rainbow(10), radius=1)                                   

pct = round(topWord/sum(topWord)*100, 1)                                  
names(topWord)

lab = paste(names(topWord), "\n", pct, "%")

pie(topWord, main="topic analysis", col=rainbow(10), cex=0.8, labels=lab)
