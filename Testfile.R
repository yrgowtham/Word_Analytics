names(watsapp)[1]<-"date_column"
names(watsapp)[2]<-"text_column"
library(dplyr)
library(stringi)
library(DescTools)
watsapp%>%separate(text_column,"-")
a<-stri_split_fixed(str = watsapp$text_column, pattern = " -", n = 1)
head(a)
library(stringr)
watsapp$time_column<-str_extract(watsapp$text_column, "[0-9]{1,2}:[0-9]{1,2} (AM|PM)")
sum(is.na(test))
watsapp$date_column_new<-str_extract(watsapp$date_column, "[0-9/]{4,}")
#remove rows that have na for now
new_Dataset$date_column<-NULL
temp<-str_extract(watsapp$text_column, "[-] [a-zA-Z]+")
temp<-str_extract(watsapp$text_column, "[^\s]*$")
temp<-sub("\\S+\\s+\\S+\\s+\\S+\\s+", '', watsapp$text_column)
temp<-word(temp, 1)
watsapp$author_column<-temp

#extract just the text which is after 4th space
temp<-sub("\\S+\\s+\\S+\\s+\\S+\\s+\\S+\\s+", '', watsapp$text_column)
watsapp$text_column_new<-temp

#final dataset :
new_Dataset<-watsapp[complete.cases(watsapp), ]
new_Dataset$date_column<-NULL
new_Dataset$text_column<-NULL
library(dplyr)
new_Dataset<-new_Dataset%>%select(date_column_new,time_column,author_column,text_column_new)
as.POSIXct(paste(new_Dataset$date_column_new[1], new_Dataset$time_column[1]), format="%m/%d/%YY %H:%M:%S")
library(lubridate)
library(chron)
times(new_Dataset$time_column[1])
new_Dataset$time_column_24hour<-(format(strptime(new_Dataset$time_column, "%I:%M %p"), "%H:%M"))

head(new_Dataset$text_column_new)

#remove omited and media

new_Dataset_2<-new_Dataset[! new_Dataset$text_column_new %in% c('<Media omitted>') ,]


sruthi_data<-new_Dataset_2[new_Dataset_2$author_column %in% c('Sruthi:'),]
gowtham_data<-new_Dataset_2[new_Dataset_2$author_column %in% c('gowtham:'),]

library(stringr)
usableText=str_replace_all(gowtham_data$text_column_new,"[^[:graph:]]", " ") 
dataSet <- iconv(usableText, 'UTF-8', 'ASCII')

head(usableText)
head(dataSet)


#text data cleaning
library(tm)
library(tidytext)
chats_corpus<-Corpus(VectorSource(as.vector(dataSet)))
#tidy_chats_corpus<-new_Dataset%>%select(date_column_new,text_column_new)%>%unnest_tokens("word",text_column_new)
#tidy_chats_corpus%>%count(word)%>%arrange(desc(n))
#the above is not informative 

#Text Pre processing 
#remove stop words
data("stop_words")

custom_stopwords<-stopwords("english")
custom_stopwords<-custom_stopwords[c(2:8,10:174)]


chats_corpus<-tm_map(chats_corpus,removeWords,custom_stopwords)
#tidy_chats_corpus<-tidy_chats_corpus%>%anti_join(stop_words)
#tidy_chats_corpus%>%count(word)%>%arrange(desc(n))
#remove punctuations
chats_corpus<-tm_map(chats_corpus,content_transformer(removePunctuation))
#remove numbers 
chats_corpus<-tm_map(chats_corpus,content_transformer(removeNumbers))
chats_corpus<-tm_map(chats_corpus,content_transformer(tolower))
chats_corpus<-tm_map(chats_corpus,content_transformer(stripWhitespace))
chats_corpus<-tm_map(chats_corpus,content_transformer(stemDocument),language="english")
dtm<-TermDocumentMatrix(chats_corpus)
library(wordcloud)
m<-as.matrix(dtm)
v<-sort(rowSums(m),decreasing = TRUE)
s<-data.frame(word=names(v),freq=v)
head(s)
set.seed(1234)

wordcloud(words=s$word,freq=s$freq,min.freq=10,max.words = 200,random.order = FALSE,
          rot.per = 0,colors = brewer.pal(34,"Dark"))
?brewer.pal
