#Install packages shiny to develop interface for the current application
###Packages-------
install.packages("shiny")
install.packages("tidytext")
install.packages("XML")
install.packages("RCurl")
install.packages("tm")
install.packages("syuzhet")
install.packages("wordcloud")
install.packages("dplyr")
install.packages("stringr")
install.packages("SentimentAnalysis")
install.packages("RSentiment")
install.packages("DT")
install.packages("Rcpp")
install.packages("RWeka")
install.packages("SnowballC")
install.packages("rJava", dependencies = T)
install.packages("plotrix")

library("shiny")
library("tidytext")
library("XML")
library("RCurl")
library("tm")
library("wordcloud")
library("syuzhet")
library(dplyr)
library("stringr")
library("SentimentAnalysis")
library("RSentiment")
library(DT)
library(Rcpp)
library(RWeka)
library("SnowballC")
library('rJava')
library(ggplot2)
library(plotrix)
Sys.setenv(Java_HOME='C:\\Program Files (x86)\\Java\\jre1.8.0_191')
options(java.parameters = "-Xmx1g")
###User Interface--------


server<-function(input,output,session){

url.react<- "https://www.indiatoday.in/india/story/rafale-supreme-court-verdict-cag-report-pac-1410102-2018-12-15" #reactive({url.react<-input$mytext})
  
###Text-input---------
#Read and parsing HTML file

URLparsed<-getURL(url.react)
print(URLparsed)
doc.html<- htmlTreeParse(URLparsed, useInternal = TRUE)

#Extract all the paragraphs (HTML tag is p, starting at the root of the document). 
#Unlist flattens the list to create a character vector.

doc<- unlist(xpathSApply(doc.html, path="//p", xmlValue))
class(doc)
print(doc)

#the doc file results in a list of statements parsed from article
#the following is done to merge the list into one paragraph below:

doc.txt<-NULL
for(i in 2:(length(doc)-1)) {
  doc.txt<-paste(doc.txt,as.character(doc[i]),sep='')
}
is.vector(doc.txt)
length(doc.txt)
print(doc.txt)

#we remove the elements \r and \n and create a corpus for text analysis

doc.txt<-gsub("\r?\n|\r","",doc.txt)
text.corpus<-Corpus(VectorSource(doc.txt))

#strwrap(text.corpus[[1]])

#we can use tm’s text mining functions to change all words to lower cases, remove punctuation, strip out white space
#and to remove stop words.  Stop words are commonly used words that are often removed from a corpus before any analysis 
#is performed

text.corpus<-tm_map(text.corpus, tolower) #text to lowercase
text.corpus<-tm_map(text.corpus, removePunctuation)
text.corpus<-tm_map(text.corpus, stripWhitespace)
#text.corpus<-tm_map(text.corpus, removewords, stopwords('english'))
text.corpus<-tm_map(text.corpus, PlainTextDocument)
text.corpus<-tm_map(text.corpus, removeNumbers)
text.corpus<-tm_map(text.corpus, removeSource)
text.corpus<-tm_map(text.corpus, stemDocument)
#strwrap(text.corpus[[1]])

#A term document matrix has a row for each unique word 
#and the value in each row is the number of times that particular word appears in a document

#termdocument.matrix<-TermDocumentMatrix(text.corpus)
#inspect(termdocument.matrix[1:10,])

#Output visualization of words in the URL
#word.matrix<-as.matrix(termdocument.matrix)
#colnames(word.matrix)<-'sentiment analysis'
#a<-sort(rowSums(word.matrix),decreasing=T)
#b<-data.frame(word=names(a),freq=a)
#par(bg='black')
#output$plot_wc<-wordcloud(b$word,b$freq,random.order = F,max.words=300,color='gold')


#calculating sentiments
alpha_up = DocumentTermMatrix(VCorpus(VectorSource(text.corpus[[1]]$content)))
print(alpha_up)
beta_up<-colSums(as.matrix(alpha_up))
print(beta_up)
senti= calculate_sentiment(names(beta_up))
senti=cbind(senti,as.data.frame(beta_up))
print(senti)
senti_positive=senti[senti$sentiment=='Positive',]
senti_negative=senti[senti$sentiment=='Negative',]

cat("We have far lower negative Sentiments: ",sum(senti_negative$beta_up)," than positive: ",sum(senti_positive$beta_up))

#Output
sentiment_result<-(sum(senti_negative$beta_up)/sum(senti_positive$beta_up))
if(sentiment_result<1){print("The article has Positive Sentiment" )} else if(sentiment_result>1){print("The article has Negative Sentiment")} else {print("The article has Neutral Sentiment")}

#calculating sentiments
DT::datatable(senti_positive)
DT::datatable(senti_negative)


#using syuzhet package to get emotional state of the article

doc_syz.txt<-gsub("\r?\n|\r","",doc.txt)
doc_syz.txt<-gsub("[[:punct:]]","",doc_syz.txt)
doc_syz.txt<-gsub("Share this withEmailFacebookMessengerMessengerTwitterPinterestWhatsAppLinkedInCopy this linkThese are external links and will open in a new window","",doc_syz.txt)
doc_syz.txt<-gsub("(RT|via)((?:\\b\\w*@\\w+)+)","",doc_syz.txt)
doc_syz.txt<-gsub("http[^[:blank:]]+","",doc_syz.txt)

syz_sentiment<-get_nrc_sentiment(doc_syz.txt)
syz_sentiment.positive =sum(syz_sentiment$positive)
syz_sentiment.anger =sum(syz_sentiment$anger)
syz_sentiment.anticipation =sum(syz_sentiment$anticipation)
syz_sentiment.disgust =sum(syz_sentiment$disgust)
syz_sentiment.fear =sum(syz_sentiment$fear)
syz_sentiment.joy =sum(syz_sentiment$joy)
syz_sentiment.sadness =sum(syz_sentiment$sadness)
syz_sentiment.surprise =sum(syz_sentiment$surprise)
syz_sentiment.trust =sum(syz_sentiment$trust)
syz_sentiment.negative =sum(syz_sentiment$negative)

#visualization
yaxis<-c(syz_sentiment.positive,
         +syz_sentiment.anger,
         +syz_sentiment.anticipation,
         +syz_sentiment.disgust,
         +syz_sentiment.fear,
         +syz_sentiment.joy,
         +syz_sentiment.sadness,
         +syz_sentiment.surprise,
         +syz_sentiment.trust,
         +syz_sentiment.negative)
xaxis<-c("positive",
         "anger",
         "anticipation",
         "disgust",
         "fear",
         "joy",
         "sadness",
         "surprise",
         "trust",
         "negative")

df_senti<-data.frame(cbind(Emotion=c("positive",
                                     "anger",
                                     "anticipation",
                                     "disgust",
                                     "fear",
                                     "joy",
                                     "sadness",
                                     "surprise",
                                     "trust",
                                     "negative")), Values=c(syz_sentiment.positive,
                                                            syz_sentiment.anger,
                                                            syz_sentiment.anticipation,
                                                            syz_sentiment.disgust,
                                                            syz_sentiment.fear,
                                                            syz_sentiment.joy,
                                                            syz_sentiment.sadness,
                                                            syz_sentiment.surprise,
                                                            syz_sentiment.trust,
                                                            syz_sentiment.negative))


p = ggplot(df_senti, aes(x=Emotion, y=Values, fill=Values)) +
  geom_histogram(binwidth=1, stat='identity') +theme_light() +
  scale_fill_gradient(low='red', high='green') +
  theme(axis.title.y=element_text(angle=0))
p+coord_polar()
output$plot_polar<-p+coord_polar()

}

Ui<-basicPage(
  titlePanel("Sentiment Analysis of the Article"),
  sidebarLayout(
    sidebarPanel(textInput("mytext",label="Enter URL Here:")
       
    ),
    
    tabsetpanel(tabPanel("WordCloud",plotOutput("plot_wc")),
                tabPanel("Polar Plot",plotOutput("plot_polar"))
    )
  )
  )
  


shinyApp(Ui,server)
runApp("~/shinyapp")