
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(quanteda)
library(tokenizers)
library(stringr)
badwords <- c("httpstconolejeofew","httpstcoe70pmemtDI","httpstcoB1LRIe9w9g","httpstcooVyWPTCypu","httpstco2BLB0m0v1U")
getDFMatrix <- function(text){
   myCorpus <- corpus(text)
   metadoc(myCorpus, "language") <- "portuguese"
   tokenInfo <- summary(myCorpus)
   myStemMat <- dfm(myCorpus, remove = stopwords("portuguese"), stem = TRUE, remove_punct = TRUE)
   mydfm <- dfm(myCorpus, remove = c(stopwords("portuguese"),badwords), remove_punct = TRUE, remove_numbers= TRUE)
   return(mydfm)
}

getUnigram <- function(text){
   text <- gsub(" *\\b[[:alpha:]]{1,2}\\b *", " ", text) # Remove 1-2 letter words
   text <- stringi::stri_trans_general(text, "latin-ascii")
   text <- removeWords(text,badwords)
   text <- gsub("^ +| +$|( ) +", "\\1", text) # Remove excessive spacing
   unigram <- data.frame(words = unlist(tokenize_ngrams(text, n = 2L, n_min = 1L, simplify = TRUE)))
   unigram$words[which(!is.na(str_extract(unigram$words,"httpstconolejeofew")))] <- NA

   return(unigram)
}

library(shiny)
library(twitteR)
library(wordcloud)
library(tm)
shinyServer(function (input, output) {
   rawData = reactive(function(){
#      tweets = searchTwitter(input$term, n=input$count, lang=input$lang)
      tweets = searchTwitter(input$term, n=input$count)
      tweets = twListToDF(tweets)
   })

   output$wordcl = renderPlot(function(){
      tw.text = enc2native(rawData()$text)
      tw.text = tolower(tw.text)
      tw.text = removeWords(tw.text,c(stopwords(input$lang),"rt"))
      tw.text = removePunctuation(tw.text,TRUE)
      tw.text = unlist(strsplit(tw.text," "))
      mydfm <- getDFMatrix(tw.text);
      set.seed(100)
      textplot_wordcloud(mydfm, min.freq = 2, random.order = FALSE,
                         rot.per = .25, 
                         colors = RColorBrewer::brewer.pal(8,"Dark2"))
   })
   
   output$plotlocal = renderPlot(function(){
      tweets %>% 
         group_by(location) %>% 
         summarise(audiencia = n()) %>% 
         arrange(audiencia) %>% 
         tail(30) %>% 
         ggplot(aes(x = reorder(location, as.numeric(audiencia)), 
                    y = as.numeric(audiencia))) + 
         geom_bar(stat="identity") + 
         geom_text(aes(x = reorder(location, as.numeric(audiencia)), 
                       y = as.numeric(audiencia), 
                       label = as.numeric(audiencia)), 
                   vjust = 0, hjust = 0, size = 2 ) + 
         xlab("Localização") + ylab("Audiência no Twitter") + 
         coord_flip()
   })
   
   output$plotautor = renderPlot(function(){
      tweets %>% 
         group_by(screenName) %>% 
         summarise(audiencia = n()) %>% 
         arrange(audiencia) %>% 
         tail(30) %>% 
         ggplot(aes(x = reorder(screenName, as.numeric(audiencia)), 
                    y = audiencia)) + 
         geom_bar(stat="identity") + 
         geom_text(aes(x = reorder(screenName, as.numeric(audiencia)), 
                       y = as.numeric(audiencia), 
                       label = as.numeric(audiencia)), 
                   vjust = 0, hjust = 0, size = 2 )
         xlab("Autor") + ylab("Audiência no Twitter") + 
         coord_flip()
   })
   
   output$plotunigram = renderPlot(function(){
      tw.text = enc2native(rawData()$text)
      unigram <- getUnigram(tw.text)
      unigram %>% 
         filter(!is.na(words)) %>% 
         select(words) %>% group_by(words) %>% 
         summarise(total = n()) %>% 
         arrange(total) %>% tail(20) %>% 
         ggplot(aes(reorder(words,total), total)) +
         geom_bar(stat = "identity") + 
         xlab("Palavras") + ylab("Frequência") +
         ggtitle("Palavras mais frequentes") +
         geom_text( aes (x = reorder(words,as.numeric(total)), y = total, label = total ) , vjust = 0, hjust = 0, size = 2 ) + 
         coord_flip()
   })      
   
   
})
