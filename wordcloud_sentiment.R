# Como utilizar a funcao wordcloud_sentiment():

#----------------.-------------------------------------------------------------------------------------------------.----------------------
#      Argumento | Definicao                                                                                       | Default
#----------------|------------------------------------------------------------------------------------------------------------------------
#              x | Coluna com textos, ou url (ver type)                                                            |
#----------------|------------------------------------------------------------------------------------------------------------------------
#         type   | Tipo de arquivo para produzir a nuvem                                                           |
#                |- url: endereço da qual deseja-se fazer a nuvem                                                  |
#                |- text: Coluna com textos (cada linha representa um comentario)                                  |                                 |
#----------------|------------------------------------------------------------------------------------------------------------------------
#            lang| Lingua utilizada na nuvem (para remover as stopwords em portugues)                              |(Default = "portuguese")
#----------------|------------------------------------------------------------------------------------------------------------------------
#   excludeWords | Vetor de stopwords adicionais para serem removidas                                              |(Default=NULL) ou seja, sem palavras adicionais por padrao
#----------------|------------------------------------------------------------------------------------------------------------------------
#       min.freq | Palavras com frequência abaixo de min.freq não serão plotadas                                   |
#----------------|------------------------------------------------------------------------------------------------------------------------
#         tf.idf | Realiza a transformação tf.idf                                                                  | (Default=F)
#----------------|------------------------------------------------------------------------------------------------------------------------
#      max.words | Número máximo de palavras a serem plotadas. Menos termos freqüentes caíram                      |
#----------------|------------------------------------------------------------------------------------------------------------------------
#      rm.accent | Controla de os acentos serao removidos:                                                         |(Default = TRUE)
#                |- TRUE para remover acentos,                                                                     |     
#                |- FALSE caso nao queira remover acentos (obs: de preferencia para remover acentos)               |
#----------------|------------------------------------------------------------------------------------------------------------------------
#          print | Controla de a nuvem de palavras sera exibida:                                                   |(Default = TRUE)
#                |- TRUE para plotar a nuvem de palavras,                                                          |
#                |- FALSE para obter apenas a matriz de termos e a tabela de frequencias                           |
#----------------|------------------------------------------------------------------------------------------------------------------------
#         ngrams | Controla a frequencia sequencias de n palavras :                                                |(Default = 0)
#                |- 2: sequencias de duas palavras                                                                 |
#                |- 3: sequencias de tres palavras                                                                 |
#                |- n: sequencias de n palavras                                                                    |
#----------------|------------------------------------------------------------------------------------------------------------------------
#   colorPalette | Este argumento define duas opcoes de como as palavras serao coloridas:                          |(Default = "Dark2") 
#                |- "sentiment" : cada palavra é pintada de azul (positivo), vermelho (negativo) ou cinza (neutro) |
#                |- "Dark2"     : exemplo de palleta de cores, para mais opcoes consultar rodar o codigo abaixo:   |
#                |                display.brewer.all()                                                             |
#----------------|------------------------------------------------------------------------------------------------------------------------
# Funcao retorna | além da nuvem de palavras, retorna uma lista com dois objetos:                                  |    
#                |- $tdm = matriz de termos e                                                                      |
#                |- $freqTable = tabela de frequencia em ordem decrescente                                         |
#----------------'-------------------------------------------------------------------------------------------------'----------------------

#Funcao:
wordcloud_sentiment <- function(x,  type=c("text", "url", "file"),
                             lang="portuguese", excludeWords=NULL,  colorPalette="Dark2",
                             min.freq=3, max.words=200,rm.accent=T,tf.idf=F,print=T,ngrams=0, 
                             textStemming=FALSE)
{ 
  
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("dplyr")
  library("RColorBrewer") 
  library("lexiconPT")
  library("stringr")
  #++++++++++++++++++++++++++++++++++
  # Download e analise de webpage
  html_to_text<-function(url){
    library(RCurl)
    library(XML)
    # download html
    html.doc <- getURL(url)  
    #convert to plain text
    doc = htmlParse(html.doc, asText=TRUE)
    # "//text()" returns all text outside of HTML tags.
    # We also don’t want text such as style and script codes
    text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
    # Format text vector into one character string
    return(paste(text, collapse = " "))
  }
  #++++++++++++++++++++++++++++++++++
  
  if(type[1]=="url"){ text <- html_to_text(x)}
  else{ if(type[1]=="text") text <- x}
  
  
  
  # Remover acentos
  rm_accent <- function(str,pattern="all") {
    # Rotinas e funções úteis V 1.0
    # rm.accent - REMOVE ACENTOS DE PALAVRAS
    # Função que tira todos os acentos e pontuações de um vetor de strings.
    # Parâmetros:
    # str - vetor de strings que terão seus acentos retirados.
    # patterns - vetor de strings com um ou mais elementos indicando quais acentos deverão ser retirados.
    #            Para indicar quais acentos deverão ser retirados, um vetor com os símbolos deverão ser passados.
    #            Exemplo: pattern = c("´", "^") retirará os acentos agudos e circunflexos apenas.
    #            Outras palavras aceitas: "all" (retira todos os acentos, que são "´", "`", "^", "~", "¨", "ç")
    if(!is.character(str))
      str <- as.character(str)
    
    pattern <- unique(pattern)
    
    if(any(pattern=="Ç"))
      pattern[pattern=="Ç"] <- "ç"
    
    symbols <- c(
      acute = "áéíóúÁÉÍÓÚýÝ",
      grave = "àèìòùÀÈÌÒÙ",
      circunflex = "âêîôûÂÊÎÔÛ",
      tilde = "ãõÃÕñÑ",
      umlaut = "äëïöüÄËÏÖÜÿ",
      cedil = "çÇ"
    )
    
    nudeSymbols <- c(
      acute = "aeiouAEIOUyY",
      grave = "aeiouAEIOU",
      circunflex = "aeiouAEIOU",
      tilde = "aoAOnN",
      umlaut = "aeiouAEIOUy",
      cedil = "cC"
    )
    
    accentTypes <- c("´","`","^","~","¨","ç")
    
    if(any(c("all","al","a","todos","t","to","tod","todo")%in%pattern)) # opcao retirar todos
      return(chartr(paste(symbols, collapse=""), paste(nudeSymbols, collapse=""), str))
    
    for(i in which(accentTypes%in%pattern))
      str <- chartr(symbols[i],nudeSymbols[i], str)
    
    return(str)
  }
  #++++++++++++++++++++++++++++++++++
  # Captação de erros de codificacao:
  catch.error = function(x){
    # let us create a missing value for test purpose
    y = NA
    # Try to catch that error (NA) we just created
    catch_error = tryCatch(tolower(x), error=function(e) e)
    # if not an error
    if (!inherits(catch_error, "error"))
      y = tolower(x)
    # check result if error exists, otherwise the function works fine.
    return(y)
  }
  #++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++
  # Limpeza de caracteres especiais
  cleanTweets<- function(tweet){
    
    # Clean the tweet for sentiment analysis
    
    # remove html links
    
    tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
    
    # Remove retweet entities
    
    tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
    
    # Remove all "#Hashtag"
    
    tweet = gsub("#\\w+", " ", tweet)
    
    # Remove all "@people"
    
    tweet = gsub("@\\w+", " ", tweet)
    
    # Remove all the punctuation
    
    tweet = gsub("[[:punct:]]", " ", tweet)
    
    # Remove numbers, we need only text for analytics
    
    tweet = gsub("[[:digit:]]", " ", tweet)
    
    # Remove unnecessary spaces (white spaces, tabs etc)
    tweet = gsub("[ \t]{2,}", " ", tweet)
    tweet = gsub("^\\s+|\\s+$", "", tweet)
    
    tweet = gsub('https://','',tweet) # removes https://
    tweet = gsub('http://','',tweet) # removes http://
    tweet=gsub('[^[:graph:]]', ' ',tweet) ## removes graphic characters 
    #like emoticons 
    tweet = gsub('[[:punct:]]', '', tweet) # removes punctuation 
    tweet = gsub('[[:cntrl:]]', '', tweet) # removes control characters
    tweet = gsub('\\d+', '', tweet) # removes numbers
    tweet=str_replace_all(tweet,"[^[:graph:]]", " ")
    #tweet=SnowballC::wordStem(tweet,language = "portuguese")
    
    
    #Convert all text to lowercase
    tweet = catch.error(tweet)
    
    return(tweet)
  }
  #++++++++++++++++++++++++++++++++++
  #++++++++++++++++++++++++++++++++++
  # Remover NAs
  cleanTweetsAndRemoveNAs<- function(Tweets) {
    
    TweetsCleaned = sapply(Tweets, cleanTweets)
    
    # Remove the "NA" tweets from this tweet list
    TweetsCleaned = TweetsCleaned[!is.na(TweetsCleaned)]
    
    names(TweetsCleaned) = NULL
    # Remove the repetitive tweets from this tweet list
    
    TweetsCleaned = unique(TweetsCleaned)
    
    TweetsCleaned
  }
  #++++++++++++++++++++++++++++++++++
  #Temporario:
  text <- text
    
  #limpeza
  text=cleanTweets(text)
  
  #Remover acentos
  if(rm.accent==T){
    text=rm_accent(text)
  }
  
  # Carrega o arquivo de texto para um corpus:
  docs <- Corpus(DataframeSource(as.data.frame(text)))
  
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument,language=lang)
  
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  
  #Se tf.idf for verdadeiro:
  if(tf.idf==T){
    tdm=weightTfIdf(tdm,normalize=T)
  }
  
  #Se Ngram=True:
  if(ngrams!=0){
    Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))
    tdm = TermDocumentMatrix(docs,control = list(tokenize = Tokenizer))
  }
  
  #Criando matriz para retornar
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(words = names(v),freq=v)
  
  if(print==T){
    # check the color palette name 
    if(colorPalette!="sentiment"){
      if(!colorPalette %in% rownames(brewer.pal.info)){ 
        colors = colorPalette
        # Plot the word cloud
        set.seed(1234)
        wordcloud(d$words,d$freq, min.freq=min.freq, max.words=max.words,
                  random.order=FALSE, rot.per=0.35, 
                  use.r.layout=FALSE, colors=colors)
      }else{
        colors = brewer.pal(8, colorPalette)
        # Plot the word cloud
        set.seed(1234)
        wordcloud(d$words,d$freq, min.freq=min.freq, max.words=max.words,
                  random.order=FALSE, rot.per=0.35, 
                  use.r.layout=FALSE, colors=colors)
      }
      return(list(tdm=tdm, freqTable = d)) 
    }
    
    if(colorPalette=="sentiment"){
      sentiLex_lem_PT02 <- lexiconPT::sentiLex_lem_PT02
      
      #Selecionando as palavras (seus radicais) e sua polaridade
      dicionary=data.frame(cbind(sentiLex_lem_PT02$term,sentiLex_lem_PT02$polarity))
      matriz=d
      #Arrumando nome das bases de dados2: (Colocar nomes iguais para words)
      names(dicionary)=c("words", "sentiment")
      names(matriz)=c("words", "freq")
      
      #Transformando palavras em character:
      dicionary$words=as.character(dicionary$words)
      matriz$words=as.character(matriz$words)
      
      
      dicionary=dicionary[ dicionary$sentiment==1 | dicionary$sentiment==0 | dicionary$sentiment==-1, ]
      table(dicionary$sentiment)
      dicionary$sentiment=as.factor(dicionary$sentiment)
      #Alterando o nome dos sentimentos:
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==-1]=c("Negativo")
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==0]=c("Neutro")
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==1]=c("Positivo")
      
      #Join das palavras do documento com o dicionario ntivo do R
      sentimentos=data.frame(matriz) %>%
        left_join(data.frame(dicionary),by="words") %>%
        select(words,sentiment,freq)%>%
        distinct(words,.keep_all = T)
      
      rownames(d)=d$words
      #Neutro para palavras fora do dicionario
      sentimentos$sentiment[is.na(sentimentos$sentiment)]="Neutro"
      
      #Criando coluna de cores para cada sentimento
      sentimentos$col=c(ifelse(sentimentos$sentiment=="Neutro","gray80",ifelse(sentimentos$sentiment=="Positivo","blue","red")))
      ##########################################
      # Plot the word cloud
      set.seed(1234)
      wordcloud(sentimentos$words,freq = sentimentos$freq, min.freq=min.freq, max.words=max.words,
                random.order=FALSE, rot.per=0.35, 
                use.r.layout=FALSE, colors = sentimentos$col,ordered.colors = T)
      return(list(tdm=tdm, freqTable = sentimentos))
    }
  }else{
    return(list(tdm=tdm, freqTable = d))}
  
}


# #Exemplo: ---------------------------------------------------------------

#Com url:
wordcloud_sentiment(x="https://support.rstudio.com/hc/en-us/articles/200714013-Slide-Transitions-and-Navigation",type="url")

#Com base exemplo:
x=base$V2                                            #Disponível em: https://github.com/gomesfellipe/appwordcloud/
wordcloud_sentiment(x,type="text",color="sentiment")

# #Referencia: ---------------------------------------------------------------

#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() versao inicial retirada:
# - http://www.sthda.com
# referencia:
# - https://sites.google.com/site/miningtwitter/questions/sentiment/analysis
#+++++++++++++++++++++++++++++++++++