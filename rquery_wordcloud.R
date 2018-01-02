#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() versao inicial retirada:
# - http://www.sthda.com
#+++++++++++++++++++++++++++++++++++

#Esta funcao criar uma nuvem de palavras possibilitando deversas formas de personalizacao, incluindo analise de sentimentos

#Funcoes personalidzadas utilizadas:
source("catch_error.R")
source("cleanTweets.R")
source("cleanTweetsAndRemoveNAs.R")
source("rm_accent.R")
source("html_to_text.R")

#++++++++++++++++++++++

# x: cadeia de caracteres (texto simples, URL da web, caminho do arquivo txt)
# tipo: especifique se x é um texto simples, um URL da página da Web ou um caminho de arquivo
# lang: o idioma do texto
# excludeWords: um vetor de palavras para excluir do texto
# textStemming: reduz as palavras na sua forma raiz
# colorPalette: o nome da paleta de cores tirada do pacote RColorBrewer,
# ou um nome de cor, ou um código de cores
# min.freq: as palavras com freqüência abaixo de min.freq não serão plotadas
# max.words: Número máximo de palavras a serem plotadas. Menos termos freqüentes caíram
# valor retornado pela função: uma lista (tdm, freqTable)
# Pacotes utilizados: c("tm", "SnowballC", "wordcloud", "RColorBrewer", "RCurl", "XML","lexiconPT","dplyr")
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="portuguese", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=3, max.words=200,rm.accent=F,tf.idf=F,print=T)
{ 
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("dplyr")
  library("RColorBrewer") 
  library("lexiconPT")
  library("stringr")
  
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  #limpeza
  text=cleanTweets(text)
  
  #Remover acentos
  if(rm.accent==T){
    text=rm_accent(text)
  }
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  
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
  
  #Criando matriz para retornar
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  
  if(print==T){
    # check the color palette name 
    if(colorPalette!="sentiment"){
      if(!colorPalette %in% rownames(brewer.pal.info)){ 
        colors = colorPalette
        # Plot the word cloud
        set.seed(1234)
        wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
                  random.order=FALSE, rot.per=0.35, 
                  use.r.layout=FALSE, colors=colors)
      }else{
        colors = brewer.pal(8, colorPalette)
        # Plot the word cloud
        set.seed(1234)
        wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
                  random.order=FALSE, rot.per=0.35, 
                  use.r.layout=FALSE, colors=colors)
      }
      return(list(tdm=tdm, freqTable = d)) 
    }
    
    if(colorPalette=="sentiment"){
      data("sentiLex_lem_PT02")
      sentiLex_lem_PT02 <- data("sentiLex_lem_PT02")
      
      #Selecionando as palavras (seus radicais) e sua polaridade
      dicionary=data.frame(cbind(sentiLex_lem_PT02$term,sentiLex_lem_PT02$polarity))
      matriz=d
      #Arrumando nome das bases de dados2: (Colocar nomes iguais para words)
      names(dicionary)=c("words", "sentiment")
      names(matriz)=c("words", "freq")
      
      #Transformando palavras em character:
      dicionary$words=as.character(dicionary$words)
      matriz$words=as.character(matriz$words)
      
      if(textStemming){ dicionary$words <- wordStem(dicionary$words,language = "portuguese")}
      
      dicionary=dicionary[ dicionary$sentiment==1 | dicionary$sentiment==0 | dicionary$sentiment==-1, ]
      dicionary$sentiment=as.factor(dicionary$sentiment)
      #Alterando o nome dos sentimentos:
      levels(dicionary$sentiment)=c("Negativo","Neutro","Positivo")
      
      #Join das palavras do documento com o dicionario ntivo do R
      sentimentos=data.frame(matriz) %>%
        left_join(data.frame(dicionary),by="words") %>%
        select(words,sentiment,freq)%>%
        distinct(words,.keep_all = T)
      
      rownames(d)=d$word
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
