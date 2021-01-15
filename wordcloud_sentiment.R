
# Como utilizar a funcao wordcloud_sentiment():

#----------------.--------------------------------------------------------------------------------.-------------------------------------
#      Argumento | Definicao                                                                      | Default
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#              x | Coluna com textos, ou url (ver type)                                           |
#----------------|----------------------------------------------------------------------------------------------------------------------
#           type | Tipo de arquivo para produzir a nuvem                                          |
#                |- url: Endereço da qual deseja-se fazer a nuvem                                 |
#                |- text: Coluna com textos (cada linha representa um comentario)                 |
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#           lang | Lingua utilizada na nuvem (para remover as stopwords em portugues)             | Default="portuguese"
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#   excludeWords | Vetor de stopwords adicionais para serem removidas                             | Default=NULL
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#           freq | Palavras com frequência abaixo de freq não serão plotadas                      | Default=15
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#         tf_idf | Realiza a transformação tf.idf                                                 | Default=F
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#            max | Número máximo de palavras a serem plotadas. Menos termos freqüentes caíram     | Default=200
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#          print | Controla se a nuvem de palavras sera exibida:                                  | Default = TRUE
#                |- TRUE para plotar a nuvem de palavras,                                         |
#                |- FALSE para obter apenas a  tabela de frequencias                              |
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#         ngrams | Controla a frequencia sequencias de n palavras :                               | Default = 1
#                |- 2: sequencias de duas palavras                                                |
#                |- 3: sequencias de tres palavras                                                |
#                |- n: sequencias de n palavras                                                   |
#----------------'--------------------------------------------------------------------------------'-------------------------------------
#      sentiment | Controla se sera utilizada a analise de sentimentos                            | Default = FALSE 
#                |- TRUE para , pintar de azul (positivo), vermelho (negativo) ou cinza (neutro)  |
#                |- FALSE para obter nuvem com cor default                                        |                   
#----------------'--------------------------------------------------------------------------------'-------------------------------------
# Funcao retorna | Além da nuvem de palavras, retorna:                                            |
#                |- $freq = tabela de frequencia em ordem decrescente                             |
#----------------'--------------------------------------------------------------------------------'-------------------------------------


# Funcao wordcloud_sentiment() --------------------------------------------

wordcloud_sentiment = function(x,                          # Uma coluna de textos
                               type=c("text", "url"),      # Tipo de entrada dos dados
                               lang = "portuguese",        # Idioma dos dados
                               excludeWords=NULL,          # Palavras que sera excluidas
                               freq=15,                    # Frequencia minima da palavra para aparecer na nuvem
                               tf_idf=F,                   # Transformacao tf-idf
                               max= 200,                   # Numero maximo de palavras na nuvem
                               print=T,                    # Define se a nuvem sera exibida
                               ngrams=1,                   # Ordem da sequencia de palavras
                               sentiment=F,                # Se a analise de sentimentos sera realizada
                               horizontal=0.35,            # Porcentagem de palavras na horizontal (De zero a um)
                               windows = FALSE,
                               textStemming=F){            # Define se os sufixos serao removidos
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+                      Importanto pacotes utilizados no aplicativo                             +
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  suppressMessages(library(stringr))       #Pacote para manipulação de strings
  suppressMessages(library(dplyr))         #Pacote para manipulação de dados
  suppressMessages(library(tm))            #Pacote de para text mining
  suppressMessages(library(wordcloud))     #Pacote para nuvem de palavras
  suppressMessages(library(readxl))        #Pacote para leitura de dados excel
  suppressMessages(library(tidytext))      #Manipulação de textos
  suppressMessages(library(reshape2))      #Manipulação de dados
  suppressMessages(library(lexiconPT))     #Importar palavras de sentimentos
  suppressMessages(library(SnowballC))     #Para steamming
  suppressMessages(library(purrr))         #Ferramentas de programação funcional
  suppressMessages(library(DT))            #Renderizar tabela da segunda pagina
  suppressMessages(library(ngram))         #Busca por sequencias de palavras 
  suppressMessages(library(abjutils))      #Remover acentos
  #+--------------------------------------+---------------------------------------------+  
  #Pacotes desativados:   (Nessa nova versao o RWeka nao sera mais utilizado)
  # library(rJava)
  # library(RWeka)
  #-----------------------------------------------------------------------------------------------
  
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  #+                  Importando fontes nativas do windows para as letras                         +
  #++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
  if(windows == TRUE){
      windowsFonts(
        A=windowsFont("Arial Black"),
        B=windowsFont("Bookman Old Style"),
        C=windowsFont("Comic Sans MS"),
        D=windowsFont("Symbol")
      )
  }
  #-----------------------------------------------------------------------------------------------
  
  
  #Leitura da coluna de texto informada:
  if(type[1]=="url"){ text <- html_to_text(x)}
  else{ if(type[1]=="text") text <- x}
  text=as.data.frame(text)
  
  text <- text %>% 
    as_tibble() %>% 
    transmute(value = cleanTweets(text) %>% enc2native() %>% abjutils::rm_accent()) 
  
  
  #Criando corpus:
  text=data.frame(doc_id=1:length(text$value),                           #Criando o data.frame para criar o corpus
                  text=text$value)
  myCorpus = Corpus(DataframeSource(as.data.frame(text)))          #Criando o corpus a partir de um data.frame
  
  # if(textStemming) myCorpus <- Corpus(VectorSource(text))        #Para stemming (Com atualizacao do pacote tm nao eh mais necessario) 
  
  
  #Tratamento do corpus:
  myCorpus=myCorpus%>%
    tm_map(content_transformer(tolower))%>%                        # Converte para minusculo
    tm_map(removeNumbers)%>%                                       # Remove numeros do corpus
    tm_map(removeWords, stopwords(lang))%>%                        # Remove stopwords do dicionario portgues
    tm_map(removePunctuation)%>%                                   # Remove pontuacao
    tm_map(stripWhitespace)%>%                                     # Remove excessos de espacos em branco
    tm_map(removeWords, excludeWords)                              # Exclue palavras adicionais  
  
  #Realiza o steamming se textStemming==V
  if(textStemming) myCorpus <- tm_map(myCorpus, stemDocument,language=lang)
  
  
  #Agora sera feita a procura por sequencias de palavras se ngrams for diferente de 1:
  
  #Abordagem com RWeka (Off)--------------------------------------------------------------------
  # Abordagem antiga utilizava o pacote RWeka, esse pacote nao sera mais utilizado
  # Tokenizer <- function(x) NGramTokenizer(x, Weka_control(min = ngrams, max = ngrams))
  # myDTM = TermDocumentMatrix(myCorpus,control = list(tokenize = Tokenizer))
  #---------------------------------------------------------------------------------------------
  
  # Se ngrams for difernte de 1:
  if(ngrams!=1){
    temp=ngram::ngram(ngram::concatenate(myCorpus),ngrams)            # Objeto temporario recebe objeto que guarda sequencias
    temp=get.phrasetable(temp)                                        # Obtendo tabela de sequencias do objeto acima
    
    temp$ngrams=temp$ngrams%>%                                        # Limpeza das sequencias obtidas:
      str_replace_all(pattern = "^([A-Za-z] [A-Za-z])+","")%>%        # Remover sequencias de apenas 1 letras 
      str_replace_all(pattern = "[:punct:]","")%>%                    # Remover caracteres especiais
      str_replace_all(pattern = "\n","")%>%                           # Remover o marcador de "nova linha"
      str_trim()                                                      # Remover espaços em branco sobrando
    
    #Apos a limpeza..
    
    temp=temp[temp$ngrams!="",]                                       # Selecionando apenas as linhas que contenham informacao
    
    temp=temp%>%                                                      # Novamente manipulando o objeto que contem a tabela de sequencias
      group_by(ngrams) %>%                                            # Agrupando por "ngrams" (sequencias obtidas)
      summarise(freq=sum(freq))%>%                                    # Resumir as linhas repetidas pela soma das frequencias
      arrange(desc(freq))%>%                                          # Organizando da maior para a menos frequencia
      as.matrix()                                                     # Alterando o tipo de objeto para matrix
    
    rownames(temp)=str_c(temp[,1])                                    # O nome das linhas passa a ser a sequencia correspondente
    v = sort(temp[,2],decreasing = T)                                 # Retorna um objeto com as frequencias em ordem decrescente e linhas nomeadas
    d <- data.frame(words = names(v),freq=v) 
    
    # Caso contrario, se ngrams for igual a 1 (sem sequencias)      
  }else{
    
    myDTM = TermDocumentMatrix(myCorpus,                              # Obtendo matriz de termos:
                               control = list(minWordLength = 1))     # Com no minimo 1 ocorrencia
    
    #Se se ngrams for igual a 1 (sem sequencias) e tf.idf for verdadeiro:
    if(tf_idf==T){
      myDTM=weightTfIdf(myDTM,normalize=T)                            # Realiza transformacao tf-idf
    }
    
    m = as.matrix(myDTM)                                              # Transforma o objeto em matrix
    v = sort(rowSums(m),decreasing=TRUE)                              # Retorna um objeto com as frequencias em ordem decrescente e linhas nomeadas
    d <- data.frame(words = names(v),freq=v) 
    
  }
  if(print==T){                                                       # Se a imagem vai ser exibida
    
    # Analise de sentimentos
    if(sentiment){                                                    # Se a opcao de sentimentos estiver selecionada
      
      sentiLex_lem_PT02 <- lexiconPT::sentiLex_lem_PT02               # Obtem o dicionario lexico em Portugues
      
      dicionary=data.frame(cbind(sentiLex_lem_PT02$term,              # Seleciona as palavras e 
                                 sentiLex_lem_PT02$polarity))         # a polaridade de cada uma
      
      matriz=d                                                        # Cria uma base temporaria para o join
      
      names(dicionary)=c("words", "sentiment")                        # Alterando os nomes das colunas do dicionario e da base obtida
      names(matriz)=c("words", "freq")                                # (O nome deve ser o mesmo para o join)
      
      dicionary$words=as.character(dicionary$words)                   # Transformando as duas
      matriz$words=as.character(matriz$words)                         # bases obtidas em strings
      
      
      if(textStemming){                                               # Se textStemming estiver ligada
        dicionary$words <- wordStem(dicionary$words,                  # Steamming o dicionario lexico tambem
                                    language = lang)                  # De acordo com a lingua portuguesa
      }
      
      dicionary=dicionary[ dicionary$sentiment==1 | dicionary$sentiment==0 | dicionary$sentiment==-1, ] # Obtendo so os polos -1, 1 e 0
      dicionary$sentiment=as.factor(dicionary$sentiment)              # Transformando em fator
      
      #Alterando o nome dos fatores para o respectivo sentimento:
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==-1]=c("Negativo")
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==0]=c("Neutro")
      levels(dicionary$sentiment)[levels(dicionary$sentiment)==1]=c("Positivo")
      
      #Join das strings do documento com as strings nativas do pacote LexiconPT
      sentimentos=data.frame(matriz) %>%                              # O objeto "sentimentos" recebe
        left_join(data.frame(dicionary),by="words") %>%               # left_join mantem todas as linhas da base matriz (esquerda) e busca por pares com o dicionario 
        select(words,sentiment,freq)%>%                               # Seleciona as colunas das strings, o sentimento e sua frequencia
        distinct(words,.keep_all = T)                                 # Seleciona apenas linhas distintas
      
      rownames(d)=d$words                                             # Nomeia as linhas do data.frame d   
      
      
      
      sentimentos$sentiment[is.na(sentimentos$sentiment)]="Neutro"    # Neutro para palavras fora do dicionario
      
      #Criando coluna de cores para cada sentimento
      sentimentos$col=c(                                              # Criando uma nova coluna para objeto "sentimentos"
        ifelse(sentimentos$sentiment=="Neutro","#666666",             # Se for "Neutro" sera cinza, se nao:
               ifelse(sentimentos$sentiment=="Positivo","blue","red"))) #Se for "Positivo" recebe azul, se nao é vermelho
      
      #dev.new(width=10, height=10)
      par(bg="#f2f2f2")                                               # Define a cor do fundo da figura
      set.seed(1234)                                                  # Semente para gerar a mesma nuvem
      wordcloud(names(v), freq=as.numeric(v),                         # Funcao repetitiva wordcloud, objeto v foi retornado pela funcao global getTermMatrix()
                scale=c(4,0.5),                                       # Um vetor de comprimento 2 que indica o range do tamanho das palavras     
                min.freq = freq,                                      # Input informado pelo usuario
                max.words=max,                                        # Input informado pelo usuario
                colors=sentimentos$col,                               # Input informado pelo usuario
                random.order=FALSE,                                   # Plot das palavras em ordem aleatória. Se falso, eles serão plotados em frequência decrescente
                rot.per=(1-horizontal),                               # Proporção de palavras com rotação de 90 graus
                use.r.layout=FALSE,                                   # Se falso, o código c ++ é usado para detecção de colisão, caso contrário, R é usado
                #family = "C",                                         # Seleciona a fonte informada em windowsFonts no inicio do documento
                font = 2)                                             # 1:default, 2:negrito, 3:italico, 4:negrito+italico
      
      return(freq = d)                                                # Retorna os termos detectados ordenados de acordo com a sua frequencia
      
      #Se a analise de sentimentos nao for selecionada  
    }else{
      #dev.new(width=10, height=10)
      par(bg="#f2f2f2")                                               # Define a cor do fundo da figura
      set.seed(1234)                                                  # Semente para gerar a mesma nuvem
      wordcloud(names(v), freq=as.numeric(v),                         # As entradas serao as mesmas mensionadas acima, com excessao da cor
                scale=c(4,0.5),                                       # Um vetor de comprimento 2 que indica o range do tamanho das palavras   
                min.freq = freq,                                      # Input informado pelo usuario
                max.words=max,                                        # Input informado pelo usuario
                colors=c("#dd003f","#002637","#0bdc99","#00bac5","#ff8947","#c20037","#00a2ab")%>%  #Cores desejadas
                  rev(),                                              # Altera a ordem que as cores participarao da figura
                random.order=FALSE,                                   # Plot das palavras em ordem aleatória. Se falso, eles serão plotados em frequência decrescente
                rot.per=(1-horizontal),                               # Proporção de palavras com rotação de 90 graus
                use.r.layout=FALSE,                                   # Se falso, o código c ++ é usado para detecção de colisão, caso contrário, R é usado
                #family = "C",                                         # Seleciona a fonte informada em windowsFonts no inicio do documento 
                font = 2)                                             # 1:default, 2:negrito, 3:italico, 4:negrito+italico
      
      return(freq = d)                                                # Retorna os termos detectados ordenados de acordo com a sua frequencia
      
    }
    
  }else{
    return(freq = d)}                                                 # Retorna os termos detectados ordenados de acordo com a sua frequencia
  
}




# # Testando a funcao: -----------------------------------------------------
# 
# library(readr)                # Carregando pacote para leitura do arquivo
# base <- read_csv("base.csv")  # Leitura do arquivo exemplo disponível em: https://github.com/gomesfellipe/appwordcloud/
# base=base[,2]                 # Selecionando apenas a coluna do texto
# 
# # Obtendo nuvem e salvando tabela num objeto com nome teste:
# teste=wordcloud_sentiment(base,                                       
#                           type = "text",
#                           sentiment = F,
#                           excludeWords = c("nao"),
#                           ngrams = 3,
#                           tf_idf = T,
#                           max = 100,
#                           freq = 10,
#                           horizontal = 1,
#                           textStemming = F,
#                           print=T)
# # Conferindo o conteudo do objeto criado:
# teste
# 
# #Com url:
# wordcloud_sentiment(x="https://support.rstudio.com/hc/en-us/articles/200714013-Slide-Transitions-and-Navigation",type="url")



#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+                               Download e analise de webpage                                  +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
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
#Fonte: http://www.sthda.com/english/wiki/print.php?id=159
#-----------------------------------------------------------------------------------------------


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+                         Captação de erros de codificacao:                                    +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
catch.error = function(x){
  y = NA                                                     # Cria um vetor com valor faltante para teste
  catch_error = tryCatch(tolower(x), error=function(e) e)    # Tente pegar esse erro (NA) que acabamos de criar
  if (!inherits(catch_error, "error"))                       # Se não for um erro
    y = tolower(x)                                           # verificar resultado se houver erro, caso contrário, a função funciona normalmente
  return(y)
}
#Fonte: https://sites.google.com/site/miningtwitter/questions/talking-about/given-topic
#-----------------------------------------------------------------------------------------------


#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+                            Limpeza de caracteres especiais                                   +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cleanTweets<- function(tweet){
  
  # Limpe o tweet para análise de sentimentos
  
  tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)  # Remove html links
  tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)       # Remove retweet 
  tweet = gsub("#\\w+", " ", tweet)                             # Remove todos "#Hashtag"
  tweet = gsub("@\\w+", " ", tweet)                             # Remove todos "@people"
  tweet = gsub("[[:punct:]]", " ", tweet)                       # Remove todas as pontuacoes
  tweet = gsub("[[:digit:]]", " ", tweet)                       # Remover numeros, precisamos apenas de texto para análise
  
  tweet = gsub("[ \t]{2,}", " ", tweet)                         # Remove espaços desnecessarios
  tweet = gsub("^\\s+|\\s+$", "", tweet)                        # (espacos em branco, tabs etc)
  
  tweet = gsub('https://','',tweet)                             # Remove https://
  tweet = gsub('http://','',tweet)                              # Remove http://
  tweet = gsub('[^[:graph:]]', ' ',tweet)                       # Remove strings gráficos como emoticons
  tweet = gsub('[[:punct:]]', '', tweet)                        # Remove pontuacao 
  tweet = gsub('[[:cntrl:]]', '', tweet)                        # Remove strings de controle
  tweet = gsub('\\d+', '', tweet)                               # Remove numeros
  tweet=str_replace_all(tweet,"[^[:graph:]]", " ")              # Remove strings gráficos como emoticons
  #tweet=SnowballC::wordStem(tweet,language = lang)     # Aplica steamming (desativado) 
  
  #Converte tudo para minusculo 
  tweet = catch.error(tweet)                                    # Aplica a funcao catch.error 
  
  return(tweet)
}
#Referencia: https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud
#-----------------------------------------------------------------------------------------------
