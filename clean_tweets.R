#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#+                            Limpeza de caracteres especiais                                   +
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
clean_tweets<- function(tweet){
library(stringr)
  
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
  tweet = catch_error(tweet)                                    # Aplica a funcao catch.error 
  
  return(tweet)
}
#Referencia: https://sites.google.com/site/miningtwitter/questions/talking-about/wordclouds/comparison-cloud
#-----------------------------------------------------------------------------------------------

