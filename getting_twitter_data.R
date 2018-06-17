# Importando to twitter

#Clear R Environment
rm(list=ls())

#Load required libraries
# install.packages("twitteR")
# install.packages("ROAuth")

library("twitteR")
library("ROAuth")

# Download the file and store in your working directory
download.file(url= "http://curl.haxx.se/ca/cacert.pem", destfile= "cacert.pem")

#Insert your consumerKey and consumerSecret below
credentials <- OAuthFactory$new(consumerKey='consumerKey',
                                consumerSecret='consumerSecret',
                                requestURL='requestURL',
                                accessURL='accessURL',
                                authURL='authURL')

credentials$handshake(cainfo="cacert.pem")
#escrever o PIN:

save(credentials, file="twitter authentication.Rdata")

#Load Authentication Data
load("twitter authentication.Rdata")

#Register Twitter Authentication
setup_twitter_oauth(credentials$consumerKey, credentials$consumerSecret, credentials$oauthKey, credentials$oauthSecret)

#Obtendo dados:
library(plyr)
tweets_br <- searchTwitter('brasil copa', n=4000, lang="pt")
Tweets.br = laply(tweets_br,function(t)t$getText()) #pega somente os textos dos Tweets

tweets_arg<- searchTwitter('argentina copa', n=4000, lang="pt")
Tweets.arg = laply(tweets_arg,function(t)t$getText()) #pega somente os textos dos Tweets

library(dplyr)
text_df_br <- data_frame(book="br",line = 1:length(Tweets.br), text = Tweets.br) %>% as_tibble()
text_df_arg <- data_frame(book="arg",line = 1:length(Tweets.arg), text = Tweets.arg) %>% as_tibble()

original_books=bind_rows(text_df_br,text_df_arg)

write.csv(original_books,"original_books.csv",row.names = F)
