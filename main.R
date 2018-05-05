#ANÁLISIS DE SENTIMIENTOS CON R

# 1. Instalación de paquetes necesarios
install.packages("installr")
install.packages("ROAuth")
install.packages("twitteR")
install.packages("httr")
install.packages("tm") #cargamos la librería para hacer text mining
install.packages("NLP")
install.packages("wordcloud")
install.packages("NLP") # Paquete tm: framework para aplicaciones de minería de textos
install.packages("tm")
install.packages("RTextTools") # Paquete RTextTools: Clasificación Automática de Textos vía Aprendizaje Supervisado
install.packages("plotrix")
install.packages("plyr")
install.packages("stringr")
install.packages("reshape")

# 2. Carga de paquetes
library(installr)
library(ROAuth)
library(twitteR)
library(httr)
library(NLP)
library(tm)
library(RTextTools)
library(plyr)
library(stringr)
library(reshape)
library(plotrix)
library(wordcloud)
library(tm)
library(NLP)
library(wordcloud)

# Asignar directorio de trabajo
setwd("C:/Users/Administrador/Documents/AnalisisDeSentimientos")

# 3. Establecemos la autentificación
consumer_key <-"K5JS249HRzAq9KA41tdErOCXf"
consumer_secret <- "ZPEKebr0SzFvJdx0M8nhJeR2DGblqmNMtaqNzV9R3QERdoQlxe"
access_token<-"989550142662508545-lGPA3XaQOUjLTDshUSXLzktLzkl3UDW"
access_secret <- "n9aLJbe63oUvtKweYnzMl1xHH4wNzp8dVMFjh1tSqsjhW"

setup_twitter_oauth(consumer_key ,consumer_secret, access_token,  access_secret )


# 4. Extracción de la Data
#  Extraemos los tweets
# ejemplos:
# mudialbrasil.tweets=searchTwitter('world cup+brazil', resultType="popular", n=15)
#trump.tweets=searchTwitter('trump',since='2011-03-01', until='2018-03-02')
#AlejandroToledo.tweets=searchTwitter("AlejandroToledo",n=150)
humala.tweets=searchTwitter("humala",n=500)

head(humala.tweets)
class(humala.tweets)

# 5. Extructuración y limpieza de la data


#Convertimos en un dataframe

#La función lapply lo que hace es aplicar la función definida en el argumento a cada elemento de la lista
df<-do.call("rbind",lapply(humala.tweets,as.data.frame))

#Verificamos que la conversión del tipo de variable sea un dataframe
class(df)

#Si queremos ver el nombre de las variables que tiene nuestro dataframe
dimnames(df)

df$text[]
df$favorited[]
df$favoriteCount[]
df$replyToSN[]
df$created[]
df$truncated[]
df$replyToSID[]

df$statusSource[]
df$screenName[]
df$isRetweet[]
df$latitude[]
df$longitude[]


#text: Texto del tweet
#favorited: Es favorito? Verdadero o Falso
#favoriteCount: El número de favoritos
#replyToSN: Nombre del usuario a quien responde
#created: La fecha de creación
#truncated: Si el tweet fue truncado
#replyToSID: 
#id: id del tweet
#replyToUID: ID del usuario a quien responde
#statusSource: Fuente de Origen de usuario para el tweet
#screenName: Nombre del usuario que posteó el tweet
#retweetCount: Número de retweets
#isRetweet: Es retweet? Verdadero o Falso
#retweeted: Es retweeteado por otro usuario? Verdadero o Falso
#longitude: sur(negativo), norte(positivo)
#latitude: este(positivo) oeste(negativo)

#hacemos limpieza de texto
df$text<- sapply(df$text, function(row) iconv(row,"latin1","ASCII",sub=""))

#Reemplazamos por vacío algunos caracteres
df$text=gsub("(f|ht)tp(s?)://(.*)[.][a-z]+","",df$text)

head(df$text)

sample<-df$text

head(sample)


#6. Análisis de Sentimientos

#Word Database.R

#Guardamos las palabras positivas y negativas en variables
pos.words = scan('positive-words.txt', what='character', comment.char=';')
neg.words = scan('negative-words.txt', what='character', comment.char=';')


#Añadimos palabras negativas y postivas
pos.words=c(pos.words)
neg.words = c(neg.words)

#score_sentiment.R
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  list=lapply(sentences, function(sentence, pos.words, neg.words)
  {
    sentence = gsub('[[:punct:]]','',sentence)
    ##[[:punct:]]	Match a punctuation character: [-!"#$%&'()*+,./:;<=>?@[\]_`{
    sentence = gsub('[[:cntrl:]]','',sentence)
    ##[[:cntrl:]]	Cuando encuentra un caracter de control
    sentence = gsub('\\d+','',sentence)
    ##\d	Cuando encuentra números
    sentence = gsub('\n','',sentence)
    ##\n cuando encuentra un salto de linea
    
    sentence = tolower(sentence)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)#la posición de la primera ocurrencia, si no encuentra da NA
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    pp=sum(pos.matches)
    nn = sum(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    list1=c(score, pp, nn)
    return (list1)
  }, pos.words, neg.words)
  score_new=lapply(list, `[[`, 1)
  pp1=score=lapply(list, `[[`, 2)
  nn1=score=lapply(list, `[[`, 3)
  
  scores.df = data.frame(score=score_new, text=sentences)
  positive.df = data.frame(Positive=pp1, text=sentences)
  negative.df = data.frame(Negative=nn1, text=sentences)
  
  list_df=list(scores.df, positive.df, negative.df)
  return(list_df)
}



result = score.sentiment(sample, pos.words, neg.words)
head(result)

#Creando una copia del resultado del data frame
test1=result[[1]]
test2=result[[2]]
test3=result[[3]]

#Creación de dataframes
test1$text=NULL
test2$text=NULL
test3$text=NULL
#Contiene Score de Sentimientos
q1=test1[1,]
q2=test2[1,]
q3=test3[1,]
qq1=melt(q1, ,var='Score')
qq2=melt(q2, ,var='Positive')
qq3=melt(q3, ,var='Negative') 
qq1['Score'] = NULL
qq2['Positive'] = NULL
qq3['Negative'] = NULL
#Creamos el dataframe
table1 = data.frame(Text=result[[1]]$text, Score=qq1)
table2 = data.frame(Text=result[[2]]$text, Score=qq2)
table3 = data.frame(Text=result[[3]]$text, Score=qq3)

#Convertimos todo en un solo dataframe
table_final=data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)


table_final

#Porcentajes en R

#Positive Percentage

#Renombramos
posSc=table_final$Positive
negSc=table_final$Negative

#Añadimos columna
table_final$PosPercent = posSc/ (posSc+negSc)

#Reemplazamos Nan con cero
pp = table_final$PosPercent
pp[is.nan(pp)] <- 0
table_final$PosPercent = pp

#Negative Percentage

#Añadimos Columna
table_final$NegPercent = negSc/ (posSc+negSc)

#Reemplazamos Nan con cero
nn = table_final$NegPercent
nn[is.nan(nn)] <- 0
table_final$NegPercent = nn

dimnames(table_final)


#Gráficos en R

#Histogram
windows()
hist(table_final$Positive, col=rainbow(10))
windows()
hist(table_final$Negative, col=rainbow(10))
windows()
hist(table_final$Score, col=rainbow(10))

#Pie
slices <- c(sum(table_final$Positive), sum(table_final$Negative))
labels <- c("Positive", "Negative")


windows()
pie(slices, labels = labels, col=rainbow(length(labels)), main="Sentiment Analysis")

windows()
pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")


#Buenos

Sc = table_final$Score

#Output of following is FALSE or TRUE
good <- sapply(Sc, function(Sc) Sc <= 3 && Sc > 0)
head(good)

#Convertimos al valor actual
Sc[good]
list_good = Sc[good]
value_good = length(list_good)

#Muy Buenos

vgood <- sapply(Sc, function(Sc) Sc > 3 && Sc <6)
#Convertimos al valor actual
Sc[vgood]
list_vgood = Sc[vgood]
value_vgood = length(list_vgood)

#Excepcional

vvgood <- sapply(Sc, function(Sc) Sc >= 6)
#Converts to actual value
Sc[vvgood]
list_vvgood = Sc[vvgood]
value_vvgood = length(list_vvgood)

#Malo : Insatisfacción

#Output of following is FALSE or TRUE
bad <- sapply(Sc, function(Sc) Sc >= -3 && Sc < 0)
#Converts to actual value
Sc[bad]
list_bad = Sc[bad]
value_bad = length(list_bad)

#Muy Malo : Pobre

#Output of following is FALSE or TRUE
vbad <- sapply(Sc, function(Sc) Sc < -3 && Sc > -6)
#Convertir al valor actual
Sc[vbad]
list_vbad = Sc[vbad]
value_vbad = length(list_vbad)

#Desastroso (awful)

vvbad <- sapply(Sc, function(Sc) Sc <= -6)
#Converts to actual value
Sc[vvbad]
list_vvbad = Sc[vvbad]
value_vvbad = length(list_vvbad)

#Neutral
neutral <- sapply(Sc, function(Sc) Sc == 0) 
list_neutral = Sc[neutral]
value_neutral = length(list_neutral)


slices1 <- c(value_good, value_vvbad, value_bad, value_vgood, value_vbad, value_neutral, value_vvgood )
lbls1 <- c("Good", "Awful", "Unsatisfactory", "Great", "Poor", "Neutral", "Outstanding")
pct <- round(slices1/sum(slices1)*100) #Percentage
lbls1 <- paste(lbls1, pct) # add percents to labels 
lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
windows()
pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
    main="Number of tweets with particular sentiment")


#wordcloud.R
humala.tweets = searchTwitter("humala", lang="en", n=1500, resultType="recent")
humala_text = sapply(humala.tweets, function(x) x$getText())
df <- do.call("rbind", lapply(humala.tweets, as.data.frame))
humala_text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))

#gives the summary/internal structure of an R object
str(humala_text)


#corpus es una colección de documentos tipo texto
quake_corpus <- Corpus(VectorSource(humala_text))
quake_corpus
inspect(quake_corpus[1])

#clean text
quake_clean <- tm_map(quake_corpus, removePunctuation)
quake_clean <- tm_map(quake_clean, content_transformer(tolower))
quake_clean <- tm_map(quake_clean, removeWords, stopwords("english"))
quake_clean <- tm_map(quake_clean, removeNumbers)
quake_clean <- tm_map(quake_clean, stripWhitespace)
#quake_clean <- tm_map(quake_clean, removeWords, c("humala","que","del","los","las","con","para","por"))#removing search words

windows()
wordcloud(quake_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(4,0.5))