#ANÁLISIS DE SENTIMIENTOS CON R
# Instalación y carga de paquetes necesarios
list.of.packages <- c("ROAuth", "httr", "tm", "NLP", "wordcloud", 
                      "NLP", "tm", "RTextTools", "plotrix", 
                      "plyr", "stringr", "reshape")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# Carga de paquetes
library(ROAuth)
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

# Funciones
exit <- function() {
  .Internal(.invokeRestart(list(NULL, NULL), NULL))
}  
openGraph <- function(width=7 , height=7 , ... ) {
  if ( .Platform$OS.type != "windows" ) { # Mac OS, Linux
    quartz( width=width , height=height ) 
  } else { # Windows OS
    windows( width=width , height=height , ... )
  }
}

# Score_sentiment.R
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

dataAnalisis <- function(file, pageTitle, removeStopWords) {
  # Extracción de la Data
  df <- read.csv(file=file, header=TRUE, encoding = "UTF-8", sep = "|") 
  df$Resumen[]
  head(df$Resumen)
  resumen<-df$Resumen
  
  # Análisis de Sentimientos
  # Word Database.R
  # Guardamos las palabras positivas y negativas en variables
  pos.words = scan('positive-words.txt', what='character', comment.char=';')
  neg.words = scan('negative-words.txt', what='character', comment.char=';')
  
  
  # Añadimos palabras negativas y postivas
  pos.words=c(pos.words)
  neg.words = c(neg.words)
  
  result = score.sentiment(resumen, pos.words, neg.words)
  head(result)
  # Creando una copia del resultado del data frame
  test1=result[[1]]
  test2=result[[2]]
  test3=result[[3]]
  
  # Creación de dataframes
  test1$text=NULL
  test2$text=NULL
  test3$text=NULL
  
  # Contiene Score de Sentimientos
  q1=test1[1,]
  q2=test2[1,]
  q3=test3[1,]
  qq1=melt(q1, ,var='Score')
  qq2=melt(q2, ,var='Positive')
  qq3=melt(q3, ,var='Negative') 
  qq1['Score'] = NULL
  qq2['Positive'] = NULL
  qq3['Negative'] = NULL
  # Creamos el dataframe
  table1 = data.frame(Text=result[[1]]$text, Score=qq1)
  table2 = data.frame(Text=result[[2]]$text, Score=qq2)
  table3 = data.frame(Text=result[[3]]$text, Score=qq3)
  # Convertimos todo en un solo dataframe
  table_final = data.frame(Text=table1$Text, Score=table1$value, Positive=table2$value, Negative=table3$value)
  
  # Ver Score : table_final
  # View(table_final)
  # exit()
  
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
  
  # Gráficos en R
  # Abriendo una ventana
  openGraph()
  par(oma=c(0,0,2,0))# all sides have 3 lines of space
  par(mar=c(5,4,4,2) + 0.1)
  par(mfrow=c(2,3))
  
  
  #Histogram
  hist(table_final$Positive, col=rainbow(10), main = "Histograma de S. Positivos", xlab = "")
  hist(table_final$Negative, col=rainbow(10), main = "Histograma de S. Negativos", xlab = "")
  hist(table_final$Score, col=rainbow(10), main = "Histograma General", xlab = "")
  
  # Titulo de la Pagina
  title(paste('Analisis del Candidato',pageTitle), outer = TRUE)
  
  #Pie
  slices <- c(sum(table_final$Positive), sum(table_final$Negative))
  labels <- c("Positive", "Negative")
  pie(slices, labels = labels, col=rainbow(length(labels)), main="Analisis de Sentimiento")
  #pie3D(slices, labels = labels, col=rainbow(length(labels)),explode=0.00, main="Sentiment Analysis")
  
  #Buenos
  Sc <- table_final$Score
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
  lbls1 <- c("Bueno", "Horrible", "Insatisfactorio", "Excelente", "Malo", "Neutro", "Excepcional")
  pct <- round(slices1/sum(slices1)*100) #Percentage
  lbls1 <- paste(lbls1, pct) # add percents to labels 
  lbls1 <- paste(lbls1,"%",sep="") # ad % to labels 
  
  pie(slices1,labels = lbls1, col=rainbow(length(lbls1)),
      main="% Analisis de Sentimiento")
  
  
  #gives the summary/internal structure of an R object
  str(resumen)
  #corpus es una colección de documentos tipo texto
  quake_corpus <- Corpus(VectorSource(resumen))
  quake_corpus
  inspect(quake_corpus[1])
  #clean text
  quake_clean <- tm_map(quake_corpus, removePunctuation)
  quake_clean <- tm_map(quake_clean, content_transformer(tolower))
  quake_clean <- tm_map(quake_clean, removeWords, stopwords("spanish"))
  quake_clean <- tm_map(quake_clean, removeNumbers)
  quake_clean <- tm_map(quake_clean, stripWhitespace)
  quake_clean <- tm_map(quake_clean, removeWords, removeStopWords)#removing search words
  
  wordcloud(quake_clean, random.order=F,max.words=80, col=rainbow(50), scale=c(4,0.5))
}

# main
dataAnalisis("PPK.csv", pageTitle = "PPK", removeStopWords = c('toledo', 'alan', 'fujimori','ppk', 'pedro', 'pablo', 'acuña', 'kuczynski', 'keiko', 'garcía', 'césar'))