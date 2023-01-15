## Import and downloading libraries
if(!require('xml2')) {
  install.packages('xml2')
  library('xml2')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library("dplyr")
}
if(!require('tidyr')) {
  install.packages('tidyr')
  library("tidyr")
}
if(!require('tibble')) {
  install.packages('tibble')
  library("tibble")
}
if(!require('stringr')) {
  install.packages('stringr')
  library("stringr")
}
if(!require('unglue')) {
  install.packages('unglue')
  library("unglue")
}
if(!require('janitor')) {
  install.packages('janitor')
  library("janitor")
}

library('arrow')

library('arules')

library('chron')

library("arulesViz")

## Libs ogasawara version 1.5
source("https://raw.githubusercontent.com/eogasawara/mylibrary/cd74dc4be718c72a059b3bde0f19b5cf961a2bec/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/cd74dc4be718c72a059b3bde0f19b5cf961a2bec/myPreprocessing.R")

## Smoothing functions from ogasawara
explore_smoothing <- function(obj, data, attribute) {
  obj <- fit(obj, data)
  sl.bi <- transform(obj, data)
  print(table(sl.bi))
  
  entro <- smoothing_evaluation(as.factor(names(sl.bi)), attribute)
  print(entro$entropy)
  return(table(sl.bi))
}


## Charge the data of all files that we downloaded priviously in the download_parquet.py.
diretorio_busdata <- "data"
df <- list.files(diretorio_busdata) %>%
  enframe(value = "arquivo") %>%
  rowwise() %>%
  mutate(conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())) %>%
  unnest(conteudo) %>%
  janitor::clean_names()


## Range das velocidades usando smoothing
rangeVelocityTable <- explore_smoothing(smoothing_freq(n=8), df$velocity, df$velocity)
rangeVelocityArray <- as.integer(names(rangeVelocityTable))

## plot da velocidade
# options(scipen=999)
# barplot(rangeVelocityArray)

# conversão do range de velocidade para números para discretizar
discretyzeVelocity <- function(v){
    if(v==0){
      return("parado")
    }else if(0<v && v<rangeVelocityArray[2]){
      return("muito lento")
    }else if(rangeVelocityArray[2]<v && v<rangeVelocityArray[3]){
      return("lento")
    }else if(rangeVelocityArray[3]<v && v<rangeVelocityArray[4]){
      return("normal")
    }else if(rangeVelocityArray[4]<v && v<rangeVelocityArray[5]){
      return("rapido")
    }else if(rangeVelocityArray[5]<v && v<rangeVelocityArray[]){
      return("muito rapido")
    }else{
      return("muito rapido++")
    }
}


## transforming the data of time in a object of datetime to extract features.
dtparts = t(as.data.frame(strsplit(df$date,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('m-d-y','h:m:s'))

## spliting for day of the week.
df$weekdays <- weekdays(thetimes)

## Discretizing time in 1 hour intervals
df$timeSlot <- as.character(hours(thetimes))

## Discretizing velocity.
df$velocit <- mapply(discretyzeVelocity, df$velocity)

##lines to be analized.
lines <- c("363.0", "383.0", "353.0", "239.0", "249.0", "457.0", "247.0", "621.0", "627.0", "629.0", "455.0", "711.0")

## Select columns for generate rules. removing data when the buss was stoped.
td <- df[df$line %in% lines,c("timeSlot", "weekdays", "velocit","nome")]

## Apriori com o lado direito fixado como as velocidades.
rules <- apriori(
  td,
  parameter = list(
    sup = 0.000005,
    conf = 0.60,
    minlen = 4),
  appearance = list(
    rhs=c(
      "velocit=rapido",
      "velocit=normal",
      "velocit=muito lento",
      "velocit=lento",
      "velocit=parado")))

## function that generate a html for analize the rules created by apriori.
inspectDT(rules)

