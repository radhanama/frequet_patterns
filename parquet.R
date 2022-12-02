if(!require('xml2')) {
  install.packages('xml2')
  library('xml2')
}
if(!require('arrow')) {
  install.packages('arrow')
  library('arrow')
}
if(!require('arules')) {
  install.packages('arules')
  library('arules')
}
if(!require('chron')) {
  install.packages('chron')
  library('chron')
}
if(!require('dplyr')) {
  install.packages('dplyr')
  library("dplyr")
}
if(!require('arulesViz')) {
  install.packages('arulesViz')
  library("arulesViz")
}

rangeVelocity <- function(v){
  if(v==0){
    return("parado")
  }else if(0<v && v<20){
    return("muito lento")
  }else if(20<=v && v<40){
    return("lento")
  }else if(40<=v && v<60){
    return("normal")
  }else if(60<=v && v<80){
    return("pouco normal")
  }else if(80<=v && v<110){
    return("rapido")
  }else{
    return("muito rapido")
  }
}

rangeTimeOfDay <- function(v){
  if(5<=v && v<9){
    return("early morning")
  }else if(9<=v && v<11){
    return("mid-morning")
  }else if(11<=v && v<13){
    return("late morning")
  }else if(13<=v && v<17){
    return("afternoon")
  }else if(17<=v && v<20){
    return("early evening")
  }else if(20<=v && v<23){
    return("late evening")
  }else{
    return("night")
  }
}
diretorio_busdata <- "data"

applysplt <- function(v){
  dtparts <- strsplit(v,' ')
  date <- chron(dates=dtparts[[1]][1],times=dtparts[[1]][2],format=c('m-d-y','h:m:s'))
  return(date) 
}

diretorio_busdata <- "data"

df <- list.files(diretorio_busdata) %>%
  enframe(value = "arquivo") %>%
  rowwise() %>%
  mutate(
    conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())
  ) %>%
  unnest(conteudo) %>%
  janitor::clean_names()


## Discretizing date and time.
df$date <- mapply(applysplt, df$DATE)
df$year <- cut(df$date, "year")
df$month <- cut(df$date, "month")
df$day <- cut(df$date, "day")
df$weekend <- is.weekend(df$date)
df$weekdays <- weekdays(df$date)
df$hour <- hours(df$date)
df$time <- mapply(rangeTimeOfDay, hours(df$date))
df$velocit <- mapply(rangeVelocity, df$VELOCITY)

## calculate velocity.


## division of areas.


##tentar identificar se é ida ou volta do onibus. se ele ta indo em direção a tal lugar ou o inverso
# talvez a partir da continuidade da latitude e da longitude

## Select Data
td <- df[df$velocit != "parado",c("velocit", "CODBAIRRO", "time", "weekdays")]

## Apriori.
rulesWithParameters <- apriori(td, parameter = list(sup = 0.0005, conf = 0.8))
rules <- apriori(td)
inspect(rulesWithParameters)
