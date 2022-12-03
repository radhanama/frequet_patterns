## import libraries
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

## function use to set ranges of velocity.
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

## function use to set ranges of time
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

## Charge data.
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
dtparts = t(as.data.frame(strsplit(df$date,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('m-d-y','h:m:s'))
df$year <- cut(thetimes, "year")
df$month <- months(thetimes)
df$day <- days(thetimes)
df$weekend <- is.weekend(thetimes)
df$weekdays <- weekdays(thetimes)
df$hour <- hours(thetimes)
df$time <- mapply(rangeTimeOfDay, hours(thetimes))

## discretizing velocity.
df$velocit <- mapply(rangeVelocity, df$velocity)

## Select columns for generate rules. removing data when the buss was stoped.
td <- df[df$velocit != "parado",c("velocit", "codbairro", "hour", "weekdays")]

## Apriori.
rulesWithParameters <- apriori(td, parameter = list(sup = 0.00005, conf = 0.8))
inspect(rulesWithParameters)
