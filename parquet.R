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
    return("normal lento")
  }else if(40<=v && v<60){
    return("poco lento")
  }else if(60<=v && v<80){
    return("normal")
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

## Download File.
file_url <- "https://mapmob.eic.cefet-rj.br/data/busdata/database/G1-2022-01-01.parquet"
tf <- "data.parquet"
download.file(file_url, destfile =  tf)
df <- read_parquet(tf)

## Discretizing date and time.
dtparts = t(as.data.frame(strsplit(df$DATE,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('m-d-y','h:m:s'))
df$year <- cut(thetimes, "year")
df$month <- cut(thetimes, "month")
df$day <- cut(thetimes, "day")
df$weekdays <- weekdays(thetimes)
df$time <- mapply(rangeTimeOfDay, hours(thetimes))
df$velocit <- mapply(rangeVelocity, df$VELOCITY)

## calculate velocity.


## division of areas.


## Select Data
td <- df[df$velocit != "parado",c("velocit", "REGIAO_ADM", "CODBAIRRO", "time")]

## Apriori.
rulesWithParameters <- apriori(td, parameter = list(sup = 0.05, conf = 0.03))
rules <- apriori(td)
inspect(rulesWithParameters)
plot(rulesWithParameters) 
