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

#libs ogasawara version 1.5
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myBasic.R")
source("https://raw.githubusercontent.com/eogasawara/mylibrary/master/myPreprocessing.R")

#smoothing functions from ogasawara
explore_smoothing <- function(obj, data, attribute) {
  obj <- fit(obj, data)
  sl.bi <- transform(obj, data)
  print(table(sl.bi))
  
  entro <- smoothing_evaluation(as.factor(names(sl.bi)), attribute)
  print(entro$entropy)
  return(table(sl.bi))
}

optimize_smoothing <- function(obj, data, attribute) {
  obj <- optimize(obj, data, do_plot=TRUE)
  explore_smoothing(obj, data, attribute)
}


## function use to set ranges of velocity.
# rangeVelocity <- function(v){
#   if(v==0){
#     return("parado")
#   }else if(0<v && v<20){
#     return("muito lento")
#   }else if(20<=v && v<40){
#     return("lento")
#   }else if(40<=v && v<60){
#     return("normal")
#   }else if(60<=v && v<80){
#     return("pouco normal")
#   }else if(80<=v && v<110){
#     return("rapido")
#   }else{
#     return("muito rapido")
#   }
# }

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


# ## Charge data.
# diretorio_busdata <- "data"
# df <- list.files(diretorio_busdata) %>%
#   enframe(value = "arquivo") %>%
#   rowwise() %>%
#   mutate(conteudo = list(str_glue("{diretorio_busdata}/{arquivo}") %>% read_parquet())) %>%
#   unnest(conteudo) %>%
#   janitor::clean_names()


# Download File.
file_url <- "https://mapmob.eic.cefet-rj.br/data/busdata/database/G1-2022-01-01.parquet"
tf <- "data.parquet"
download.file(file_url, destfile =  tf)
df <- read_parquet(tf)

#range das velocidades usando smoothing
rangeVelocity <- explore_smoothing(smoothing_freq(n=20), df$VELOCITY, df$VELOCITY)
#rangeVelocity <- optimize_smoothing(smoothing_freq(n=20), df$VELOCITY, df$VELOCITY)
rangeVelocityArray <- names(rangeVelocity)
discretyzeVelocity <- function(v){
    if(v==rangeVelocityArray[1]){
      return("parado")
    }else if(rangeVelocityArray[1]<v && v<rangeVelocityArray[2]){
      return("muito lento")
    }else if(rangeVelocityArray[2]<v && v<rangeVelocityArray[3]){
      return("lento")
    }else if(rangeVelocityArray[3]<v && v<rangeVelocityArray[4]){
      return("normal")
    }else if(rangeVelocityArray[4]<v && v<rangeVelocityArray[5]){
      return("pouco acima do normal")
    }else if(rangeVelocityArray[5]<v && v<rangeVelocityArray[]){
      return("rapido")
    }else{
      return("muito rapido")
    }
}

## Discretizing date and time.
dtparts = t(as.data.frame(strsplit(df$DATE,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('m-d-y','h:m:s'))
df$year <- cut(thetimes, "year")
df$month <- months(thetimes)
df$day <- days(thetimes)
df$weekend <- is.weekend(thetimes)
df$weekdays <- weekdays(thetimes)
df$hour <- hours(thetimes) %>% as.character()
df$time <- mapply(rangeTimeOfDay, hours(thetimes))

## discretizing velocity.
df$velocit <- mapply(discretyzeVelocity, df$VELOCITY)

## Select columns for generate rules. removing data when the buss was stoped.
td <- df[df$velocit != "parado",c("velocit", "NOME", "hour", "weekdays")]

## Apriori.
rules <- apriori(td, parameter = list(sup = 0.000000005, conf = 0.60))
inspect(rules)
