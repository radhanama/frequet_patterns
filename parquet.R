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

##Download File.
file_url <- "https://mapmob.eic.cefet-rj.br/data/busdata/database/G1-2022-01-01.parquet"
tf <- "data.parquet"
download.file(file_url, destfile =  tf)
df <- read_parquet(tf)
head(df)
df

##Discretizing date and time.
dtimes = df$DATE
dtparts = t(as.data.frame(strsplit(dtimes,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('m-d-y','h:m:s'))
df$year <- cut(thetimes, "year")
df$month <- cut(thetimes, "month")
df$day <- cut(thetimes, "day")
df$weekdays <- weekdays(thetimes)
df$hours <- hours(thetimes)
df$minutes <- minutes(thetimes)
df$seconds <- seconds(thetimes)

td <- df["year","day"]

##Apriori.
rules <- apriori(df,parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)