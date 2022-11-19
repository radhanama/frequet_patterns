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

file_url <- "https://mapmob.eic.cefet-rj.br/data/busdata/database/G1-2022-01-01.parquet"
tf <- "data.parquet"
download.file(file_url, destfile =  tf)
df <- read_parquet(tf)
head(df)
df

dtimes = df$DATE
dtparts = t(as.data.frame(strsplit(dtimes,' ')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],format=c('m-d-y','h:m:s'))
thetimes



dataframeName$datetime <- as.factor(datataframeName$DATE)

apply(df["DATE"], MARGIN=2,FUN=as.Date)

df$year <- cut(thetimes, "year")
df$month <- cut(thetimes, "month")
df$day <- cut(thetimes, "day")
df$weekdays <- weekdays(thetimes)
df$hours <- hours(thetimes)
df$minutes <- minutes(thetimes)
df$seconds <- seconds(thetimes)


rules <- apriori(df,parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)