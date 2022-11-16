library('xml2')
if(!require('arrow')) {
  install.packages('arrow')
  library('arrow')
}
library('arrow')
if(!require('arules')) {
  install.packages('arules')
  library('arules')
}

file_url <- "https://mapmob.eic.cefet-rj.br/data/busdata/database/G1-2022-01-01.parquet"
tf <- "data.parquet"
download.file(file_url, destfile =  tf)
df <- read_parquet(tf)
head(df)
df

rules <- apriori(df,parameter = list(supp = 0.5, conf = 0.9, target = "rules"))
summary(rules)