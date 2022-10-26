library('xml2')
library('arrow')

file_url <- "https://mapmob.eic.cefet-rj.br/data/busdata/G1-2022-01-01.parquet"
tf <- "data.parquet"
download.file(file_url, destfile =  tf)
df <- read_parquet(tf)
head(df)

realengo <- df[LINE == "383.0"]
