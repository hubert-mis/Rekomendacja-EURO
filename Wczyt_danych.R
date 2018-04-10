script.dir <- dirname(sys.frame(1)$ofile)
setwd(script.dir)

#GDP Poland
url <- "https://stooq.pl/q/d/l/?s=gdpypl.m&i=d"
destfile <- "gdp_pl.csv"
tryCatch(download.file(url, destfile, mode="wb"),
         error = function(e){
           warning("Couldn't download the newest version of GDP historical values. Loading the existing one..")
         })
gdp_pl_data <- read.csv("gdp_pl.csv", header = TRUE, sep = ",", dec = ".")

#Unemployment Poland
url <- "https://stooq.pl/q/d/l/?s=unrtpl.m&i=q"
destfile <- "unempl_pl.csv"
tryCatch(download.file(url, destfile, mode="wb"),
         error = function(e){
           warning("Couldn't download the newest version of unemployment historical values. Loading the existing one..")
         })
unempl_pl_data <- read.csv("unempl_pl.csv", header = TRUE, sep = ",", dec = ".")

#Inflation Poland
url <- "https://stooq.pl/q/d/l/?s=cpiypl.m&i=q"
destfile <- "infl_pl.csv"
tryCatch(download.file(url, destfile, mode="wb"),
         error = function(e){
           warning("Couldn't download the newest version of inflation historical values. Loading the existing one..")
         })
infl_pl_data <- read.csv("infl_pl.csv", header = TRUE, sep = ",", dec = ".")

#Historical rates EUR/PLN
url <- "https://stooq.pl/q/d/l/?s=eurpln&i=d"
destfile <- "euro_pl.csv"
tryCatch(download.file(url, destfile, mode="wb"),
         error = function(e){
           warning("Couldn't download the newest version of EUR/PLN rates historical values. Loading the existing one..")
         })
euro_pl_data <- read.csv("euro_pl.csv", header = TRUE, sep = ",", dec = ".")
