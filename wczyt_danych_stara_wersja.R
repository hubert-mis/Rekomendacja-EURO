inflation <- read.csv("U:/Informatyka Ekonomiczna/Projekt/inflacja_s.csv")
inflation <- inflation[-(3:5)]
colnames(inflation)[2] <- "Inflation Rate"

unemployment <- read.csv("U:/Informatyka Ekonomiczna/Projekt/bezrobocie.csv")
unemployment <- unemployment[-(3:5)]
colnames(unemployment)[2] <- "Unemployment Rate"

gdp <- read.csv("U:/Informatyka Ekonomiczna/Projekt/pkb.csv")
gdp <- gdp[-(3:5)]
colnames(gdp)[2] <- "Gross Domestic Product"
#STARA WERSJA
