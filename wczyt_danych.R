inflation <- read.csv("U:/Informatyka Ekonomiczna/Projekt/inflacja_s.csv")
inflation <- inflation[-(3:5)]
colnames(inflation)[2] <- "Inflation Rate"

unemployment <- read.csv("U:/Informatyka Ekonomiczna/Projekt/bezrobocie.csv")
unemployment <- unemployment[-(3:5)]
colnames(unemployment)[2] <- "Unemployment Rate"

gdp <- read.csv("U:/Informatyka Ekonomiczna/Projekt/pkb.csv")
gdp <- gdp[-(3:5)]
colnames(gdp)[2] <- "Gross Domestic Product"


################## 
library(RJSONIO)
library(WDI)

WDI(country = "PL", indicator = "NY.GDP.MKTP.CD", end = 2018, start = 2010)

library(OECD)

dataset <- "DP_LIVE"
datastruc <- get_data_structure(dataset)
str(a2, max.level = 1)
datastruc$TIME


temp <- get_dataset(dataset = dataset, filter = list(c("POL", "EA19"), c("CPI"), c("TOT")))
