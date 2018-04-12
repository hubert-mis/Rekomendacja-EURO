library(plyr)
library(dplyr)
library(ggplot2)
library(quantmod)
library(zoo)
library(xts)
library(forecast)
library(imputeTS)
source("Wczyt_danych.R")


euro_pl_data <- euro_pl_data[,-c(3,4,5)]
euro_pl_data$Data <- as.Date(euro_pl_data$Data, "%Y-%m-%d")
euro_pl_data <- euro_pl_data[which(format(euro_pl_data$Data, "%Y") >= 2005),]
rownames(euro_pl_data) <- 1:nrow(euro_pl_data)


infl_pl_data <- infl_pl_data[,-c(3,4,5)]
infl_pl_data$Data <- as.Date(infl_pl_data$Data, "%Y-%m-%d")
infl_pl_data <- infl_pl_data[which(format(infl_pl_data$Data, "%Y") >= 2005),]
rownames(infl_pl_data) <- 1:nrow(infl_pl_data)


unempl_pl_data <- unempl_pl_data[,-c(3,4,5)]
unempl_pl_data$Data <- as.Date(unempl_pl_data$Data, "%Y-%m-%d")
unempl_pl_data <- unempl_pl_data[which(format(unempl_pl_data$Data, "%Y") >= 2005),]
rownames(unempl_pl_data) <- 1:nrow(unempl_pl_data)

dates <- data.frame(Data = euro_pl_data$Data)

inflation <- full_join(infl_pl_data, dates, on="Data")
inflation <- inflation[order(as.Date(inflation$Data)),]

unemployment <- full_join(unempl_pl_data, dates, on="Data")
unemployment <- unemployment[order(as.Date(unemployment$Data)),]

inflation$Otwarcie <- na.ma(inflation$Otwarcie)
inflation$Otwarcie <- round(inflation$Otwarcie,3)
colnames(inflation) <- c("Date", "Value")

unemployment$Otwarcie <- na.ma(unemployment$Otwarcie)
unemployment$Otwarcie <- round(unemployment$Otwarcie,3)
colnames(unemployment) <- c("Date", "Value")
colnames(euro_pl_data) <- c("Date", "Value")

write.csv(inflation, "inflation_pl.csv", row.names = FALSE)
write.csv(unemployment, "unemployment_pl.csv", row.names = FALSE)
write.csv(euro_pl_data, "euro_pl.csv", row.names = FALSE)


