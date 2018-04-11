library(dplyr)
library(ggplot2)
library(quantmod)
library(zoo)
library(xts)
library(forecast)
source("Wczyt_danych.R")

euro_pl_data <- euro_pl_data[,-c(3,4,5)]
euro_pl_data$Data <- as.Date(euro_pl_data$Data, "%Y-%m-%d")
euro_pl_data <- euro_pl_data[which(format(euro_pl_data$Data, "%Y") > 2004),]
plot(euro_pl_data$Data, euro_pl_data$Otwarcie, type="l")

years <- format(euro_pl_data$Data, "%Y")
tab <- table(years)
tab
mean(tab[1:(length(tab) - 1)])

tsEuro <- ts(euro_pl_data$Otwarcie, start = 1, frequency = 256)
fit.Stl <- stl(tsEuro, s.window = 256)
sts <- fit.Stl$time.series
trend <- sts[, "trend"]
prediction <- forecast(fit.Stl, h=90, level = 95)
plot(prediction)
pred <- prediction$mean
upper <- prediction$upper
lower <- prediction$lower
result <- data.frame(actual = c(euro_pl_data$Otwarcie, rep(NA, 90)), 
                     trend = c(trend, rep(NA, 90)), pred = c(rep(NA, nrow(euro_pl_data)), pred)
                     , lower = c(rep(NA,nrow(euro_pl_data)), lower), upper = c(rep(NA, nrow(euro_pl_data)), upper)
                     ,date = c(euro_pl_data$Data, max(euro_pl_data$Data) + (1:90)))

result <- result[order(result$date), ]
max.val <- max(c(result$actual, result$upper), na.rm = T)
min.val <- min(c(result$actual, result$lower), na.rm = T)
plot(result$date, result$actual, type = "l", col = "grey", main = "Euro exchange rate prediction", 
xlab = "Time", ylab = "Exchange Rate", xlim = range(result$date), 
ylim = c(min.val, max.val))
grid()
lines(result$date, result$trend, col = "yellowgreen")
lines(result$date, result$pred, col = "green")
lines(result$date, result$lower, col = "blue")
lines(result$date, result$upper, col = "blue")
legend("bottomleft", col = c("grey", "yellowgreen", "green", 
"blue"), lty = 1, c("Actual", "Trend", "Forecast", "Lower/Upper Bound"))

