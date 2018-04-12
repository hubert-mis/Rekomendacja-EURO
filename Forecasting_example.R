library(dplyr)
library(ggplot2)
library(quantmod)
library(zoo)
library(xts)
library(forecast)
source("Wczyt_danych.R")

colnames(euro_pl_data) <- c("Date", "Value")
euro_pl_data <- euro_pl_data[,-c(3,4,5)]
euro_pl_data$Date <- as.Date(euro_pl_data$Date, "%Y-%m-%d")
euro_pl_data <- euro_pl_data[which(format(euro_pl_data$Date, "%Y") > 2004),]
#plot(euro_pl_data$Data, euro_pl_data$Otwarcie, type="l")

forecast1 <- function(rates, days.ahead = 30) {
  days.ahead <<- days.ahead
  years <- format(rates$Date, "%Y")
  tab <- table(years)
  n <- round(mean(tab[1:(length(tab) - 1)]))
  tsEuro <- ts(rates$Value,
               start = 1,
               frequency = n)
  fit.Stl <- stl(tsEuro, s.window = n)
  sts <- fit.Stl$time.series
  trend <- sts[, "trend"]
  prediction <- forecast(fit.Stl, h = days.ahead, level = 95)
  #plot(prediction)
  pred <- prediction$mean
  upper <- prediction$upper
  lower <- prediction$lower
  result <-
    data.frame(
      actual = c(rates$Value, rep(NA, days.ahead)),
      trend = c(trend, rep(NA, days.ahead)),
      pred = c(rep(NA, nrow(rates)), pred)
      ,
      lower = c(rep(NA, nrow(rates)), lower),
      upper = c(rep(NA, nrow(rates)), upper)
      ,
      Date = c(rates$Date, max(rates$Date) + (1:days.ahead))
    )
  return (result)
}

forecast_Arima <- function(rates, days.ahead = 30) {
  days.ahead <<- days.ahead
  years <- format(rates$Date, "%Y")
  tab <- table(years)
  n <- round(mean(tab[1:(length(tab) - 1)]))
  myTs <- ts(rates$Value, start = 1, frequency = n)
  fit.arima <- arima(myTs, order = c(0, 0, 1))
  prediction <- forecast(fit.arima, h = days.ahead)
  #plot(fore)
  upper <- prediction$upper[, "95%"]
  lower <- prediction$lower[, "95%"]
  trend <- as.numeric(prediction$fitted)
  pred <- as.numeric(prediction$mean)
  result <- data.frame(
    actual = c(rates$Value, rep(NA, days.ahead)),
    trend = c(trend, rep(NA, days.ahead)),
    pred = c(rep(NA,
                 nrow(rates)), pred),
    lower = c(rep(NA, nrow(rates)), lower),
    upper = c(rep(NA, nrow(rates)), upper),
    Date = c(rates$Date,
             max(rates$Date) + (1:days.ahead))
  )
 return(result)
}


plotResults <- function(result) {
  result <- result[order(result$Date),]
  max.val <- max(c(result$actual, result$upper), na.rm = T)
  min.val <- min(c(result$actual, result$lower), na.rm = T)
  plot(
    result$Date,
    result$actual,
    type = "l",
    col = "grey",
    main = paste("Euro exchange rate prediction for", days.ahead, "days"),
    xlab = "Time",
    ylab = "Exchange Rate",
    xlim = range(result$Date),
    ylim = c(min.val, max.val)
  )
  grid()
  lines(result$Date, result$trend, col = "orange")
  lines(result$Date, result$pred, col = "darkgreen")
  lines(result$Date, result$lower, col = "blue")
  lines(result$Date, result$upper, col = "blue")
  legend(
    "bottomleft",
    col = c("darkgrey", "orange", "darkgreen",
            "blue"),
    lty = 1,
    c("Actual", "Trend", "Forecast", "Lower/Upper Bound")
  )
}

