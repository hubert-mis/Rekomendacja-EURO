library(quantmod)
getSymbols("PLN=X", src = )
getSymbols("EUR=X")
"PLN/EUR"<-`PLN=X`[,-5]/`EUR=X`[,-5]
chartSeries(`PLN/EUR`)
#chart
