# pos_analysis_with_R
arima model


library(readxl)
possa <- read_excel("C:/Users/C.Maringe/Desktop/possa.xlsx")
possa
pos_timeseries <- ts(possa, frequency=12, start=c(2009))
possa.ts <- as.ts(possa$"Thousand Units") 
plot.ts(pos_timeseries)
logpos_timeseries <- log(pos_timeseries)
plot.ts(logpos_timeseries)

install.packages("TTR")
library("TTR")

possaseries <- ts(possa,start=c(2009))
plot.ts(possaseries)

possaseriesforecasts <- HoltWinters(possaseries, beta=FALSE, gamma=FALSE)
possaseriesforecasts
###Call:"
#HoltWinters(x = possaseries, beta = FALSE, gamma = FALSE)

#Smoothing parameters:
 # alpha: 0.9828177
#beta : FALSE
#gamma: FALSE

#Coefficients:
#  [,1]
#a 423.6169

possaseriesforecasts$fitted

plot(possaseriesforecasts)

possaseriesforecasts$SSE

HoltWinters(possaseries, beta=FALSE, gamma=FALSE, l.start=23.56)

install.packages("forecast")

possa.tsarima <- arima(possa.ts, order=c(0,1,1)) # fit an ARIMA(0,1,1) model
possa.tsarima

possa.tsarimaforecasts <- forecast(possa.tsarima, h=5)
possa.tsarimaforecasts
plot.forecast(possa.tsarima)
