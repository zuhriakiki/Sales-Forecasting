#input library
library(data.table)
library(tseries)
library(forecast)
library(ggplot2)
library(plotly)
library(dplyr)

#load dataset
df <- read.csv("...\\data.csv", 1, sep = ";")
summary(df)
boxplot(df$penjualan, ylab='Rupiah', main="Boxplot Sales Lestari Alam")

#transform our data frame to time series data 
data_forecast <- ts(df[2], start=c(2018, 1), end=c(2020, 12), frequency=12)

#make plot from our data
plot.ts(data_forecast)

#Stationary Test (ADF Test)
adf.test(data_forecast)
adf.test(data_forecast, k=12)

#auto.arima function can be differencing our data
df2 <- auto.arima(data_forecast, ic = 'aic', trace = TRUE)

#check autocorrelation function and partial autocorrelation function plot
acf(ts(df2$residuals))
pacf(ts(df2$residuals))

#forecast ARIMA
arima <- forecast(df2, h = 12)
summary(arima)
arima
plot(arima, xlab = "tahun", ylab = "Penjualan (Rp)", main = "Forecast Sales")
df_arima <- data.frame(arima)
df_arima$num <- 1:12
df_arima_sort <- arrange(df_arima, desc(num))

#bismillah plotly
trace1 <- list(
  line = list(
    color = "rgbargba(184, 136, 169, 1.0)", 
    fillcolor = "rgbargba(184, 136, 169, 1.0)"
  ), 
  mode = "lines", 
  name = "observed", 
  type = "scatter", 
  x = c(2018, 2018.08333333333, 2018.16666666667, 2018.25, 2018.33333333333, 2018.41666666667, 2018.5, 2018.58333333333, 2018.66666666667, 2018.75, 2018.83333333333, 2018.91666666667, 
        2019, 2019.08333333333, 2019.16666666667, 2019.25, 2019.33333333333, 2019.41666666667, 2019.5, 2019.58333333333, 2019.66666666667, 2019.75, 2019.83333333333, 2019.91666666667, 
        2020, 2020.08333333333, 2020.16666666667, 2020.25, 2020.33333333333, 2020.41666666667, 2020.5, 2020.58333333333, 2020.66666666667, 2020.75, 2020.83333333333, 2020.91666666667), 
  y = c(df$penjualan), 
  xaxis = "x", 
  yaxis = "y"
)
trace2 <- list(
  fill = "toself", 
  line = list(
    color = "rgba(242,242,242,1)", 
    fillcolor = "rgba(242,242,242,1)"
  ), 
  mode = "lines", 
  name = "95% confidence", 
  type = "scatter", 
  x = c(2021, 2021.08333333333, 2021.16666666667, 2021.25, 2021.33333333333, 2021.41666666667, 2021.5, 2021.58333333333, 2021.66666666667, 2021.75, 2021.83333333333, 2021.91666666667,
        2021.91666666667, 2021.83333333333, 2021.75, 2021.66666666667, 2021.58333333333, 2021.5, 2021.41666666667, 2021.33333333333, 2021.25, 2021.16666666667, 2021.08333333333, 2021), 
  y = c(df_arima$Lo.95, df_arima_sort$Hi.95), 
  xaxis = "x", 
  yaxis = "y", 
  hoveron = "points"
)
trace3 <- list(
  fill = "toself", 
  line = list(
    color = "rgba(204,204,204,1)", 
    fillcolor = "rgba(204,204,204,1)"
  ), 
  mode = "lines", 
  name = "80% confidence", 
  type = "scatter", 
  x = c(2021, 2021.08333333333, 2021.16666666667, 2021.25, 2021.33333333333, 2021.41666666667, 2021.5, 2021.58333333333, 2021.66666666667, 2021.75, 2021.83333333333, 2021.91666666667,
        2021.91666666667, 2021.83333333333, 2021.75, 2021.66666666667, 2021.58333333333, 2021.5, 2021.41666666667, 2021.33333333333, 2021.25, 2021.16666666667, 2021.08333333333, 2021), 
  y = c(df_arima$Lo.80, df_arima_sort$Hi.80), 
  xaxis = "x", 
  yaxis = "y", 
  hoveron = "points"
)
trace4 <- list(
  line = list(
    color = "rgba(0,0,255,1)", 
    fillcolor = "rgba(0,0,255,1)"
  ), 
  mode = "lines", 
  name = "prediction", 
  type = "scatter", 
  x = c(2021, 2021.08333333333, 2021.16666666667, 2021.25, 2021.33333333333, 2021.41666666667, 2021.5, 2021.58333333333, 2021.66666666667, 2021.75, 2021.83333333333, 2021.91666666667), 
  y = c(df_arima$Point.Forecast), 
  xaxis = "x", 
  yaxis = "y"
)
data_plot <- list(trace1, trace2, trace3, trace4)
layout <- list(
  title = "Sales Forecast", 
  xaxis = list(
    title = "Year", 
    domain = c(0, 1)
  ), 
  yaxis = list(
    title = "Rupiah (Rp)", 
    domain = c(0, 1)
  ), 
  margin = list(
    b = 40, 
    l = 60, 
    r = 10, 
    t = 25
  )
)
m <- plot_ly()
m <- add_trace(m, line=trace1$line, mode=trace1$mode, name=trace1$name, type=trace1$type, x=trace1$x, y=trace1$y, xaxis=trace1$xaxis, yaxis=trace1$yaxis)
m <- add_trace(m, fill=trace2$fill, line=trace2$line, mode=trace2$mode, name=trace2$name, type=trace2$type, x=trace2$x, y=trace2$y, xaxis=trace2$xaxis, yaxis=trace2$yaxis, hoveron=trace2$hoveron)
m <- add_trace(m, fill=trace3$fill, line=trace3$line, mode=trace3$mode, name=trace3$name, type=trace3$type, x=trace3$x, y=trace3$y, xaxis=trace3$xaxis, yaxis=trace3$yaxis, hoveron=trace3$hoveron)
m <- add_trace(m, line=trace4$line, mode=trace4$mode, name=trace4$name, type=trace4$type, x=trace4$x, y=trace4$y, xaxis=trace4$xaxis, yaxis=trace4$yaxis)
m <- layout(m, title=layout$title, xaxis=layout$xaxis, yaxis=layout$yaxis, margin=layout$margin)
m
saveWidget(m, "...\\sales_forecast.html", selfcontained = F, libdir = "lib")