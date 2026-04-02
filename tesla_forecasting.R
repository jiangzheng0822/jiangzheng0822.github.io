---
title: "math443project"
output: html_document
date: "2025-12-17"
---

```{r}
tsla <- read.csv("C:/Users/jiang/OneDrive/Desktop/math443/Tesla_stock.csv")
head(tsla)
tail(tsla)
dim(tsla)
```

```{r}
tsla_close <- ts(tsla$Close)
length(tsla_close)
plot(tsla_close)
```

```{r}
summary(tsla_close)
sd(tsla_close)
var(tsla_close)
range(tsla_close)
```





```{r}
tsla_close_f <- ts(tsla$Close, frequency = 252)
plot(tsla_close_f)
```
```{r}
# check trend
t <- as.numeric(time(tsla_close))
fit <- lm(as.numeric(tsla_close) ~ t)
summary(fit)
# small p-value, reject hypothesis, prove upward trend
```

```{r}
acf(tsla_close, main = "ACF of TSLA Close Price")
pacf(tsla_close, main = "PACF of TSLA Close Price")
```

```{r}
library(tseries)
adf.test(tsla_close)
```

```{r}
log_tsla <- log(tsla_close)
diff_log_tsla <- diff(log_tsla)
length(diff_log_tsla)
diff_log_tsla <- na.omit(diff_log_tsla)
adf.test(diff_log_tsla)
acf(diff_log_tsla)
pacf(diff_log_tsla)
```

```{r}
library(forecast)
fit <- auto.arima(log_tsla)
fit
```

```{r}
library(itsmr)
test(fit$residuals)
```



```{r}
fit_010 <- arima(log_tsla, order = c(0,1,0))   # random walk
fit_110 <- arima(log_tsla, order = c(1,1,0))   # AR(1) on differences
fit_011 <- arima(log_tsla, order = c(0,1,1))   # MA(1) on differences
fit_111 <- arima(log_tsla, order = c(1,1,1))   # ARMA(1,1) on differences
AIC(fit_010, fit_110, fit_011, fit_111)
```

```{r}
test(fit_010$residuals)
```

```{r}
test(fit_110$residuals)
```

```{r}
test(fit_011$residuals)
```

```{r}
test(fit_111$residuals)
```

```{r}
# GARCH(1,1)
library(forecast)
library(rugarch)
library(itsmr)

log_tsla <- log(tsla_close)
returns <- diff(log_tsla)
returns <- na.omit(returns)

spec <- ugarchspec(
  variance.model = list(
    model = "sGARCH",
    garchOrder = c(1, 1)
  ),
  mean.model = list(
    armaOrder = c(0, 0),
    include.mean = TRUE
  ),
  distribution.model = "norm"
)

fit_garch <- ugarchfit(
  spec = spec,
  data = returns
)
fit_garch

```





```{r}
# ARCH(2)
spec2 = ugarchspec(mean.model = list(armaOrder=c(0,0)),variance.model = list(garchOrder =c(2,0)))
spec2
ugfit = ugarchfit(spec = spec2, data = returns)
ugfit
```

```{r}
library(forecast)

train <- tsla_close[1:2264]
test  <- tsla_close[2265:2274]

fit <- Arima(log(train), order = c(0,1,0), include.drift = TRUE)
fc  <- forecast::forecast(fit, h = 10, level = 95)

price_forecast <- exp(fc$mean)
lower95 <- exp(fc$lower)
upper95 <- exp(fc$upper)

forecast_table <- data.frame(
  Actual  = as.numeric(test),
  lower95 = as.numeric(lower95),
  upper95 = as.numeric(upper95),
  Covered = as.numeric(test) >= as.numeric(lower95) &
            as.numeric(test) <= as.numeric(upper95)
)

forecast_table
```

```{r}
train_end <- length(train)
fc_index  <- (train_end + 1):(train_end + 10)

# plot last 100 training points + test period
hist_index <- (train_end - 100):train_end
hist_price <- as.numeric(tsla_close[hist_index])

plot(hist_index, hist_price,
     type = "l",
     xlim = c(train_end - 100, train_end + 10),
     ylim = range(c(hist_price, test, lower95, upper95)),
     xlab = "Time",
     ylab = "TSLA Price",
     main = "TSLA Backtest: ARIMA(0,1,0) with 95% Prediction Interval")

# actual test prices
lines(fc_index, as.numeric(test),
      col = "black", lwd = 2)

# forecast mean
lines(fc_index, price_forecast,
      col = "blue", lwd = 2)

# 95% prediction interval
lines(fc_index, lower95, col = "red", lty = 2)
lines(fc_index, upper95, col = "red", lty = 2)

legend("topleft",
       legend = c("Training data", "Actual (test)", "Forecast", "95% CI"),
       col = c("black", "black", "blue", "red"),
       lty = c(1, 1, 1, 2),
       lwd = c(1, 2, 2, 1),
       bty = "n")
```











