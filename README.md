# binancer

An R client to the Public Rest API for Binance.

API docs: https://github.com/binance/binance-spot-api-docs/blob/master/rest-api.md

Quick intro to using `binancer` by getting the most recent ~USD price of the cryptocurrencies supported on Binance:

```r
library(binancer)
binance_coins_prices()
```

Getting data on a specific symbol pair, e.g. the most recent Bitcoin/USDT changes:

```r
(klines <- binance_klines('BTCUSDT', interval = '1m'))
```

Visualize this data, e.g. on a simple line chart:

```r
library(ggplot2)
ggplot(klines, aes(close_time, close)) + geom_line()
```

Poor man's candle chart with some `ggplot2` tweaks:

```r
library(scales)
ggplot(klines, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Last Updated:', Sys.time())) +
  scale_y_continuous(labels = dollar) +
  scale_color_manual(values = c('#1a9850', '#d73027')) # RdYlGn
```

Extend this to multiple pairs in the past 24 hours using 15 mins intervals:

```r
library(data.table)
klines <- rbindlist(lapply(
  c('ETHBTC', 'ARKBTC', 'NEOBTC', 'IOTABTC'),
  binance_klines,
  interval = '15m',
  limit = 4*24))
ggplot(klines, aes(open_time)) +
  geom_linerange(aes(ymin = open, ymax = close, color = close < open), size = 2) +
  geom_errorbar(aes(ymin = low, ymax = high), size = 0.25) +
  theme_bw() + theme('legend.position' = 'none') + xlab('') +
  ggtitle(paste('Last Updated:', Sys.time())) +
  scale_color_manual(values = c('#1a9850', '#d73027')) +
  facet_wrap(~symbol, scales = 'free', nrow = 2)
```
