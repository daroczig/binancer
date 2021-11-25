# binancer

An R client to some Cryptocurrency exchanges:

* Binance API docs: https://github.com/binance/binance-spot-api-docs/blob/master/rest-api.md

Example on getting data from Binance on the most recent Bitcoin/USDT changes:

```r
library(binancer)
binance_klines('BTCUSDT', interval = '1m')
```

Extend this to multiple pairs:

```r
rbindlist(lapply(c("ETHBTC", "LTCBTC"), binance_klines, interval = "1d", limit = 200))
```
