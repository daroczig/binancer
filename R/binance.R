INTERVALS <- c(
    '1m', '3m', '5m', '15m', '30m',
    '1h', '2h', '4h', '6h', '8h', '12h',
    '1d', '3d', '1w', '1M')


## #############################################################################
## Utils

#' Look up Binance API secret stored in the environment
#' @return string
#' @keywords internal
binance_secret <- function() {
    binance_check_credentials()
    credentials$secret
}


#' Look up Binance API key stored in the environment
#' @return string
#' @keywords internal
binance_key <- function() {
    binance_check_credentials()
    credentials$key
}


#' Sets the API key and secret to interact with the Binance API
#' @param key string
#' @param secret string
#' @export
#' @examples \dontrun{
#' binance_credentials('foo', 'bar')
#' }
binance_credentials <- function(key, secret) {
    credentials$key <- key
    credentials$secret <- secret
}


#' Check if Binance credentials were set previously
#' @return fail on missing credentials
#' @keywords internal
binance_check_credentials <- function() {
    if (is.null(credentials$secret)) {
        stop('Binance API secret not set? Call binance_credentials()')
    }
    if (is.null(credentials$key)) {
        stop('Binance API key not set? Call binance_credentials()')
    }
}


#' Sign the query string for Binance
#' @param params list
#' @return string
#' @keywords internal
#' @importFrom digest hmac
#' @examples \dontrun{
#' signature(list(foo = 'bar', z = 4))
#' }
binance_sign <- function(params) {
    params$timestamp <- timestamp()
    params$signature <- hmac(
        key = binance_secret(),
        object = paste(
            mapply(paste, names(params), params, sep = '=', USE.NAMES = FALSE),
            collapse = '&'),
        algo = 'sha256')
    params
}


#' Request the Binance API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param sign if signature required
#' @param retry allow retrying the query on failure
#' @return R object
#' @keywords internal
binance_query <- function(endpoint, method = 'GET',
                          params = list(), sign = FALSE,
                          retry = method == 'GET') {

    method <- match.arg(method)

    if (isTRUE(sign)) {
        params <- binance_sign(params)
        config <- add_headers('X-MBX-APIKEY' = binance_key())
    } else {
        config <- config()
    }

    query(
        base = 'https://api.binance.com',
        path = endpoint,
        method = method,
        params = params,
        config = config)

}

## #############################################################################
## Stats

#' Get kline/candlestick data from Binance
#' @param symbol string
#' @param interval enum
#' @param limit int
#' @return data.table
#' @export
#' @importFrom data.table rbindlist
#' @examples \dontrun{
#' get_klines('ETHUSDT')
#' }
get_klines <- function(symbol, interval, limit = 500) {

    interval <- match.arg(interval)

    klines <- binance_query(
        endpoint = 'api/v1/klines',
        params   = list(symbol   = symbol,
                        interval = interval,
                        limit    = limit))

    klines <- rbindlist(klines)
    ## drop last dummy column
    klines <- klines[, -12]
    names(klines) <- c(
        'open_time',
        'open',
        'high',
        'low',
        'close',
        'volume',
        'close_time',
        'quote_asset_volume',
        'trades',
        'taker_buy_base_asset_volume',
        'taker_buy_quote_asset_volume')

    for (v in names(klines)) {
        klines[, (v) := as.numeric(get(v))]
    }

    for (v in c('open_time', 'close_time')) {
        klines[, (v) := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
    }

    ## return
    klines[, symbol := symbol]
    klines

}


## #############################################################################
## Ticker data

#' Get latest Binance conversion rates and USD prices on all symbol pairs
#' @return data.table
#' @export
#' @importFrom data.table rbindlist
binance_ticker_all_prices <- function() {

    prices <- binance_query(endpoint = 'api/v1/ticker/allPrices')
    prices <- rbindlist(prices)
    prices[, price := as.numeric(price)]

    ## not sure what's this
    prices <- prices[symbol != '123456']

    ## split from/to
    prices[, from := sub('(ETH|BTC|USDT|BNB)$', '', symbol)]
    prices[, to := sub('.*(ETH|BTC|USDT|BNB)$', '\\1', symbol)]

    ## add computed price in USD
    prices[grepl('ETH$', symbol), to_usd := prices[symbol == 'ETHUSDT', price]]
    prices[grepl('BTC$', symbol), to_usd := prices[symbol == 'BTCUSDT', price]]
    prices[grepl('USDT$', symbol), to_usd := price]
    prices[grepl('BNB$', symbol), to_usd := prices[symbol == 'BNBUSDT', price]]

    prices[grepl('USDT$', symbol), from_usd := price]
    prices[!grepl('USDT$', symbol), from_usd := price * to_usd]

    prices[, .(symbol, price, from, from_usd, to, to_usd)]

}


#' Get all currently valid symbol names from Binance
#' @return character vector
#' @export
binance_symbols <- function() {
    unlist(sapply(
        binance_query(endpoint = '/api/v1/exchangeInfo')$symbols,
        `[`, 'symbol'), use.names = FALSE)
}


#' Get all currently valid coin names from Binance
#' @return character vector
#' @export
binance_coins <- function() {
    sort(unique(sub('(ETH|BTC|USDT|BNB)$', '', binance_symbols())))
}


#' Get all currently valid coin names from Binance along with the USDT prices
#' @return data.table
#' @export
binance_coins_prices <- function(unit = 'USDT') {
    unique(binance_ticker_all_prices(), by = 'from')[, .(symbol = from, usd = from_usd)]
}


## #############################################################################
## Account info

#' Get current Binance account information, including balances
#' @return data.table
#' @export
#' @importFrom data.table as.data.table
binance_account <- function() {
    as.data.table(binance_query(endpoint = 'api/v3/account', sign = TRUE))
}

#' Get current Binance balances in a nice table
#' @return data.table
#' @export
#' @importFrom data.table rbindlist
#' @param threshold show assets with greater number of coins
#' @param usd to include balance in USDT too
binance_balances <- function(threshold = -1, usdt = FALSE) {

    balances <- rbindlist(binance_account()$balances)
    balances[, free := as.numeric(free)]
    balances[, locked := as.numeric(locked)]
    balances[, total := free + locked]

    if (isTRUE(usdt)) {

        balances <- merge(
            balances, binance_coins_prices(),
            by.x = 'asset', by.y = 'symbol', all.x = TRUE, all.y = FALSE)
        balances[, usd := usd * total]

    }

    as.data.table(balances)[total > threshold]

}


#' Get all trades on the Binance account
#' @param symbol string
#' @param limit number of trades to fetch
#' @param from_id optional trade id to fetch from
#' @return data.table
#' @export
#' @importFrom data.table as.data.table setnames
#' @examples \dontrun{
#' binance_mytrades('ARKETH')
#' binance_mytrades(c('ARKBTC', 'ARKETH'))
#' }
#' @importFrom snakecase to_snake_case
binance_mytrades <- function(symbol, limit = 500, from_id) {

    if (length(symbol) > 1) {
        return(rbindlist(lapply(symbol, binance_mytrades), fill = TRUE))
    }

    params <- list(symbol = symbol)
    if (!missing(from_id)) {
        params$fromId = from_id
    }

    trades <- binance_query(endpoint = 'api/v3/myTrades', params = params, sign = TRUE)
    trades <- rbindlist(trades)
    if (nrow(trades) == 0) {
        return(data.table())
    }

    ## return with snake_case column names
    setnames(trades, to_snake_case(names(trades)))

}
