BINANCE <- list(
    TYPE = c('LIMIT', 'MARKET', 
             'STOP_LOSS', 'STOP_LOSS_LIMIT', 
             'TAKE_PROFIT', 'TAKE_PROFIT_LIMIT', 
             'LIMIT_MAKER'), 
    SIDE = c('BUY', "SELL"), 
    TIMEINFORCE = c('GTC', 'IOC', 'FOK'), 
    INTERVALS = c(
        '1m', '3m', '5m', '15m', '30m', 
        '1h', '2h', '4h', '6h', '8h', '12h', 
        '1d', '3d', '1w', '1M'), 
    METHOD = c('GET', 'POST', 'PUT', 'DELETE'), 
    DEPTH_LIMITS = c(5, 10, 20, 50, 100, 500, 1000)
    )


# Utils -------------------------------------------------------------------

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
#' @inheritParams query
#' @param sign if signature required
#' @return R object
#' @keywords internal
binance_query <- function(endpoint, method,
                          params = list(), body = FALSE, sign = FALSE,
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


# Stats -------------------------------------------------------------------

#' Get kline/candlestick data from Binance
#' @param symbol string
#' @param interval enum
#' @param limit optional int
#' @param start_time optional POSIX timestamp
#' @param end_time optional POSIX timestamp
#' @return data.table
#' @export
#' @importFrom data.table rbindlist data.table
#' @examples \dontrun{
#' binance_klines('ETHUSDT')
#' binance_klines('ETHUSDT', interval = '1h', limit = 24*7)
#' binance_klines('ETHUSDT', interval = '1h',
#'     start_time = as.POSIXct('2018-01-01'), end_time = as.POSIXct('2018-01-08'))
#' }
binance_klines <- function(symbol, interval, limit, start_time, end_time) {

    interval <- match.arg(interval)

    params <- list(symbol   = symbol,
                   interval = interval)
    
    if (!missing(limit)) {
        params$limit <- limit
    }
    if (!missing(start_time)) {
        params$startTime <- format(as.numeric(start_time) * 1e3, scientific = FALSE)
    }
    if (!missing(end_time)) {
        params$endTime <- format(as.numeric(end_time) * 1e3, scientific = FALSE)
    }

    klines <- binance_query(endpoint = 'api/v1/klines', params = params)

    klines <- rbindlist(klines)
    # drop last dummy column
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

    # return
    klines[, symbol := symbol]
    data.table(klines)

}

#' Get tick data from Binance
#' @param symbol string
#' @param from_id optional number
#' @param start_time optional POSIX timestamp
#' @param end_time optional POSIX timestamp
#' @param limit optional int
#' @return data.table
#' @export
#' @importFrom data.table rbindlist data.table
#' @examples \dontrun{
#' binance_ticks('ETHUSDT')
#' binance_ticks('ETHUSDT', start_time = as.POSIXct('2018-01-01'), end_time = as.POSIXct('2018-01-08'))
#' }
binance_ticks <- function(symbol, from_id, start_time, end_time, limit) {
    
    params <- list(symbol = symbol)
    
    if (!missing(limit)) {
        params$limit <- limit
    }
    if (!missing(from_id)) {
        params$fromId <- from_id
    }
    if (!missing(start_time)) {
        params$startTime <- format(as.numeric(start_time) * 1e3, scientific = FALSE)
    }
    if (!missing(end_time)) {
        if (as.numeric(difftime(end_time, start_time, units = "secs")) > 3600) {
            end_time <- start_time + 3600
        }
        params$endTime <- format(as.numeric(end_time) * 1e3, scientific = FALSE)
    }
    
    ticks <- binance_query(endpoint = 'api/v1/aggTrades', params = params)
    
    if (length(ticks) > 0) {
        ticks <- rbindlist(ticks)
        names(ticks) <- c(
            'agg_tradeID',
            'price',
            'quantity',
            'first_tradeID',
            'last_tradeID',
            'T',
            'buyer_maker',
            'best_price_match')
        
        for (v in c('price', 'quantity')) {
            ticks[, (v) := as.numeric(get(v))]
        }
        
        ticks[, T := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
        
        # return
        ticks[, symbol := symbol]
        data.table(ticks)
    }
}

#' Get orderbook depth data from Binance
#' @param symbol string
#' @param limit int optional
#' @return data.table
#' @export
#' @importFrom data.table rbindlist data.table
#' @examples \dontrun{
#' binance_depth('ETHUSDT')
#' binance_depth('ETHUSDT', limit = 1000)
#' }
binance_depth <- function(symbol, limit) {
    
    params <- list(symbol = symbol)

    if (!missing(limit)) {
        limit <- match.arg(limit)
        params$limit <- limit
    }
    
    depth <- binance_query(endpoint = 'api/v1/depth', params = params)
    
    bids <- rbindlist(depth$bids)
    asks <- rbindlist(depth$asks)
    
    names(bids) <- c(
        'price',
        'quantity')
    
    names(asks) <- c(
        'price',
        'quantity')
    
    for (v in names(bids)) {
        bids[, (v) := as.numeric(get(v))]
    }
    
    for (v in names(asks)) {
        asks[, (v) := as.numeric(get(v))]
    }
    
    # return
    depth$bids <- bids
    depth$asks <- asks
    
    depth
    
}


# Ticker data -------------------------------------------------------------

#' Get latest Binance conversion rates and USD prices on all symbol pairs
#' @return data.table
#' @export
#' @importFrom data.table rbindlist
binance_ticker_all_prices <- function() {

    prices <- binance_query(endpoint = 'api/v1/ticker/allPrices')
    prices <- rbindlist(prices)
    prices[, price := as.numeric(price)]

    # not sure what's this
    prices <- prices[symbol != '123456']

    # split from/to
    prices[, from := sub('(BTC|ETH|BNB|USDT|TUSD|PAX|USDC|XRP|USDS)$', '', symbol)]
    prices[, to := sub('.*(BTC|ETH|BNB|USDT|TUSD|PAX|USDC|XRP|USDS)$', '\\1', symbol)]

    # add computed price in USD
    prices[to == 'BTC', to_usd := prices[symbol == 'BTCUSDT', price]]
    prices[to == 'ETH', to_usd := prices[symbol == 'ETHUSDT', price]]
    prices[to == 'BNB', to_usd := prices[symbol == 'BNBUSDT', price]]
    prices[to == 'XRP', to_usd := prices[symbol == 'XRPUSDT', price]]
    prices[to == 'USDT' | to == 'TUSD' | to == 'PAX' | to == 'USDC' | to == 'USDS', to_usd := 1]

    prices[to == 'USDT' | to == 'TUSD' | to == 'PAX' | to == 'USDC' | to == 'USDS', from_usd := price]
    prices[to != 'USDT' & to != 'TUSD' & to != 'PAX' & to != 'USDC' & to != 'USDS', from_usd := price * to_usd]
    
    prices[, .(symbol, price, from, from_usd, to, to_usd)]
}

#' Get exchangeInfo from Binance
#' @return list
#' @export
binance_exchangeInfo <- function() {
    binance_query(endpoint = '/api/v1/exchangeInfo')
}


#' Get all currently valid symbol names from Binance
#' @return character vector
#' @export
binance_symbols <- function() {
    unlist(sapply(
        binance_exchangeInfo()$symbols,
        `[`, 'symbol'), use.names = FALSE)
}


#' Get all currently valid coin names from Binance
#' @return character vector
#' @export
binance_coins <- function() {
    sort(unique(sub('(BTC|ETH|BNB|USDT|TUSD|PAX|USDC|XRP|USDS)$', '', binance_symbols())))
}


#' Get all currently valid coin names from Binance along with the USDT prices
#' @return data.table
#' @export
#' @param unit to set quote asset
binance_coins_prices <- function(unit = 'USDT') {
    unique(binance_ticker_all_prices(), by = 'from')[, .(symbol = from, usd = from_usd)]
}


# Account info ------------------------------------------------------------

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
#' @param usdt to include balance in USDT too
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
#' @param limit optional int number of trades to fetch
#' @param from_id optional trade id to fetch from
#' @param start_time optional POSIX timestamp
#' @param end_time optional POSIX timestamp
#' @return data.table
#' @export
#' @importFrom data.table as.data.table setnames
#' @examples \dontrun{
#' binance_mytrades('ARKETH')
#' binance_mytrades(c('ARKBTC', 'ARKETH'))
#' }
#' @importFrom snakecase to_snake_case
binance_mytrades <- function(symbol, limit, from_id, start_time, end_time) {

    if (length(symbol) > 1) {
        return(rbindlist(lapply(symbol, binance_mytrades), fill = TRUE))
    }

    params <- list(symbol = symbol)
    
    if (!missing(limit)) {
        params$limit = limit
    }
    if (!missing(from_id)) {
        params$fromId = from_id
    }
    if (!missing(start_time)) {
        params$startTime <- format(as.numeric(start_time) * 1e3, scientific = FALSE)
    }
    if (!missing(end_time)) {
        params$endTime <- format(as.numeric(end_time) * 1e3, scientific = FALSE)
    }
    
    trades <- binance_query(endpoint = 'api/v3/myTrades', params = params, sign = TRUE)
    trades <- rbindlist(trades)
    if (nrow(trades) == 0) {
        return(data.table())
    }

    # return with snake_case column names
    setnames(trades, to_snake_case(names(trades)))

}


#' Open new order on the Binance account
#' @param symbol string
#' @param side enum
#' @param type enum
#' @param timeInForce optional enum
#' @param quantity number
#' @param price optional number
#' @param stopPrice optional number
#' @param icebergQty optional number
#' @param test bool
#' @return data.table
#' @export
#' @examples \dontrun{
#' binance_new_order('ARKETH', side = 'BUY', type = 'MARKET', quantity = 1)
#' binance_new_order('ARKBTC', side = 'BUY', type = 'LIMIT', quantity = 1, price = 0.5, timeInForce = 'GTC')
#' }
binance_new_order <- function(symbol, side, type, timeInForce, quantity, price, stopPrice, icebergQty, test = TRUE) {
    
    side <- match.arg(side)
    type <- match.arg(type)
    
    if (type == 'LIMIT') {
        stopifnot(!missing(timeInForce), !missing(price))
    }
    if (type == 'STOP_LOSS' | type == 'TAKE_PROFIT') {
        stopifnot(!missing(stopPrice))
    }
    if (type == 'STOP_LOSS_LIMIT' | type == 'TAKE_PROFIT_LIMIT') {
        stopifnot(!missing(timeInForce), !missing(price), !missing(stopPrice))
    }
    if (type == 'LIMIT_MAKER') {
        stopifnot(!missing(price))
    }
    
    params <- list(symbol   = symbol,
                   side     = side,
                   type     = type,
                   quantity = quantity)
    
    if (!missing(timeInForce)) {
        timeInForce <- match.arg(timeInForce)
        params$timeInForce = timeInForce
    }
    if (!missing(icebergQty)) {
        params$icebergQty = icebergQty
        if (icebergQty > 0) {
            stopifnot(timeInForce == 'GTC')
        }
    }
    if (!missing(price)) {
        params$price = price
    }

    if (isTRUE(test)) {
        binance_query(endpoint = 'api/v3/order/test', method = 'POST', params = params, sign = TRUE)
    } else {
        binance_query(endpoint = 'api/v3/order', method = 'POST', params = params, sign = TRUE)
    }
}


#' Query order on the Binance account
#' @param symbol string
#' @param order_id optional number
#' @param client_order_id optional string
#' @return data.table
#' @export
#' @examples \dontrun{
#' binance_query_order('ARKETH', 8)
#' binance_query_order('ARKBTC', "myOrder7")
#' }
binance_query_order <- function(symbol, order_id, client_order_id) {
    
    stopifnot(!missing(order_id) | !missing(client_order_id))
    
    params <- list(symbol = symbol)
    
    if (!missing(order_id)) {
        params$orderId = order_id
    }
    if (!missing(client_order_id)) {
        params$origClientOrderId = client_order_id
    }
    
    binance_query(endpoint = 'api/v3/order', method = 'GET', params = params, sign = TRUE)
}


#' Cancel order on the Binance account
#' @param symbol string
#' @param order_id optional number
#' @param client_order_id optional string
#' @return data.table
#' @export
#' @examples \dontrun{
#' binance_cancel_order('ARKETH', 8)
#' binance_cancel_order('ARKBTC', "myOrder7")
#' }
binance_cancel_order <- function(symbol, order_id, client_order_id) {
    
    stopifnot(!missing(order_id) | !missing(client_order_id))
    
    params <- list(symbol = symbol)
    
    if (!missing(order_id)) {
        params$orderId = order_id
    }
    if (!missing(client_order_id)) {
        params$origClientOrderId = client_order_id
    }
    
    binance_query(endpoint = 'api/v3/order', method = 'DELETE', params = params, sign = TRUE)
}


#' Fetch open orders from the Binance account
#' @param symbol optional string
#' @return data.table
#' @export
#' @examples \dontrun{
#' binance_cancel_order('ARKETH', 8)
#' binance_cancel_order('ARKBTC', "myOrder7")
#' }
binance_open_orders <- function(symbol) {
    
    params <- list(symbol = symbol)
    
    binance_query(endpoint = 'api/v3/openOrders', method = 'GET', params = params, sign = TRUE)
}
