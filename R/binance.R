BINANCE <- list(
    BASE = c(
        'https://api.binance.com',    # SPOT
        'https://fapi.binance.com'    # USDM
    ),
    SPOT = list(
        TIMEINFORCE = c('GTC', 'IOC', 'FOK'),
        TYPE = c('LIMIT', 'MARKET',
                 'STOP_LOSS', 'STOP_LOSS_LIMIT',
                 'TAKE_PROFIT', 'TAKE_PROFIT_LIMIT',
                 'LIMIT_MAKER')
    ),
    USDM = list(
        TIMEINFORCE = c('GTC', 'IOC', 'FOK', 'GTX'),
        TYPE = c('LIMIT', 'MARKET',
                 'STOP', 'STOP_MARKET',
                 'TAKE_PROFIT', 'TAKE_PROFIT_MARKET',
                 'TRAILING_STOP_MARKET')
    ),
    SIDE = c('BUY', 'SELL'),
    INTERVALS = c(
        '1m', '3m', '5m', '15m', '30m',
        '1h', '2h', '4h', '6h', '8h', '12h',
        '1d', '3d', '1w', '1M'),
    METHODS = c('GET', 'POST', 'PUT', 'DELETE')
)

BINANCE_WEIGHT <- 0

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
#' @return No return values, setting config in the package namespace.
#' @examples \dontrun{
#' binance_credentials('foo', 'bar')
#' }
binance_credentials <- function(key, secret) {
    credentials$key <- key
    credentials$secret <- secret
}


#' Check if Binance credentials were set previously
#' @return No return values, but fails when credentials were not set.
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
#' @param time string
#' @return list
#' @keywords internal
#' @importFrom digest hmac
#' @examples \dontrun{
#' binance_sign(list(foo = 'bar', z = 4))
#' }
binance_sign <- function(params, time = timestamp()) {
    params$timestamp <- time
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
#' @param content_as parameter to httr::content
#' @return R object
#' @keywords internal
#' @importFrom httr headers add_headers content
#' @importFrom utils assignInMyNamespace
binance_query <- function(endpoint, base = 'https://api.binance.com', method = 'GET',
                          params = list(), body = NULL, sign = FALSE,
                          retry = method == 'GET', content_as = 'parsed') {

    # if Binance weight is approaching the limit of 1200, wait for the next full minute
    if (BINANCE_WEIGHT > 1159) {
        Sys.sleep(61 - as.integer(format(Sys.time(), "%S")))
    }

    method <- match.arg(method)

    if (isTRUE(sign)) {
        params <- binance_sign(params)
        config <- add_headers('X-MBX-APIKEY' = binance_key())
    } else {
        config <- config()
    }

    res <- query(
        base = base,
        path = endpoint,
        method = method,
        params = params,
        config = config)

    assignInMyNamespace('BINANCE_WEIGHT', as.integer(headers(res)$`x-mbx-used-weight`))
    res <- content(res, as = content_as)

    if (content_as == 'parsed' & length(res) == 2 & !is.null(names(res))) {
        if (all(names(res) == c('code', 'msg'))) {
            stop(paste(res, collapse = ' '))
        }
    }

    res
}
formals(binance_query)$method <- BINANCE$METHODS


#' Test connectivity to the Rest API
#' @export
#' @return "OK" string on success
binance_ping <- function() {
    res <- binance_query(endpoint = '/api/v1/ping')
    if (is.list(res) & length(res) == 0) {
        res <- 'OK'
    }
    res
}


#' Get the current server time from Binance
#' @export
#' @return \code{POSIXct}
binance_time <- function() {
    res <- binance_query(endpoint = '/api/v1/time')$serverTime
    res <- as.POSIXct(res/1e3, origin = '1970-01-01')
    res
}


# Stats -------------------------------------------------------------------

#' Get kline/candlestick data from Binance
#' @param symbol string
#' @param interval enum
#' @param limit optional int
#' @param start_time optional POSIX timestamp
#' @param end_time optional POSIX timestamp
#' @return \code{data.table} with open-high-low-close values
#' @export
#' @importFrom data.table rbindlist data.table :=
#' @examples \dontrun{
#' binance_klines('ETHUSDT')
#' binance_klines('ETHUSDT', interval = '1h', limit = 24*7)
#' binance_klines('ETHUSDT', interval = '1h', start_time = '2018-01-01', end_time = '2018-01-08')
#' }
binance_klines <- function(symbol, interval, limit, start_time, end_time) {

    interval <- match.arg(interval)

    params <- list(symbol   = symbol,
                   interval = interval)

    if (!missing(limit)) {
        stopifnot(limit <= 1000L)
        params$limit <- limit
    }
    if (!missing(start_time)) {
        params$startTime <- format(as.numeric(as.POSIXct(start_time)) * 1e3, scientific = FALSE)
    }
    if (!missing(end_time)) {
        params$endTime <- format(as.numeric(as.POSIXct(end_time)) * 1e3, scientific = FALSE)
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
        'taker_buy_quote_asset_volume'
    )

    for (v in setdiff(names(klines), c('open_time', 'close_time', 'trades'))) {
        klines[, (v) := as.numeric(get(v))]
    }

    for (v in c('open_time', 'close_time')) {
        klines[, (v) := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
    }

    # return
    klines[, symbol := symbol]
    data.table(klines)

}
formals(binance_klines)$interval <- BINANCE$INTERVALS


#' Get tick data from Binance
#' @param symbol string
#' @param from_id optional number
#' @param start_time optional POSIX timestamp
#' @param end_time optional POSIX timestamp
#' @param limit optional int
#' @return \code{data.table}
#' @export
#' @importFrom data.table rbindlist data.table
#' @examples \dontrun{
#' binance_ticks('ETHUSDT')
#' binance_ticks('ETHUSDT', start_time = '2018-01-01 00:00:00', end_time = '2018-01-01 01:00:00')
#' }
binance_ticks <- function(symbol, from_id, start_time, end_time, limit) {

    # silence "no visible global function/variable definition" R CMD check
    time <- NULL

    params <- list(symbol = symbol)

    if (!missing(limit)) {
        stopifnot(limit <= 1000L)
        params$limit <- limit
    }
    if (!missing(from_id)) {
        # work around RCurl issue
        # see https://github.com/daroczig/binancer/pull/9#issuecomment-517916493
        params$fromId <- as.character(from_id)
    }
    if (!missing(start_time)) {
        params$startTime <- format(as.numeric(as.POSIXct(start_time)) * 1e3, scientific = FALSE)
    }
    if (!missing(end_time)) {
        if (!missing(start_time)) {
            stopifnot(as.numeric(difftime(end_time, start_time, units = 'secs')) <= 3600)
        }
        params$endTime <- format(as.numeric(as.POSIXct(end_time)) * 1e3, scientific = FALSE)
    }

    ticks <- binance_query(endpoint = 'api/v1/aggTrades', params = params)

    if (length(ticks) > 0) {
        ticks <- rbindlist(ticks)
        names(ticks) <- c(
            'agg_trade_id',
            'price',
            'quantity',
            'first_trade_id',
            'last_trade_id',
            'time',
            'buyer_maker',
            'best_price_match')

        for (v in c('price', 'quantity')) {
            ticks[, (v) := as.numeric(get(v))]
        }

        ticks[, time := as.POSIXct(time/1e3, origin = '1970-01-01')]

        # return
        ticks[, symbol := symbol]
        ticks
    }
}

#' Get last trades from Binance
#' @param symbol string
#' @param limit optional int
#' @return \code{data.table}
#' @export
#' @importFrom data.table rbindlist data.table
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_trades('ETHUSDT')
#' binance_trades('ETHUSDT', limit = 1000)
#' }
binance_trades <- function(symbol, limit) {

    # silence "no visible global function/variable definition" R CMD check
    time <- NULL

    params <- list(symbol = symbol)

    if (!missing(limit)) {
        stopifnot(limit <= 1000L)
        params$limit <- limit
    }

    trades <- binance_query(endpoint = 'api/v1/trades', params = params)

    trades <- rbindlist(trades)

    for (v in c('price', 'qty', 'quoteQty')) {
        trades[, (v) := as.numeric(get(v))]
    }

    trades[, time := as.POSIXct(time/1e3, origin = '1970-01-01')]

    trades[, symbol := symbol]
    # return with snake_case column names
    setnames(trades, to_snake_case(names(trades)))
    trades
}


#' Get orderbook depth data from Binance
#' @param symbol string
#' @param limit int optional
#' @return \code{data.table}
#' @export
#' @importFrom data.table rbindlist data.table
#' @examples \dontrun{
#' binance_depth('ETHUSDT')
#' binance_depth('ETHUSDT', limit = 1000)
#' }
binance_depth <- function(symbol, limit) {

    params <- list(symbol = symbol)

    if (!missing(limit)) {
        stopifnot(limit %in% c(5, 10, 20, 50, 100, 500, 1000, 5000))
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

#' Get last price for a symbol or all symbols
#' @param symbol optional string
#' @return \code{data.table}
#' @export
#' @examples \dontrun{
#' binance_ticker_price('ETHUSDT')
#' }
binance_ticker_price <- function(symbol) {

    # silence "no visible global function/variable definition" R CMD check
    price <- NULL

    if (!missing(symbol)) {
        params <- list(symbol = symbol)
        res <- binance_query(endpoint = 'api/v3/ticker/price', params = params)
        res <- as.data.table(res)
    } else {
        res <- binance_query(endpoint = 'api/v3/ticker/price')
        res <- rbindlist(res)
    }

    res[, price := as.numeric(price)]
    res
}


#' Get last bids and asks for a symbol or all symbols
#' @param symbol optional string
#' @return \code{data.table}
#' @export
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_ticker_book('ETHUSDT')
#' }
binance_ticker_book <- function(symbol) {

    if (!missing(symbol)) {
        params <- list(symbol = symbol)
        res <- binance_query(endpoint = 'api/v3/ticker/bookTicker', params = params)
        res <- as.data.table(res)
    } else {
        res <- binance_query(endpoint = 'api/v3/ticker/bookTicker')
        res <- rbindlist(res)
    }

    for (v in setdiff(names(res), 'symbol')) {
        res[, (v) := as.numeric(get(v))]
    }

    # return with snake_case column names
    setnames(res, to_snake_case(names(res)))
    res
}


#' Get latest Binance conversion rates and USD prices on all symbol pairs
#' @return \code{data.table}
#' @export
#' @importFrom data.table rbindlist
binance_ticker_all_prices <- function() {

    # silence "no visible global function/variable definition" R CMD check
    price <- from <- to <- to_usd <- from_usd <- symbol <- NULL

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

    prices[, list(symbol, price, from, from_usd, to, to_usd)]
}


#' Get latest Binance bids and asks on all symbol pairs
#' @return \code{data.table}
#' @export
#' @importFrom data.table rbindlist
#' @importFrom snakecase to_snake_case
binance_ticker_all_books <- function() {

    books <- binance_query(endpoint = 'api/v1/ticker/allBookTickers')
    books <- rbindlist(books)

    for (v in setdiff(names(books), 'symbol')) {
        books[, (v) := as.numeric(get(v))]
    }

    # return with snake_case column names
    setnames(books, to_snake_case(names(books)))
    books
}


#' 24 hour rolling window price change statistics
#' @param symbol optional string
#' @return \code{data.table}
#' @export
#' @importFrom data.table rbindlist
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_ticker_24hr('ARKETH')
#' binance_ticker_24hr() # all symbols - binance.weight 40
#' }
binance_ticker_24hr <- function(symbol) {

    if (!missing(symbol)) {
        params <- list(symbol = symbol)
        prices <- binance_query(endpoint = 'api/v1/ticker/24hr', params = params)
        prices <- as.data.table(prices)
    } else {
        prices <- binance_query(endpoint = 'api/v1/ticker/24hr')
        prices <- rbindlist(prices)
    }

    for (v in setdiff(names(prices), c('symbol', 'openTime', 'closeTime', 'firstId', 'lastId', 'count'))) {
        prices[, (v) := as.numeric(get(v))]
    }

    for (v in c('openTime', 'closeTime')) {
        prices[, (v) := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
    }

    # return with snake_case column names
    setnames(prices, to_snake_case(names(prices)))
    prices
}


#' Get current average price for a symbol
#' @param symbol string
#' @return \code{data.table}
#' @export
#' @importFrom jsonlite fromJSON
binance_avg_price <- function(symbol) {

    # silence "no visible global function/variable definition" R CMD check
    price <- NULL

    params <- list(symbol = symbol)

    res <- binance_query(endpoint = '/api/v3/avgPrice', params = params)
    res <- as.data.table(res)
    res[, price := as.numeric(price)]
    res
}

#' Get exchangeInfo from Binance
#' @return \code{list}
#' @export
#' @importFrom jsonlite fromJSON
binance_exchange_info <- function() {
    res <- binance_query(endpoint = '/api/v1/exchangeInfo', content_as = 'text')
    res <- fromJSON(res)
    res$serverTime <- as.POSIXct(res$serverTime/1e3, origin = '1970-01-01')
    res$rateLimits <- as.data.table(res$rateLimits)
    res$symbols <- as.data.table(res$symbols)
    res
}

#' Get current filters for a symbol
#' @param symbol string
#' @return \code{data.table}
#' @export
binance_filters <- function(symbol) {
    # workaround the problem in data.table when variable has the same name as column
    symb <- symbol
    filters <- as.data.table(binance_exchange_info()$symbols[symbol == symb, filters][[1]])

    for (v in setdiff(names(filters), c('filterType', 'avgPriceMins', 'applyToMarket', 'limit', 'maxNumAlgoOrders'))) {
        filters[, (v) := as.numeric(get(v))]
    }

    filters
}


#' Get all currently valid symbol names from Binance
#' @param all optional bool include non-trading symbols
#' @return character vector of symbol names
#' @export
binance_symbols <- function(all = FALSE) {
    # silence "no visible global function/variable definition" R CMD check
    symbol <- status <- NULL

    if (isTRUE(all)) {
        binance_exchange_info()$symbols$symbol
    } else {
        binance_exchange_info()$symbols[status == 'TRADING', symbol]
    }
}


#' Get all currently valid coin names from Binance
#' @return character vector of coin names
#' @export
binance_coins <- function() {
    sort(unique(sub('(BTC|ETH|BNB|USDT|TUSD|PAX|USDC|XRP|USDS)$', '', binance_symbols())))
}


#' Get all currently valid coin names from Binance along with the USDT prices
#' @param unit to set quote asset
#' @return \code{data.table} with \code{symbol} and \code{usd} columns
#' @export
binance_coins_prices <- function(unit = 'USDT') {
    # silence "no visible global function/variable definition" R CMD check
    from <- from_usd <- NULL

    unique(binance_ticker_all_prices(), by = 'from')[, list(symbol = from, usd = from_usd)]
}


# Account info ------------------------------------------------------------

#' Get current general Binance account information, without balances
#' @return data.table
#' @export
#' @importFrom data.table as.data.table
#' @importFrom snakecase to_snake_case
binance_account <- function() {
    account <- binance_query(endpoint = 'api/v3/account', sign = TRUE)
    account$balances <- NULL
    account <- as.data.table(account)

    account$updateTime <- as.POSIXct(account$updateTime/1e3, origin = '1970-01-01')

    # return with snake_case column names
    setnames(account, to_snake_case(names(account)))
    account
}

#' Get current Binance balances in a nice table
#' @param threshold optional show assets with greater number of coins
#' @param usdt optional to include balance in USDT too
#' @return data.table
#' @export
#' @importFrom data.table rbindlist
binance_balances <- function(threshold = -1, usdt = FALSE) {

    # silence "no visible global function/variable definition" R CMD check
    free <- locked <- total <- usd <- NULL

    balances <- binance_query(endpoint = 'api/v3/account', sign = TRUE)$balances
    balances <- rbindlist(balances)
    balances[, free := as.numeric(free)]
    balances[, locked := as.numeric(locked)]
    balances[, total := free + locked]

    if (isTRUE(usdt)) {
        balances <- merge(
            balances, binance_coins_prices(),
            by.x = 'asset', by.y = 'symbol', all.x = TRUE, all.y = FALSE)
        balances[, usd := usd * total]
    }

    balances[total > threshold]
}


#' Get trades for a specific symbol on the Binance account
#' @param symbol string
#' @param limit optional int number of trades to fetch
#' @param from_id optional trade id to fetch from
#' @param start_time optional POSIX timestamp
#' @param end_time optional POSIX timestamp
#' @return data.table
#' @export
#' @importFrom data.table as.data.table setnames
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_mytrades('ARKETH')
#' binance_mytrades(c('ARKBTC', 'ARKETH'))
#' }
binance_mytrades <- function(symbol, limit, from_id, start_time, end_time) {

    # silence "no visible global function/variable definition" R CMD check
    time <- NULL

    if (length(symbol) > 1) {
        return(rbindlist(lapply(symbol, binance_mytrades), fill = TRUE))
    }

    params <- list(symbol = symbol)

    if (!missing(limit)) {
        stopifnot(limit <= 1000L)
        params$limit <- limit
    }
    if (!missing(from_id)) {
        params$fromId <- from_id
    }
    if (!missing(start_time)) {
        params$startTime <- format(as.numeric(as.POSIXct(start_time)) * 1e3, scientific = FALSE)
    }
    if (!missing(end_time)) {
        params$endTime <- format(as.numeric(as.POSIXct(end_time)) * 1e3, scientific = FALSE)
    }

    trades <- binance_query(endpoint = 'api/v3/myTrades', params = params, sign = TRUE)
    trades <- rbindlist(trades)

    if (nrow(trades) == 0) {
        return(data.table())
    } else {
        trades[, time := as.POSIXct(time/1e3, origin = '1970-01-01')]
    }

    # return with snake_case column names
    setnames(trades, to_snake_case(names(trades)))
    data.table(trades)

}


#' Open new order on the Binance account
#' @param symbol string
#' @param side enum
#' @param type enum
#' @param time_in_force optional enum
#' @param quantity number
#' @param price optional number
#' @param stop_price optional number
#' @param iceberg_qty optional number
#' @param test bool
#' @return data.table
#' @export
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_new_order('ARKETH', side = 'BUY', type = 'MARKET', quantity = 1)
#' binance_new_order('ARKBTC', side = 'BUY', type = 'LIMIT', quantity = 1,
#'                   price = 0.5, time_in_force = 'GTC')
#' }
binance_new_order <- function(symbol, side, type, time_in_force, quantity, price, stop_price, iceberg_qty, test = TRUE) {

    # silence "no visible global function/variable definition" R CMD check
    filterType <- minQty <- maxQty <- stepSize <- applyToMarket <- avgPriceMins <- limit <- NULL
    minNotional <- minPrice <- maxPrice <- tickSize <- multiplierDown <- multiplierUp <- NULL

    side <- match.arg(side)
    type <- match.arg(type)

    # check for additional mandatory parameters based on type
    if (type == 'LIMIT') {
        stopifnot(!missing(time_in_force), !missing(price))
    }
    if (type == 'STOP_LOSS' | type == 'TAKE_PROFIT') {
        stopifnot(!missing(stop_price))
    }
    if (type == 'STOP_LOSS_LIMIT' | type == 'TAKE_PROFIT_LIMIT') {
        stopifnot(!missing(time_in_force), !missing(price), !missing(stop_price))
    }
    if (type == 'LIMIT_MAKER') {
        stopifnot(!missing(price))
    }

    params <- list(symbol   = symbol,
                   side     = side,
                   type     = type,
                   quantity = quantity)

    if (!missing(time_in_force)) {
        time_in_force <- match.arg(time_in_force)
        params$timeInForce = time_in_force
    }

    # get filters and check
    filters <- binance_filters(symbol)

    stopifnot(quantity >= filters[filterType == 'LOT_SIZE', minQty],
              quantity <= filters[filterType == 'LOT_SIZE', maxQty])
    # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
    quot <- (quantity - filters[filterType == 'LOT_SIZE', minQty]) / filters[filterType == 'LOT_SIZE', stepSize]
    stopifnot(abs(quot - round(quot)) < 1e-10)

    if (type == 'MARKET') {
        stopifnot(quantity >= filters[filterType == 'MARKET_LOT_SIZE', minQty],
                  quantity <= filters[filterType == 'MARKET_LOT_SIZE', maxQty])
        # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
        quot <- (quantity - filters[filterType == 'MARKET_LOT_SIZE', minQty]) / filters[filterType == 'MARKET_LOT_SIZE', stepSize]
        stopifnot(abs(quot - round(quot)) < 1e-10)

        if (isTRUE(filters[filterType == 'MIN_NOTIONAL', applyToMarket])) {
            if (filters[filterType == 'MIN_NOTIONAL', avgPriceMins] == 0) {
                ref_price <- binance_ticker_price(symbol)$price
            } else {
                ref_price <- binance_avg_price(symbol)
                stopifnot(ref_price$mins == filters[filterType == 'MIN_NOTIONAL', avgPriceMins])
                ref_price <- ref_price$price
            }
            stopifnot(ref_price * quantity >= filters[filterType == 'MIN_NOTIONAL', minNotional])
        }
    }

    if (!missing(price)) {
        stopifnot(price >= filters[filterType == 'PRICE_FILTER', minPrice])
        if (filters[filterType == 'PRICE_FILTER', maxPrice] > 0) {
            stopifnot(price <= filters[filterType == 'PRICE_FILTER', maxPrice])
        }
        if (filters[filterType == 'PRICE_FILTER', tickSize] > 0) {
            # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
            quot <- (price - filters[filterType == 'PRICE_FILTER', minPrice]) / filters[filterType == 'PRICE_FILTER', tickSize]
            stopifnot(abs(quot - round(quot)) < 1e-10)
        }

        if (filters[filterType == 'PERCENT_PRICE', avgPriceMins] == 0) {
            ref_price <- binance_ticker_price(symbol)$price
        } else {
            ref_price <- binance_avg_price(symbol)
            stopifnot(ref_price$mins == filters[filterType == 'PERCENT_PRICE', avgPriceMins])
            ref_price <- ref_price$price
        }
        stopifnot(
            price >= ref_price * filters[filterType == 'PERCENT_PRICE', multiplierDown],
            price <= ref_price * filters[filterType == 'PERCENT_PRICE', multiplierUp]
        )

        stopifnot(price * quantity >= filters[filterType == 'MIN_NOTIONAL', minNotional])

        params$price = price
    }

    if (!missing(stop_price)) {
        stopifnot(stop_price >= filters[filterType == 'PRICE_FILTER', minPrice])
        if (filters[filterType == 'PRICE_FILTER', maxPrice] > 0) {
            stopifnot(stop_price <= filters[filterType == 'PRICE_FILTER', maxPrice])
        }
        if (filters[filterType == 'PRICE_FILTER', tickSize] > 0) {
            # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
            quot <- (stop_price - filters[filterType == 'PRICE_FILTER', minPrice]) / filters[filterType == 'PRICE_FILTER', tickSize]
            stopifnot(abs(quot - round(quot)) < 1e-10)
        }
        params$stopPrice = stop_price
    }

    if (!missing(iceberg_qty)) {
        if (iceberg_qty > 0) {
            stopifnot(time_in_force == 'GTC')
            stopifnot(ceiling(quantity / iceberg_qty) <= filters[filterType == 'ICEBERG_PARTS', limit])
            stopifnot(iceberg_qty >= filters[filterType == 'LOT_SIZE', minQty],
                      iceberg_qty <= filters[filterType == 'LOT_SIZE', maxQty])
            # work around the limitation of %% (e.g. 200.1 %% 0.1 = 0.1 !!)
            quot <- (iceberg_qty - filters[filterType == 'LOT_SIZE', minQty]) / filters[filterType == 'LOT_SIZE', stepSize]
            stopifnot(abs(quot - round(quot)) < 1e-10)
        }
        params$icebergQty = iceberg_qty
    }

    if (isTRUE(test)) {
        message('TEST')
        ord <- binance_query(endpoint = 'api/v3/order/test', method = 'POST', params = params, sign = TRUE)
        if (is.list(ord) & length(ord) == 0) {
            ord <- 'OK'
        }
    } else {
        ord <- binance_query(endpoint = 'api/v3/order', method = 'POST', params = params, sign = TRUE)

        ord$fills <- NULL

        ord <- as.data.table(ord)

        for (v in c('price', 'origQty', 'executedQty', 'cummulativeQuoteQty')) {
            ord[, (v) := as.numeric(get(v))]
        }

        for (v in c('transactTime')) {
            ord[, (v) := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
        }

        # return with snake_case column names
        setnames(ord, to_snake_case(names(ord)))
    }

    data.table(ord)
}
formals(binance_new_order)$side <- BINANCE$SIDE
formals(binance_new_order)$type <- BINANCE$SPOT$TYPE
formals(binance_new_order)$time_in_force <- BINANCE$SPOT$TIMEINFORCE


#' Query order on the Binance account
#' @param symbol string
#' @param order_id optional number
#' @param client_order_id optional string
#' @return data.table
#' @export
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_query_order('ARKETH')
#' binance_query_order('ARKBTC', client_order_id = 'myOrder7')
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

    ord <- binance_query(endpoint = 'api/v3/order', method = 'GET', params = params, sign = TRUE)
    ord <- as.data.table(ord)

    # ncol(ord) == 2 is error message
    if (nrow(ord) > 0 & ncol(ord) > 2) {
        for (v in c('price', 'origQty', 'executedQty', 'cummulativeQuoteQty', 'stopPrice', 'icebergQty')) {
            ord[, (v) := as.numeric(get(v))]
        }

        for (v in c('time', 'updateTime')) {
            ord[, (v) := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
        }

        # return with snake_case column names
        setnames(ord, to_snake_case(names(ord)))
    }
    data.table(ord)
}


#' Cancel order on the Binance account
#' @param symbol string
#' @param order_id optional number
#' @param client_order_id optional string
#' @return data.table
#' @export
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_cancel_order('ARKETH', order_id = 123456)
#' binance_cancel_order('ARKBTC', client_order_id = 'myOrder7')
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

    ord <- binance_query(endpoint = 'api/v3/order', method = 'DELETE', params = params, sign = TRUE)
    ord <- as.data.table(ord)

    # ncol(ord) == 2 is error message
    if (nrow(ord) > 0 & ncol(ord) > 2) {
        for (v in c('price', 'origQty', 'executedQty', 'cummulativeQuoteQty')) {
            ord[, (v) := as.numeric(get(v))]
        }

        # return with snake_case column names
        setnames(ord, to_snake_case(names(ord)))
    }
    data.table(ord)
}


#' Fetch open orders from the Binance account
#' @param symbol optional string
#' @return data.table
#' @export
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_open_orders('ARKETH')
#' binance_open_orders() # all symbols - binance.weight 40
#' }
binance_open_orders <- function(symbol) {

    if (!missing(symbol)) {
        params <- list(symbol = symbol)
    } else {
        params <- list()
    }

    ord <- binance_query(endpoint = 'api/v3/openOrders', params = params, sign = TRUE)

    if (is.null(names(ord))) {
        ord <- rbindlist(ord)
    } else {
        ord <- as.data.table(ord)
    }

    if (nrow(ord) > 0) {
        for (v in c('price', 'origQty', 'executedQty', 'cummulativeQuoteQty', 'stopPrice', 'icebergQty')) {
            ord[, (v) := as.numeric(get(v))]
        }

        for (v in c('time', 'updateTime')) {
            ord[, (v) := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
        }

        # return with snake_case column names
        setnames(ord, to_snake_case(names(ord)))
    }
    ord
}


#' Fetch all orders from the Binance account
#' @param symbol string
#' @param order_id optional number
#' @param start_time optional POSIX timestamp
#' @param end_time optional POSIX timestamp
#' @param limit optional int
#' @return data.table
#' @export
#' @importFrom snakecase to_snake_case
#' @examples \dontrun{
#' binance_all_orders('ARKETH')
#' binance_all_orders('ARKBTC', order_id = '123456')
#' }
binance_all_orders <- function(symbol, order_id, start_time, end_time, limit) {

    params <- list(symbol   = symbol)

    if (!missing(order_id)) {
        params$orderId <- order_id
    }
    if (!missing(start_time)) {
        params$startTime <- format(as.numeric(as.POSIXct(start_time)) * 1e3, scientific = FALSE)
    }
    if (!missing(end_time)) {
        params$endTime <- format(as.numeric(as.POSIXct(end_time)) * 1e3, scientific = FALSE)
    }
    if (!missing(limit)) {
        stopifnot(limit <= 1000L)
        params$limit <- limit
    }

    ord <- binance_query(endpoint = 'api/v3/allOrders', params = params, sign = TRUE)
    ord <- rbindlist(ord)

    if (nrow(ord) > 0) {
        for (v in c('price', 'origQty', 'executedQty', 'cummulativeQuoteQty', 'stopPrice', 'icebergQty')) {
            ord[, (v) := as.numeric(get(v))]
        }

        for (v in c('time', 'updateTime')) {
            ord[, (v) := as.POSIXct(get(v)/1e3, origin = '1970-01-01')]
        }

        # return with snake_case column names
        setnames(ord, to_snake_case(names(ord)))
    }
    data.table(ord)
}
