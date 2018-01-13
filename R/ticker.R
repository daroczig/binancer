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
    prices[, from := sub('(ETH|BTC|USD|BNB)$', '', symbol)]
    prices[, to := sub('.*(ETH|BTC|USD|BNB)$', '\\1', symbol)]

    ## add computed price in USD
    prices[grepl('ETH$', symbol), to_usd := prices[symbol == 'ETHUSDT', price]]
    prices[grepl('BTC$', symbol), to_usd := prices[symbol == 'BTCUSDT', price]]
    prices[grepl('USDT$', symbol), to_usd := price]
    prices[grepl('BNB$', symbol), to_usd := prices[symbol == 'BNBUSDT', price]]
    prices[, from_usd := price * to_usd]

    prices[, .(symbol, price, from, from_usd, to, to_usd)]

}


#' Get all currently valid symbol names from Binance
#' @return character vector
#' @export
binance_symbols <- function() {
    binance_ticker_all_prices()$symbol
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
    unique(binance_ticker_all_prices(), by = 'from')[, .(symbol, usd = from_usd)]
}
