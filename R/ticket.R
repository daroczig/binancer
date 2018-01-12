#' Get latest prices on all symbols
#' @return data.table
#' @export
#' @importFrom data.table rbindlist
binance_ticker_all_prices <- function() {

    prices <- binance_query(endpoint = 'api/v1/ticker/allPrices')
    prices <- rbindlist(prices)
    prices[, price := as.numeric(price)]
    prices

}
