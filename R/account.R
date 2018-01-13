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
binance_balances <- function() {
    balances <- rbindlist(binance_account()$balances)
    balances[, free := as.numeric(free)]
    balances[, locked := as.numeric(locked)]
    as.data.table(balances)
}


#' Get all trades on the Binance account
#' @param symbol string
#' @param limit number of trades to fetch
#' @param from_id optional trade id to fetch from
#' @return data.table
#' @export
#' @importFrom data.table as.data.table
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
