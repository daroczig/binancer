#' Get current Binance account information, including balances
#' @return data.table
#' @export
#' @importFrom data.table as.data.table
binance_account <- function() {
    as.data.table(binance_query(endpoint = 'api/v3/account', sign = TRUE))
}


#' Get all trades on the Binance account
#' @param symbol string
#' @return data.table
#' @export
#' @importFrom data.table as.data.table
#' @examples \dontrun{
#' binance_mytrades('ETH')
#' }
#' @importFrom snakecase to_snake_case
binance_mytrades <- function(symbol) {

    trades <- binance_query(
        endpoint = 'api/v3/myTrades',
        params = list(symbol = symbol),
        sign = TRUE)

    ## return with snake_case column names
    trades <- rbindlist(trades)
    setnames(trades, to_snake_case(names(trades)))

}
