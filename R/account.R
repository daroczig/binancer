#' Get current account information, including balances
#' @return data.table
#' @export
#' @importFrom data.table as.data.table
binance_account <- function() {
    as.data.table(binance_query(endpoint = 'api/v3/account', sign = TRUE))
}
