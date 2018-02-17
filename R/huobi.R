#' Request the Huobi API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param sign if signature required
#' @param retry allow retrying the query on failure
#' @return R object
#' @keywords internal
#' @importFrom jsonlite fromJSON
huobi_query <- function(endpoint, method = 'GET',
                          params = list(), sign = FALSE,
                          retry = method == 'GET') {

    method <- match.arg(method)

    if (isTRUE(sign)) {
        ## TODO
        params <- binance_sign(params)
        config <- add_headers('X-MBX-APIKEY' = binance_key())
    } else {
        config <- config()
    }

    fromJSON(rawToChar(query(
        base = 'https://api.huobi.pro',
        path = endpoint,
        method = method,
        params = params,
        config = config)))

}


#' Get kline/candlestick data from Huobi
#' @param symbol string
#' @param period enum
#' @param size int
#' @return list where you are looking for the \code{data} element
#' @export
huobi_klines <- function(symbol,
                         period = c('1min', '5min', '15min', '30min', '60min', '1day', '1mon', '1week', '1year'),
                         size = 500) {

    period <- match.arg(period)
    huobi_query(
        endpoint = 'market/history/kline',
        method = 'GET',
        params = match.call()[[-1]])

}


#' Get Huobi accountid
#' @return string
#' @export
huobi_accountid <- function() {
    huobi_query(endpoint = 'v1/account/accounts', sign = TRUE)
}
