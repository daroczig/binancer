#' Request the Huobi API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @param retry allow retrying the query on failure
#' @return R object
#' @keywords internal
#' @importFrom jsonlite fromJSON
huobi_query <- function(endpoint, method = 'GET',
                          params = list(),
                          retry = method == 'GET') {

    method <- match.arg(method)
    config <- config()

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
