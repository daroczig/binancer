#' Return current UNIX timestamp in millisecond
#' @return milliseconds since Jan 1, 1970
#' @keywords internal
timestamp <- function() {
    as.character(round(as.numeric(Sys.time()) * 1e3))
}


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
    futile.logger::flog.info(jsonlite::toJSON(params))
    params
}


#' Request the Binance API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @return R object
#' @keywords internal
#' @importFrom httr GET content config add_headers
binance_query <- function(endpoint, method = 'GET', params = list(), sign = FALSE) {

    if (isTRUE(sign)) {
        params <- binance_sign(params)
        config <- add_headers('X-MBX-APIKEY' = binance_key())
    } else {
        config <- config()
    }

    content(GET('https://api.binance.com', config = config, path = endpoint, query = params))

}
