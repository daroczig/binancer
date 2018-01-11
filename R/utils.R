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
signature <- function(params) {
    params <- paste(mapply(paste, params, names(params), sep = '=', USE.NAMES = FALSE), collapse = '&')
    hmac(binance_secret(), params, algo = 'sha256')
}


#' Request the Binance API
#' @param endpoint string
#' @param method HTTP request method
#' @param params list
#' @return R object
#' @keywords internal
#' @importFrom httr GET content
binance_query <- function(endpoint, method = 'GET', params) {

    content(GET('https://api.binance.com',
                path  = endpoint,
                query = params))

}
