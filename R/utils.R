#' Return current UNIX timestamp in millisecond
#' @return milliseconds since Jan 1, 1970
#' @keywords internal
timestamp <- function() {
    as.character(round(as.numeric(Sys.time()) * 1e3))
}


#' Request the Binance API
#' @param base URL
#' @param path string
#' @param method HTTP request method
#' @param params list
#' @param config httr::config
#' @param retry allow retrying the query on failure
#' @param retries internal counter of previous retries
#' @return R object
#' @keywords internal
#' @importFrom httr GET content config add_headers
#' @importFrom futile.logger flog.error
query <- function(base, path, method = 'GET',
                  params = list(), config = config(),
                  retry = method == 'GET', retries = 0) {

    res <- tryCatch(
        content(GET(base, config = config, path = path, query = params)),
        error = function(e) e)

    if (inherits(res, 'error')) {
        if (isTRUE(retry) & retries < 4) {
            mc <- match.call()
            mc$retries <- mc$retries + 1
            flog.error('Query to %s/%s failed for the %sst/nd/rd/th time, retrying',
                       base, path, mc$retries)
            eval(mc, envir = parent.frame())
        }
    }

    res

}
