#' Return current UNIX timestamp in millisecond
#' @return milliseconds since Jan 1, 1970
#' @keywords internal
timestamp <- function() {
    as.character(round(as.numeric(Sys.time()) * 1e3))
}


#' Make an API request with retries
#' @param base URL
#' @param path string
#' @param method HTTP request method
#' @param params URL parameters provided as a list
#' @param body body of the request
#' @param config httr::config
#' @param retry allow retrying the query on failure
#' @param retries internal counter of previous retries
#' @return raw object returned by \code{httr}
#' @keywords internal
#' @importFrom httr GET POST PUT DELETE config add_headers
#' @importFrom logger log_error
#' @importFrom utils getFromNamespace
query <- function(base, path, method,
                  params = list(), body = NULL, config = config(),
                  retry = method == 'GET', retries = 0) {

    METHOD <- getFromNamespace(method, ns = 'httr')

    res <- tryCatch(
        METHOD(base, config = config, path = path, query = params, body = body),
        error = function(e) e)

    if (inherits(res, 'error')) {
        if (isTRUE(retry) & retries < 4) {
            mc <- match.call()
            mc$retries <- retries + 1
            log_error(sprintf(
                'Query to %s/%s failed for the %sst/nd/rd/th time, retrying',
                base, path, mc$retries))
            eval(mc, envir = parent.frame())
        }
    }

    res
}
