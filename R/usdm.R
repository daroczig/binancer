as_timestamp <- function(number) as.POSIXct(number, origin = "1970-01-01")

usdm_query <- function(endpoint, ...) {
    binance_query(endpoint, ..., base = BINANCE$BASE$USDM)
}

#' Test connectivity to the USDM Rest API
#' @export
#' @return "OK" string on success
usdm_v1_ping <- function() {
    res <- usdm_query("/fapi/v1/ping")
    if (is.list(res) & length(res) == 0) {
        res <- "OK"
    }
    res
}

#' Get the current server time from Binance USDM
#' @export
#' @return \code{POSIXct}
usdm_v1_time <- function() {
    res <- usdm_query("/fapi/v1/time")$serverTime
    res <- as_timestamp(res / 1e3)
    res
}

#' Get exchangeInfo from Binance USDM
#' @return \code{list}
#' @export
#' @importFrom jsonlite fromJSON
usdm_v1_exchange_info <- function() {
    res <- usdm_query("/fapi/v1/exchangeInfo", content_as = "text")
    res <- fromJSON(res)
    res$serverTime <- as_timestamp(res$serverTime / 1e3)
    res$rateLimits <- as.data.table(res$rateLimits)
    res$symbols <- as.data.table(res$symbols)
    res
}
