usdm_query <- function(endpoint, ...) {
    binance_query(endpoint, ..., base = "https://fapi.binance.com")
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
    res <- as.POSIXct(res / 1e3, origin = "1970-01-01")
    res
}
