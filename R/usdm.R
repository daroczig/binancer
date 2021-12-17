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
