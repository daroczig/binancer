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

#' Get mark/index price for a symbol
#' @param symbol string
#' @return \code{data.table}
#' @export
#' @importFrom data.table as.data.table
usdm_v1_premium_index <- function(symbol) {
    nextFundingTime <- time <- NULL

    params <- list(symbol = symbol)
    res <- usdm_query("/fapi/v1/premiumIndex", params = params)
    res <- as.data.table(res)
    for (v in setdiff(names(res), c("symbol", "nextFundingTime", "time"))) {
        res[, (v) := as.numeric(get(v))]
    }
    res[, nextFundingTime := as_timestamp(nextFundingTime)]
    res[, time := as_timestamp(time)]
}

#' Open new order on the Binance USDM account
#'
#' This function serves as a low level entry for order classes.
#' Do not use it directly.
#'
#' @param symbol string
#' @param side enum
#' @param type enum
#' @return data.table
#' @export
usdm_v1_new_order <- function(symbol,
                              side = BINANCE$SIDE,
                              position_side = BINANCE$USDM$POSITION_SIDE,
                              type = BINANCE$USDM$TYPE,
                              ...) {
    params <- list(
        side = match.arg(side),
        positionSide = match.arg(position_side),
        type = match.arg(type),
        ...
    )

    order <- usdm_query(
        "/fapi/v1/order",
        method = "POST",
        params = params,
        sign = TRUE
    )

    as.data.table(order)
}

#' Get all open USDM orders on a symbol.
#'
#' @param symbol string
#' @return data.table
#' @export
usdm_v1_open_orders <- function(symbol) {
    params <- list(
        symbol = symbol
    )

    order <- usdm_query(
        "/fapi/v1/openOrders",
        params = params,
        sign = TRUE
    )

    as.data.table(order)
}
