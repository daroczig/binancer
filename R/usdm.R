#' @importFrom data.table rbindlist

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

#' Get the filters of a symbol from Binance USDM
#' @param symbol string
#' @param info list
#' @return \code{data.table}
#' @export
usdm_v1_filters <- function(symbol, info = usdm_v1_exchange_info()) {
    filters <- NULL
    symbol_q <- symbol
    as_filters_table(info$symbols[symbol == symbol_q, filters])

}

as_filters_table <- function(filters) {
    .SD <- NULL
    filters <- as.data.table(filters)
    numeric_columns <- setdiff(names(filters), "filterType")
    filters[
        ,
        (numeric_columns) := lapply(.SD, as.numeric),
        .SDcols = numeric_columns
    ]
}

#' Get mark/index price for a symbol
#' @param symbol optional string
#' @return \code{data.table}
#' @export
#' @importFrom data.table as.data.table
usdm_v1_premium_index <- function(symbol) {
    .SD <- nextFundingTime <- time <- NULL

    params <- list()

    if (!missing(symbol)) {
        params$symbol <- symbol
    }

    res <- usdm_query("/fapi/v1/premiumIndex", params = params)

    if (missing(symbol)) {
        res <- rbindlist(res)
    } else {
        res <- as.data.table(res)
    }

    numeric_columns <- setdiff(
        names(res),
        c("symbol", "nextFundingTime", "time")
    )
    res[
        ,
        (numeric_columns) := lapply(.SD, as.numeric),
        .SDcols = numeric_columns
    ]
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
#' @param position_side enum
#' @param type enum
#' @param ... list
#' @return data.table
#' @export
usdm_v1_new_order <- function(symbol,
                              side = BINANCE$SIDE,
                              position_side = BINANCE$USDM$POSITION_SIDE,
                              type = BINANCE$USDM$TYPE,
                              ...) {
    params <- list(
        symbol = symbol,
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

#' Get all open orders of a symbol on USDM.
#'
#' Get all open orders on a symbol. Careful when accessing this with no symbol.
#' Weight: 1 for a single symbol; 40 when the symbol parameter is omitted.
#'
#' @param symbol optional string
#' @return data.table
#' @export
usdm_v1_open_orders <- function(symbol) {
    params <- list()

    if (!missing(symbol)) {
        params$symbol <- symbol
    }

    order <- usdm_query(
        "/fapi/v1/openOrders",
        params = params,
        sign = TRUE
    )

    rbindlist(order)
}

#' Get positions of a symbol or all symbols on USDM.
#' @param symbol optional string
#' @return data.table
#' @export
usdm_v2_position_risks <- function(symbol) {
    params <- list()

    if (!missing(symbol)) {
        params$symbol <- symbol
    }

    rbindlist(
        usdm_query(
            "/fapi/v2/positionRisk",
            params = params,
            sign = TRUE
        )
    )
}

#' Convert columns of positions into numeric.
#' @param positions data.table
#' @return data.table
#' @export
convert_position_risks <- function(positions) {
    .SD <- isAutoAddMargin <- NULL

    numeric_columns <- c(
        "positionAmt", "entryPrice", "markPrice", "unRealizedProfit",
        "liquidationPrice", "leverage", "maxNotionalValue", "isolatedMargin",
        "notional", "isolatedWallet"
    )
    positions[,
        (numeric_columns) := lapply(.SD, as.numeric),
        .SDcols = numeric_columns
    ]
}

#' Get current account information on USDM.
#' @return list
#' @export
usdm_v2_account <- function() {
    account <- usdm_query("/fapi/v2/account", sign = TRUE)
    account$assets <- rbindlist(account$assets)
    account$positions <- rbindlist(account$positions)
    account
}

#' Change user's initial leverage of specific symbol market.
#' @param symbol string
#' @param leverage integer
#' @return list
#' @export
usdm_v1_change_initial_leverage <- function(symbol, leverage) {
    params <- list(
        symbol = symbol,
        leverage = leverage
    )

    usdm_query(
        "/fapi/v1/leverage",
        method = "POST",
        params = params,
        sign = TRUE
    )
}

#' Change the margin type of specific symbol market.
#' @param symbol string
#' @param type enum
#' @return list
#' @export
usdm_v1_change_margin_type <- function(symbol,
                                       type = c("ISOLATED", "CROSSED")) {
    params <- list(
        symbol = symbol,
        marginType = match.arg(type)
    )

    usdm_query(
        "/fapi/v1/marginType",
        method = "POST",
        params = params,
        sign = TRUE
    )
}

#' Cancel an active order by its order id.
#' @param symbol string
#' @param order_id integer
#' @return data.table
#' @export
usdm_v1_cancel_order_by_id <- function(symbol, order_id) {
    params <- list(
        symbol = symbol,
        orderId = order_id
    )

    order <- usdm_query(
        "/fapi/v1/order",
        method = "DELETE",
        params = params,
        sign = TRUE
    )

    as.data.table(order)
}
