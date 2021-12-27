#' @importFrom data.table %chin%
#' @importFrom stringr str_glue

algo_order_type <- c(
    "STOP",
    "STOP_MARKET",
    "TAKE_PROFIT",
    "TAKE_PROFIT_MARKET",
    "TRAILING_STOP_MARKET"
)

is_buy <- function(self) identical(self$side, "BUY")
is_sell <- function(self) identical(self$side, "SELL")
is_algo_order <- function(self) inherits(self, algo_order_type)

usdm_limit_order <- function(symbol,
                             price,
                             quantity,
                             side = BINANCE$SIDE,
                             position_side = BINANCE$USDM$POSITION_SIDE,
                             time_in_force = BINANCE$USDM$TIMEINFORCE) {
    structure(
        list(
            symbol = symbol,
            price = price,
            quantity = quantity,
            side = match.arg(side),
            position_side = match.arg(position_side),
            time_in_force = match.arg(time_in_force)
        ),
        class = "LIMIT"
    )
}

#' Open long position at a limited price
#'
#' @param symbol string
#' @param price numeric
#' @param quantity numeric
#' @param time_in_force enum
#' @return data.table
#' @export
open_long_limit <- function(symbol, price, quantity, time_in_force = "GTC") {
    usdm_limit_order(
        symbol,
        price,
        quantity,
        time_in_force = time_in_force,
        side = "BUY",
        position_side = "LONG"
    )
}

#' Close long position at a limited price
#'
#' @param symbol string
#' @param price numeric
#' @param quantity numeric
#' @param time_in_force enum
#' @return data.table
#' @export
close_long_limit <- function(symbol, price, quantity, time_in_force = "GTC") {
    usdm_limit_order(
        symbol,
        price,
        quantity,
        time_in_force = time_in_force,
        side = "SELL",
        position_side = "LONG"
    )
}

#' Open short position at a limited price
#'
#' @param symbol string
#' @param price numeric
#' @param quantity numeric
#' @param time_in_force enum
#' @return data.table
#' @export
open_short_limit <- function(symbol, price, quantity, time_in_force = "GTC") {
    usdm_limit_order(
        symbol,
        price,
        quantity,
        time_in_force = time_in_force,
        side = "SELL",
        position_side = "SHORT"
    )
}

#' Close short position at a limited price
#'
#' @param symbol string
#' @param price numeric
#' @param quantity numeric
#' @param time_in_force enum
#' @return data.table
#' @export
close_short_limit <- function(symbol, price, quantity, time_in_force = "GTC") {
    usdm_limit_order(
        symbol,
        price,
        quantity,
        time_in_force = time_in_force,
        side = "BUY",
        position_side = "SHORT"
    )
}

#' @export
format.LIMIT <- function(x, ...) {
    str_glue("LIMIT({x$symbol}, {x$side}, {x$position_side}, {x$price}, {x$quantity})")
}

usdm_market_order <- function(symbol,
                              quantity,
                              side = BINANCE$SIDE,
                              position_side = BINANCE$USDM$POSITION_SIDE) {
    structure(
        list(
            symbol = symbol,
            quantity = quantity,
            side = match.arg(side),
            position_side = match.arg(position_side)
        ),
        class = "MARKET"
    )
}

#' @export
format.MARKET <- function(x, ...) {
    str_glue("MARKET({x$symbol}, {x$side}, {x$position_side}, {x$quantity})")
}

#' Open long position at market price
#'
#' @param symbol string
#' @param quantity numeric
#' @return data.table
#' @export
open_long_market <- function(symbol, quantity) {
    usdm_market_order(
        symbol,
        quantity,
        side = "BUY",
        position_side = "LONG"
    )
}

#' Close long position at market price
#'
#' @param symbol string
#' @param quantity numeric
#' @return data.table
#' @export
close_long_market <- function(symbol, quantity) {
    usdm_market_order(
        symbol,
        quantity,
        side = "SELL",
        position_side = "LONG"
    )
}

#' Open short position at market price
#'
#' @param symbol string
#' @param quantity numeric
#' @return data.table
#' @export
open_short_market <- function(symbol, quantity) {
    usdm_market_order(
        symbol,
        quantity,
        side = "SELL",
        position_side = "SHORT"
    )
}

#' Close short position at market price
#'
#' @param symbol string
#' @param quantity numeric
#' @return data.table
#' @export
close_short_market <- function(symbol, quantity) {
    usdm_market_order(
        symbol,
        quantity,
        side = "BUY",
        position_side = "SHORT"
    )
}

default_usdm_filters <- function(self) {
    UseMethod("default_usdm_filters")
}

default_usdm_filters.LIMIT <- function(self) {
    setdiff(BINANCE$USDM$FILTER, "MARKET_LOT_SIZE")
}

default_usdm_filters.MARKET <- function(self) {
    c(
        "MARKET_LOT_SIZE",
        "MAX_NUM_ORDERS",
        "MAX_NUM_ALGO_ORDERS",
        "MIN_NOTIONAL"
    )
}

is_valid_usdm_order <- function(order,
                                context,
                                filters = usdm_v1_filters(order$symbol),
                                types = default_usdm_filters(order)) {
    filterType <- NULL
    filters <- filters[filterType %chin% types, ]

    results <- apply(filters, 1, function(row) {
        row <- as.list(row)
        row <- as_filters_table(row)
        filter <- binance_filter(row$filterType, row)
        usdm_filter_check(filter, order, context)
    })

    all(results)
}

#' Execute an order on USDM
#'
#' @param order string
#' @return data.table
#' @export
execute_usdm_order <- function(order) {
    UseMethod("execute_usdm_order")
}

#' @export
execute_usdm_order.LIMIT <- function(order) {
    usdm_v1_new_order(
        symbol = order$symbol,
        side = order$side,
        position_side = order$position_side,
        type = "LIMIT",
        price = format(order$price, scientific = FALSE),
        quantity = format(order$quantity, scientific = FALSE),
        timeInForce = order$time_in_force
    )
}

#' @export
execute_usdm_order.MARKET <- function(order) {
    usdm_v1_new_order(
        symbol = order$symbol,
        side = order$side,
        position_side = order$position_side,
        type = "MARKET",
        quantity = format(order$quantity, scientific = FALSE)
    )
}

usdm_take_profit_market_all <- function(symbol,
                                        stop_price,
                                        side = BINANCE$SIDE,
                                        position_side = BINANCE$USDM$POSITION_SIDE) {
    structure(
        list(
            symbol = symbol,
            stop_price = stop_price,
            side = match.arg(side),
            position_side = match.arg(position_side),
            close_position = TRUE
        ),
        class = "TAKE_PROFIT_MARKET"
    )
}

#' @export
format.TAKE_PROFIT_MARKET <- function(x, ...) {
    str_glue("TAKE_PROFIT_MARKET({x$symbol}, {x$side}, {x$position_side}, {x$stop_price}, {x$close_position})")
}

#' Take all profit from a long position at market price
#'
#' @param symbol string
#' @param stop_price numeric
#' @return data.table
#' @export
long_take_profit_market_all <- function(symbol, stop_price) {
    usdm_take_profit_market_all(symbol, stop_price, "SELL", "LONG")
}

#' Take all profit from a short position at market price
#'
#' @param symbol string
#' @param stop_price numeric
#' @return data.table
#' @export
short_take_profit_market_all <- function(symbol, stop_price) {
    usdm_take_profit_market_all(symbol, stop_price, "BUY", "SHORT")
}

#' @export
execute_usdm_order.TAKE_PROFIT_MARKET <- function(order) {
    usdm_v1_new_order(
        symbol = order$symbol,
        side = order$side,
        position_side = order$position_side,
        type = "TAKE_PROFIT_MARKET",
        stopPrice = format(order$stop_price, scientific = FALSE),
        closePosition = tolower(format(order$close_position))
    )
}
