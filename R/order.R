#' @importFrom assertive assert_is_numeric
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
is_algo_order <- function(self) is(self, algo_order_type)

usdm_limit_order <- function(symbol,
                             price,
                             quantity,
                             side = BINANCE$SIDE,
                             position_side = BINANCE$USDM$POSITION_SIDE,
                             time_in_force = BINANCE$USDM$TIMEINFORCE) {
    assert_is_numeric(price)
    assert_is_numeric(quantity)

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

format.LIMIT <- function(self) {
    str_glue("LIMIT({self$symbol}, {self$side}, {self$position_side}, {self$price}, {self$quantity})")
}
