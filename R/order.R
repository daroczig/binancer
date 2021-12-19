price <- function(self) {
    UseMethod("price")
}

price.default <- function(self) self$price

quantity <- function(self) {
    UseMethod("quantity")
}

quantity.default <- function(self) self$quantity

usdm_limit_order <- function(price,
                             quantity,
                             side = BINANCE$SIDE,
                             position_side = BINANCE$USDM$POSITION_SIDE,
                             time_in_force = BINANCE$USDM$TIMEINFORCE) {
    structure(
        list(
            price = price,
            quantity = quantity,
            side = match.arg(side),
            position_side = match.arg(position_side),
            time_in_force = match.arg(time_in_force)
        ),
        class = "LIMIT"
    )
}
