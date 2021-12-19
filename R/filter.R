#' @importFrom assertive assert_is_numeric
#' @importFrom logger log_warn
#' @importFrom stringr str_glue

# Validate both of price and quantity
# This should resolve the problem such as 200.1 %% 0.1 == 0.1
validate_scale <- function(x, min, max, step, digits = 8) {
    a <- x - min
    b <- a / step
    round(b - round(b), digits) == 0 &&
        x >= min && x <= max
}

binance_filter <- function(type, params) {
    structure(
        params,
        class = type
    )
}

usdm_filter_check <- function(self, ...) {
    UseMethod("usdm_filter_check")
}

format.PRICE_FILTER <- function(self) {
    str_glue("PRICE_FILTER({self$minPrice}, {self$maxPrice}, {self$tickSize})")
}

usdm_filter_check.PRICE_FILTER <- function(self, price) {
    min <- self$minPrice
    assert_is_numeric(min)
    max <- self$maxPrice
    assert_is_numeric(max)
    step <- self$tickSize
    assert_is_numeric(step)

    result <- validate_scale(price, min, max, step)

    if (isFALSE(result)) {
        log_warn("{format(self)} failed: {price}")
    }

    result
}

format.LOT_SIZE <- function(self) {
    str_glue("LOT_SIZE({self$minQty}, {self$maxQty}, {self$stepSize})")
}

usdm_filter_check.LOT_SIZE <- function(self, quantity) {
    min <- self$minQty
    assert_is_numeric(min)
    max <- self$maxQty
    assert_is_numeric(max)
    step <- self$stepSize
    assert_is_numeric(step)

    result <- validate_scale(quantity, min, max, step)

    if (isFALSE(result)) {
        log_warn("{format(self)} failed: {quantity}")
    }

    result
}

format.MARKET_LOT_SIZE <- function(self) {
    str_glue("MARKET_LOT_SIZE({self$minQty}, {self$maxQty}, {self$stepSize})")
}

usdm_filter_check.MARKET_LOT_SIZE <- usdm_filter_check.LOT_SIZE

format.MAX_NUM_ORDERS <- function(self) {
    str_glue("MAX_NUM_ORDERS({self$limit})")
}

usdm_filter_check.MAX_NUM_ORDERS <- function(self, number) {
    limit <- self$limit
    assert_is_numeric(limit)
    assert_is_numeric(number)

    result <- number <= limit

    if (isFALSE(result)) {
        log_warn("{format(self)} failed: {number}")
    }

    result
}

format.MAX_NUM_ALGO_ORDERS <- function(self) {
    str_glue("MAX_NUM_ALGO_ORDERS({self$limit})")
}

usdm_filter_check.MAX_NUM_ALGO_ORDERS <- usdm_filter_check.MAX_NUM_ORDERS

format.PERCENT_PRICE <- function(self) {
    str_glue("PERCENT_PRICE({self$multiplierDown}, {self$multiplierUp})")
}

usdm_filter_check.PERCENT_PRICE <- function(self,
                                            price,
                                            mark_price,
                                            side = c("BUY", "SELL")) {
    side <- match.arg(side)

    assert_is_numeric(price)
    assert_is_numeric(mark_price)
    down <- self$multiplierDown
    assert_is_numeric(down)
    up <- self$multiplierUp
    assert_is_numeric(up)

    result <- (side == "BUY" && price <= mark_price * up) ||
        (side == "SELL" && price >= mark_price * down)

    if (isFALSE(result)) {
        log_warn("{format(self)} failed: {price}/{mark_price}")
    }

    result
}

format.MIN_NOTIONAL <- function(self) {
    str_glue("MIN_NOTIONAL({self$notional})")
}

usdm_filter_check.MIN_NOTIONAL <- function(self, price, quantity) {
    notional <- self$notional
    assert_is_numeric(notional)
    assert_is_numeric(price)
    assert_is_numeric(quantity)

    result <- price * quantity >= notional

    if (isFALSE(result)) {
        log_warn("{format(self)} failed: {price} * {quantity} = {price * quantity}")
    }

    result
}
