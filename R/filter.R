#' @importFrom assertive assert_is_numeric
#' @importFrom data.table %chin%
#' @importFrom logger log_warn
#' @importFrom stringr str_glue

# Validate both of price and quantity
# This should resolve the problem such as 200.1 %% 0.1 == 0.1
validate_scale <- function(x, min, max, step, digits = 8) {
    assert_is_numeric(x)
    assert_is_numeric(min)
    assert_is_numeric(max)
    assert_is_numeric(step)

    a <- x - min
    b <- a / step
    round(b - round(b), digits) == 0 &&
        x >= min && x <= max
}

usdm_order_context <- function(open_orders = usdm_v1_open_orders(),
                               premium_index = usdm_v1_premium_index()) {
    structure(
        list(
            open_orders = open_orders,
            premium_index = premium_index
        ),
        class = "usdm_order_context"
    )
}

order_number <- function(context) {
    nrow(context$open_orders)
}

algo_order_number <- function(context) {
    type <- NULL
    nrow(context$open_orders[type %chin% algo_order_type, ])
}

mark_price <- function(context, symbol) {
    symbol_q <- symbol
    context$premium_index[symbol == symbol_q, markPrice]
}

binance_filter <- function(type, params) {
    structure(
        params,
        class = type
    )
}

usdm_filter_check <- function(self, order, context) {
    UseMethod("usdm_filter_check")
}

log_if_false <- function(self, order, f) {
    result <- f()

    if (isFALSE(result)) {
        log_warn("{format(self)} failed: {format(order)}")
    }

    result
}

format.PRICE_FILTER <- function(self) {
    str_glue("PRICE_FILTER({self$minPrice}, {self$maxPrice}, {self$tickSize})")
}

usdm_filter_check.PRICE_FILTER <- function(self, order, context) {
    log_if_false(self, order, function() {
        validate_scale(order$price, self$minPrice, self$maxPrice, self$tickSize)
    })
}

format.LOT_SIZE <- function(self) {
    str_glue("LOT_SIZE({self$minQty}, {self$maxQty}, {self$stepSize})")
}

usdm_filter_check.LOT_SIZE <- function(self, order, context) {
    log_if_false(self, order, function() {
        validate_scale(order$quantity, self$minQty, self$maxQty, self$stepSize)
    })
}

format.MARKET_LOT_SIZE <- function(self) {
    str_glue("MARKET_LOT_SIZE({self$minQty}, {self$maxQty}, {self$stepSize})")
}

usdm_filter_check.MARKET_LOT_SIZE <- usdm_filter_check.LOT_SIZE

format.MAX_NUM_ORDERS <- function(self) {
    str_glue("MAX_NUM_ORDERS({self$limit})")
}

order_number_check <- function(self, number) {
    limit <- self$limit
    assert_is_numeric(limit)

    number < limit
}

usdm_filter_check.MAX_NUM_ORDERS <- function(self, order, context) {
    log_if_false(self, order, function() {
        order_number_check(self, order_number(context))
    })
}

format.MAX_NUM_ALGO_ORDERS <- function(self) {
    str_glue("MAX_NUM_ALGO_ORDERS({self$limit})")
}

usdm_filter_check.MAX_NUM_ALGO_ORDERS <- function(self, order, context) {
    log_if_false(self, order, function() {
        order_number_check(self, algo_order_number(context))
    })
}

format.PERCENT_PRICE <- function(self) {
    str_glue("PERCENT_PRICE({self$multiplierDown}, {self$multiplierUp})")
}

usdm_filter_check.PERCENT_PRICE <- function(self, order, context) {
    down <- self$multiplierDown
    assert_is_numeric(down)
    up <- self$multiplierUp
    assert_is_numeric(up)

    mark_price <- mark_price(context, order$symbol)

    log_if_false(self, order, function() {
        (is_buy(order) && order$price <= mark_price * up) ||
            (is_sell(order) && order$price >= mark_price * down)
    })
}

format.MIN_NOTIONAL <- function(self) {
    str_glue("MIN_NOTIONAL({self$notional})")
}

usdm_filter_check.MIN_NOTIONAL <- function(self, order, context) {
    notional <- self$notional
    assert_is_numeric(notional)
    ref_price <- if (inherits(order, "LIMIT")) {
        order$price
    } else {
        mark_price(context, order$symbol)
    }

    log_if_false(self, order, function() {
        ref_price * order$quantity >= notional
    })
}
