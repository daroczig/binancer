# A filter for both of price and quantity
# This should resolve the problem such as 200.1 %% 0.1 == 0.1
scale_filter <- function(x, min, max, step, digits = 8) {
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

usdm_filter_check.PRICE_FILTER <- function(self, price) {
    scale_filter(price, self$minPrice, self$maxPrice, self$tickSize)
}
