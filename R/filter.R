# A filter for both of price and quantity
# This should resolve the problem such as 200.1 %% 0.1 == 0.1
scale_filter <- function(x, min, max, step, digits = 8) {
    a <- x - min
    b <- a / step
    round(b - round(b), digits) == 0 &&
        x >= min && x <= max
}
