credentials <- new.env()

.onLoad <- function(libname, pkgname) {
    formals(binance_klines)$interval <<- INTERVALS
}
