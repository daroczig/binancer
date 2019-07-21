credentials <- new.env()

.onLoad <- function(libname, pkgname) {
    formals(binance_klines)$interval <<- BINANCE$INTERVALS
    formals(binance_new_order)$side <<- BINANCE$SIDE
    formals(binance_new_order)$type <<- BINANCE$TYPE
    formals(binance_new_order)$timeInForce <<- BINANCE$TIMEINFORCE
    formals(binance_query)$method <<- BINANCE$METHOD
    formals(query)$method <<- BINANCE$METHOD
}
