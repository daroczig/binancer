credentials <- new.env()

.onLoad <- function(libname, pkgname) {
    formals(get_klines)$interval <<- INTERVALS
}
