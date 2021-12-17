library("vcr") # *Required* as vcr is set up on loading

api_key <- Sys.getenv("BINANCE_API_KEY")
secret <- Sys.getenv("BINANCE_SECRET")

invisible(vcr::vcr_configure(
    filter_sensitive_data = list(
        "<<<api_key>>>" = api_key,
        "<<<secret>>>" = secret
    ),
    dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()

as_time <- function(number) as.POSIXct(number, origin = "1970-01-01")
