library("vcr") # *Required* as vcr is set up on loading

invisible(vcr::vcr_configure(
    filter_sensitive_data = list(
        "<<<api_key>>>" = Sys.getenv("BINANCE_API_KEY"),
        "<<<secret>>>" = Sys.getenv("BINANCE_SECRET")
    ),
    dir = vcr::vcr_test_path("fixtures")
))

vcr::check_cassette_names()
