test_that("Binance Ping on Spot", {
    vcr::use_cassette("binance_spot_ping", {
        response <- binance_ping()
    })

    expect_equal(response, "OK")
})

test_that("Binance Time on Spot", {
    vcr::use_cassette("binance_spot_time", {
        response <- binance_time()
    })

    expect_equal(response, as.POSIXct(1639747098, origin = "1970-01-01"))
})
