as_time <- function(number) as.POSIXct(number, origin = "1970-01-01")

test_that("Ping on Spot", {
    vcr::use_cassette("spot_ping", {
        response <- binance_ping()
    })

    expect_equal(response, "OK")
})

test_that("Time on Spot", {
    vcr::use_cassette("spot_time", {
        response <- binance_time()
    })

    expect_equal(response, as_time(1639747098))
})

test_that("Exchange info on Spot", {
    vcr::use_cassette("spot_exchange_info", {
        response <- binance_exchange_info()
    })

    expect_equal(
        response$serverTime,
        as_time(1639754298)
    )
})

test_that("Klines on Spot", {
    vcr::use_cassette("spot_klines_ethusdt_1d_limit_1", {
        response <- binance_klines("ETHUSDT", interval = "1d", limit = 1)
    })

    expect_equal(response$symbol, "ETHUSDT")
    expect_equal(
        response$open_time,
        as_time(1639699200)
    )
    expect_equal(
        response$close_time,
        as_time(1639785600)
    )
    expect_equal(response$open, 3959.1, tolerance = 0.01)
    expect_equal(response$high, 3959.1, tolerance = 0.01)
    expect_equal(response$low, 3796, tolerance = 0.01)
    expect_equal(response$close, 3821, tolerance = 0.01)
    expect_equal(response$volume, 210423, tolerance = 0.01)
    expect_equal(response$quote_asset_volume, 819993474)
    expect_equal(response$trades, 470771)
    expect_equal(
        response$taker_buy_base_asset_volume,
        105358.2,
        tolerance = 0.01
    )
    expect_equal(response$taker_buy_quote_asset_volume, 411011816)
})
