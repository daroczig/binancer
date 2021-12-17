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

test_that("Ticks on Spot", {
    vcr::use_cassette("spot_ticks_ethusdt_limit_2", {
        response <- binance_ticks("ETHUSDT", limit = 2)
    })

    expect_equal(response$agg_trade_id, c(579794207, 579794208))
    expect_equal(response$price, c(3911, 3911), tolerance = 0.01)
    expect_equal(response$quantity, c(0.92, 0.09), tolerance = 0.01)
    expect_equal(response$first_trade_id, c(710227286, 710227288))
    expect_equal(response$last_trade_id, c(710227286, 710227288))
    expect_equal(response$time, as_time(c(1639759382, 1639759382)))
    expect_equal(response$buyer_maker, c(FALSE, FALSE))
    expect_equal(response$best_price_match, c(TRUE, TRUE))
})

test_that("Trades on Spot", {
    vcr::use_cassette("spot_trades_ethusdt_limit_2", {
        response <- binance_trades("ETHUSDT", limit = 2)
    })

    expect_equal(response$id, c(710243051, 710243052))
    expect_equal(response$price, c(3911, 3911), tolerance = 0.01)
    expect_equal(response$qty, c(0.5, 0.01), tolerance = 0.01)
    expect_equal(response$quote_qty, c(1948.526, 48.449), tolerance = 0.01)
    expect_equal(response$time, as_time(c(1639760356, 1639760356)))
    expect_equal(response$is_buyer_maker, c(FALSE, TRUE))
    expect_equal(response$is_best_match, c(TRUE, TRUE))
})

test_that("Depth on Spot", {
    vcr::use_cassette("spot_depth_ethusdt_limit_5", {
        response <- binance_depth("ETHUSDT", limit = 5)
    })

    expect_equal(
        response$bids$price,
        c(3880.79, 3880.69, 3880.63, 3880.59, 3880.58)
    )
    expect_equal(
        response$bids$quantity,
        c(0.448, 0.026, 0.003, 0.182, 0.520),
        tolerance = 0.001
    )
    expect_equal(
        response$asks$price,
        c(3880.80, 3880.83, 3880.84, 3880.85, 3881.01)
    )
    expect_equal(
        response$asks$quantity,
        c(4.60, 0.11, 0.11, 1.08, 0.19),
        tolerance = 0.01
    )
})

test_that("Ticker price on Spot", {
    vcr::use_cassette("spot_ticker_price_ethusdt", {
        response <- binance_ticker_price("ETHUSDT")
    })

    expect_equal(response$symbol, "ETHUSDT")
    expect_equal(response$price, 3866.44, tolerance = 0.01)
})

test_that("Ticker book on Spot", {
    vcr::use_cassette("spot_ticker_book_ethusdt", {
        response <- binance_ticker_book("ETHUSDT")
    })

    expect_equal(response$symbol, "ETHUSDT")
    expect_equal(response$bid_price, 3848.8, tolerance = 0.001)
    expect_equal(response$bid_qty, 0.084, tolerance = 0.01)
    expect_equal(response$ask_price, 3848.8, tolerance = 0.001)
    expect_equal(response$ask_qty, 3.09, tolerance = 0.01)
})

test_that("Average Price on Spot", {
    vcr::use_cassette("spot_avg_price_ethusdt", {
        response <- binance_avg_price("ETHUSDT")
    })

    expect_equal(response$mins, 5)
    expect_equal(response$price, 3862.3, tolerance = 0.001)
})
