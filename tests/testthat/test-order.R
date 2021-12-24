test_that("usdm_limit_order", {
    order <- usdm_limit_order("BTCUSDT", 5, 120)

    expect_equal(order$price, 5)
    expect_equal(order$quantity, 120)
    expect_equal(order$side, "BUY")
})

test_that("is_algo_order", {
    expect_false(is_algo_order(structure(list(), class = "LIMIT")))
    expect_true(is_algo_order(structure(list(), class = "STOP")))
    expect_true(is_algo_order(structure(list(), class = "STOP_MARKET")))
    expect_true(is_algo_order(structure(list(), class = "TAKE_PROFIT")))
    expect_true(is_algo_order(structure(list(), class = "TAKE_PROFIT_MARKET")))
    expect_true(
        is_algo_order(structure(list(), class = "TRAILING_STOP_MARKET"))
    )
})

test_that("format", {
    order <- open_long_limit("BTCUSDT", 5, 120)
    expect_equal(format(order), "LIMIT(BTCUSDT, BUY, LONG, 5, 120)")
    order <- open_short_limit("BTCUSDT", 5, 120)
    expect_equal(format(order), "LIMIT(BTCUSDT, SELL, SHORT, 5, 120)")
    order <- close_long_limit("BTCUSDT", 5, 120)
    expect_equal(format(order), "LIMIT(BTCUSDT, SELL, LONG, 5, 120)")
    order <- close_short_limit("BTCUSDT", 5, 120)
    expect_equal(format(order), "LIMIT(BTCUSDT, BUY, SHORT, 5, 120)")

    order <- open_long_market("BTCUSDT", 120)
    expect_equal(format(order), "MARKET(BTCUSDT, BUY, LONG, 120)")
    order <- open_short_market("BTCUSDT", 120)
    expect_equal(format(order), "MARKET(BTCUSDT, SELL, SHORT, 120)")
    order <- close_long_market("BTCUSDT", 120)
    expect_equal(format(order), "MARKET(BTCUSDT, SELL, LONG, 120)")
    order <- close_short_market("BTCUSDT", 120)
    expect_equal(format(order), "MARKET(BTCUSDT, BUY, SHORT, 120)")
})
