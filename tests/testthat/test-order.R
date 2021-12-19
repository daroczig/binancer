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
