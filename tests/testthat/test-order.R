test_that("usdm_limit_order", {
    order <- usdm_limit_order("BTCUSDT", 5, 120)

    expect_equal(order$price, 5)
    expect_equal(order$quantity, 120)
    expect_equal(order$side, "BUY")
})
