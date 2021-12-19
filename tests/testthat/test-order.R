test_that("usdm_limit_order", {
    order <- usdm_limit_order(5, 120)

    expect_equal(price(order), 5)
    expect_equal(quantity(order), 120)
})
