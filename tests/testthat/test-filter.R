test_that("validate_scale", {
    expect_true(validate_scale(200, 1, 200, 1))
    expect_false(validate_scale(201, 1, 200, 1))
    expect_true(validate_scale(1, 1, 200, 1))
    expect_false(validate_scale(0, 1, 200, 1))
    expect_false(validate_scale(200.5, 1, 200, 1))
    expect_true(validate_scale(210.1, 10, 300, 0.1))
    expect_false(validate_scale(210.11, 10, 300, 0.1))
    expect_true(validate_scale(210.11, 10, 300, 1e-8))
    expect_true(validate_scale(210.111, 10, 300, 1e-8))
})

test_that("usdm_filter_check.UNKNOWN", {
    filter <- binance_filter("UNKNOWN", list())
    expect_error(usdm_filter_check("foo"))
})

test_that("usdm_filter_check.PRICE_FILTER", {
    params <- data.table(
        filterType = "PRICE_FILTER",
        minPrice = 200,
        maxPrice = 400,
        tickSize = 0.01
    )
    filter <- binance_filter(params$filterType, params)

    expect_true(usdm_filter_check(filter, 200))
    expect_true(usdm_filter_check(filter, 400))
    expect_true(usdm_filter_check(filter, 200.1))
    expect_true(usdm_filter_check(filter, 200.01))
    expect_false(usdm_filter_check(filter, 10))
    expect_false(usdm_filter_check(filter, 500))
    expect_false(usdm_filter_check(filter, 200.001))

    params <- data.table(
        filterType = "PRICE_FILTER",
        minPrice = "200",
        maxPrice = 400,
        tickSize = 0.01
    )
    filter <- binance_filter(params$filterType, params)
    expect_error(usdm_filter_check(filter, 200))
})

test_that("usdm_filter_check.LOT_SIZE", {
    params <- data.table(
        filterType = "LOT_SIZE",
        minQty = 200,
        maxQty = 400,
        stepSize = 0.01
    )
    filter <- binance_filter(params$filterType, params)

    expect_true(usdm_filter_check(filter, 200.01))

    params <- data.table(
        filterType = "LOT_SIZE",
        minQty = 200,
        maxQty = "400",
        stepSize = 0.01
    )
    filter <- binance_filter(params$filterType, params)
    expect_error(usdm_filter_check(filter, 200))
})

test_that("usdm_filter_check.MARKET_LOT_SIZE", {
    params <- data.table(
        filterType = "MARKET_LOT_SIZE",
        minQty = 200,
        maxQty = 400,
        stepSize = 0.01
    )
    filter <- binance_filter(params$filterType, params)

    expect_true(usdm_filter_check(filter, 200.01))
})

test_that("usdm_filter_check.MAX_NUM_ORDERS", {
    params <- data.table(
        filterType = "MAX_NUM_ORDERS",
        limit = 10
    )
    filter <- binance_filter(params$filterType, params)

    expect_true(usdm_filter_check(filter, 9))
    expect_true(usdm_filter_check(filter, 10))
    expect_false(usdm_filter_check(filter, 11))
    expect_error(usdm_filter_check(filter, "10"))
})

test_that("usdm_filter_check.MAX_NUM_ALGO_ORDERS", {
    params <- data.table(
        filterType = "MAX_NUM_ALGO_ORDERS",
        limit = 10
    )
    filter <- binance_filter(params$filterType, params)

    expect_true(usdm_filter_check(filter, 9))
    expect_true(usdm_filter_check(filter, 10))
    expect_false(usdm_filter_check(filter, 11))
})
