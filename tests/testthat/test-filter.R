context <- usdm_order_context(
    open_orders = data.table(
        type = c(rep("TRAILING_STOP_MARKET", 4), rep("LIMIT", 4))
    ),
    mark_price = 4
)

test_that("usdm_order_context", {
    expect_equal(order_number(context), 8)
    expect_equal(algo_order_number(context), 4)
    expect_equal(mark_price(context), 4)
})

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
    expect_error(usdm_filter_check(filter, "foo", "bar"))
})

test_that("usdm_filter_check.PRICE_FILTER", {
    params <- data.table(
        filterType = "PRICE_FILTER",
        minPrice = 200,
        maxPrice = 400,
        tickSize = 0.01
    )
    filter <- binance_filter(params$filterType, params)

    expect_equal(format(filter), "PRICE_FILTER(200, 400, 0.01)")

    order <- usdm_limit_order("BTCUSDT", 200, 1)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 400, 1)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 200.1, 1)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 200.01, 1)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 10, 1)
    expect_false(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 500, 1)
    expect_false(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 200.001, 1)
    expect_false(usdm_filter_check(filter, order, context))

    params <- data.table(
        filterType = "PRICE_FILTER",
        minPrice = "200",
        maxPrice = 400,
        tickSize = 0.01
    )
    filter <- binance_filter(params$filterType, params)
    order <- usdm_limit_order("BTCUSDT", 200, 1)
    expect_error(usdm_filter_check(filter, order, context))
})

test_that("usdm_filter_check.LOT_SIZE", {
    params <- data.table(
        filterType = "LOT_SIZE",
        minQty = 5,
        maxQty = 10,
        stepSize = 0.1
    )
    filter <- binance_filter(params$filterType, params)

    expect_equal(format(filter), "LOT_SIZE(5, 10, 0.1)")

    order <- usdm_limit_order("BTCUSDT", 200, 5)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 200, 4)
    expect_false(usdm_filter_check(filter, order, context))
})

test_that("usdm_filter_check.MARKET_LOT_SIZE", {
    params <- data.table(
        filterType = "MARKET_LOT_SIZE",
        minQty = 5,
        maxQty = 10,
        stepSize = 0.1
    )
    filter <- binance_filter(params$filterType, params)

    expect_equal(format(filter), "MARKET_LOT_SIZE(5, 10, 0.1)")

    order <- usdm_limit_order("BTCUSDT", 200, 10)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 200, 11)
    expect_false(usdm_filter_check(filter, order, context))
})

test_that("usdm_filter_check.MAX_NUM_ORDERS", {
    params <- data.table(
        filterType = "MAX_NUM_ORDERS",
        limit = 9
    )
    filter <- binance_filter(params$filterType, params)

    expect_equal(format(filter), "MAX_NUM_ORDERS(9)")

    order <- usdm_limit_order("BTCUSDT", 400.01, 1)
    expect_true(usdm_filter_check(filter, order, context))

    params <- data.table(
        filterType = "MAX_NUM_ORDERS",
        limit = 8
    )
    filter <- binance_filter(params$filterType, params)
    expect_false(usdm_filter_check(filter, order, context))
})

test_that("usdm_filter_check.MAX_NUM_ALGO_ORDERS", {
    params <- data.table(
        filterType = "MAX_NUM_ALGO_ORDERS",
        limit = 5
    )
    filter <- binance_filter(params$filterType, params)

    expect_equal(format(filter), "MAX_NUM_ALGO_ORDERS(5)")

    order <- structure(list(), class = "TRAILING_STOP_MARKET")
    expect_true(usdm_filter_check(filter, order, context))

    params <- data.table(
        filterType = "MAX_NUM_ALGO_ORDERS",
        limit = 4
    )
    filter <- binance_filter(params$filterType, params)
    expect_false(usdm_filter_check(filter, order, context))
})

test_that("usdm_filter_check.PERCENT_PRICE", {
    params <- data.table(
        filterType = "PERCENT_PRICE",
        multiplierDown = 0.5,
        multiplierUp = 2,
        multiplierDecimal = 4
    )
    filter <- binance_filter(params$filterType, params)

    expect_equal(format(filter), "PERCENT_PRICE(0.5, 2)")

    order <- usdm_limit_order("BTCUSDT", 8, 4)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 8.1, 4)
    expect_false(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 2, 4, side = "SELL")
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 1.9, 4, side = "SELL")
    expect_false(usdm_filter_check(filter, order, context))
})

test_that("usdm_filter_check.MIN_NOTIONAL", {
    params <- data.table(
        filterType = "MIN_NOTIONAL",
        notional = 10
    )
    filter <- binance_filter(params$filterType, params)

    expect_equal(format(filter), "MIN_NOTIONAL(10)")

    order <- usdm_limit_order("BTCUSDT", 2, 5)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 5, 2)
    expect_true(usdm_filter_check(filter, order, context))

    order <- usdm_limit_order("BTCUSDT", 4, 2)
    expect_false(usdm_filter_check(filter, order, context))
})
