test_that("Ping on USDM", {
    vcr::use_cassette("usdm_v1_ping", {
        response <- usdm_v1_ping()
    })

    expect_equal(response, "OK")
})

test_that("Time on USDM", {
    vcr::use_cassette("usdm_v1_time", {
        response <- usdm_v1_time()
    })

    expect_equal(response, as_timestamp(1639784297))
})

test_that("Exchange info on USDM", {
    vcr::use_cassette("usdm_v1_exchange_info", {
        response <- usdm_v1_exchange_info()
    })

    expect_equal(
        response$serverTime,
        as_timestamp(1639724963)
    )

    expect_equal(
        names(response$symbols),
        c("symbol", "pair", "contractType", "deliveryDate", "onboardDate",
          "status", "maintMarginPercent", "requiredMarginPercent", "baseAsset",
          "quoteAsset", "marginAsset", "pricePrecision", "quantityPrecision",
          "baseAssetPrecision", "quotePrecision", "underlyingType",
          "underlyingSubType", "settlePlan", "triggerProtect", "liquidationFee",
          "marketTakeBound", "filters", "orderTypes", "timeInForce")
    )

    expect_equal(length(response$symbols), 24)

    filters <- usdm_v1_filters("BTCUSDT", response)

    expect_equal(nrow(filters), 7)
    expect_equal(ncol(filters), 12)
    expect_true(is.character(filters$filterType))
    expect_true(is.numeric(filters$minPrice))
})

test_that("Premium index on USDM", {
    vcr::use_cassette("usdm_v1_premium_index_btcusdt", {
        response <- usdm_v1_premium_index("BTCUSDT")
    })

    expect_equal(response$symbol, "BTCUSDT")
    expect_equal(response$markPrice, 46840.63)
    expect_equal(response$indexPrice, 46849.34)
    expect_equal(response$time, as_timestamp(1639846994000))
})
