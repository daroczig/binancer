test_that("Sign query", {
    key <- "foo"
    secret <- "bar"
    binance_credentials(key, secret)
    expect_equal(binance_key(), key)
    expect_equal(binance_secret(), secret)

    signed_list <- binance_sign(list(foo = "bar", z = 4), "1639755497935")
    expect_equal(signed_list$foo, "bar")
    expect_equal(signed_list$z, 4)
    expect_equal(signed_list$timestamp, "1639755497935")
    expect_equal(
        signed_list$signature,
        "b6d60f421e0481f16af34b46d1db6577786cf8688538a3f8ca76bb1bc5b90d3a"
    )
})
