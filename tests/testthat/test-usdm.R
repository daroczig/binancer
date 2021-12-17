test_that("Ping on USDM", {
    vcr::use_cassette("usdm_v1_ping", {
        response <- usdm_v1_ping()
    })

    expect_equal(response, "OK")
})
