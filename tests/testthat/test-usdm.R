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

    expect_equal(response, as_time(1639784297))
})
