context("Test string functions")

test_that("'sub_sdg_names' works", {
    expect_equal(sub_sdg_names(c("Australia/New Zealand", "World")),
                 c("Australia and New Zealand", "World"))
    expect_equal(sub_sdg_names(c("Africa", "Oceania (excluding Australia and New Zealand)")),
                 c("Africa", "Oceania excluding Australia and New Zealand"))
    })
