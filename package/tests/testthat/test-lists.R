context("Test list functions")

test_that("list functions return correct length", {
    expect_equal(length(list_reg_names(family = "WB_inc", level = "1")), 5)
    expect_equal(length(list_reg_names(family = "Dev", level = "1")), 2)
})

test_that("list functions return 'other' level correctly", {
    expect_equal(list_reg_names(level = "other", family = "WB_inc"), "Middle-income countries")
    expect_equal(list_reg_names(level = "other", family = "SDG"), "Europe")
})

test_that("listing SDG colours works", {
    expect_equal(length(list_sdg_colours("hex")), 9)
    expect_equal(length(list_sdg_colours("rgb")), 9)
    })
