context("Test list functions")

test_that("list functions return correct length", {
    expect_equal(length(list_reg_names(family = "WB_inc", level = "1")), 5)
    expect_equal(length(list_reg_names(family = "Dev", level = "1")), 2)
    })
