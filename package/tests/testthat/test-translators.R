context("Test translator functions")

test_that("'code' and 'name' return the right length results", {
    expect_equal(length(code(c("france", "france", "spain"))), 3)
    expect_equal(length(name(c(250, 724, 250))), 3)
})

test_that("'code' and 'name' return the right class of results", {
    expect_is(code(c("france", "france", "spain")), "numeric")
    expect_is(name(c(250, 724, 250)), "character")

    expect_true(is.na(code(NA)))
    expect_true(is.na(code(c("france", NA, "france"))[2]))
    expect_true(is.na(code(c("lac", NA, "lac"))[2]))
    expect_true(is.na(name(NA)))
    expect_true(is.na(name(c(4, NA, 4))[2]))
})

test_that("'reg_code' and 'reg_name' return the right length results", {
    expect_equal(length(reg_code(4)), 1)
    expect_equal(length(reg_code(4, family = "SDG")), 1)
    expect_equal(length(reg_code(c(4, 250, 901))), 3)
    expect_equal(length(reg_code(c(4, 250, 901), family = "SDG")), 3)
    })
