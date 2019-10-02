context("Test translator functions")

test_that("'code' and 'name' return the right length results", {
    expect_equal(length(code(c("france", "france", "spain"))), 3)
    expect_equal(length(name(c(250, 724, 250))), 3)
})

test_that("'code' and 'name' return the right class of results", {
    expect_is(code(c("france", "france", "spain")), "numeric")
    expect_is(name(c(250, 724, 250)), "character")
})
