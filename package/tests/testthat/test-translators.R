context("Test translator functions")

## TODO: test that passing regions, invalid codes/names, return warnings and 'NA's.

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

test_that("'code' correctly uses the right code if multiple names across families", {
    expect_equal(suppressWarnings(code(c("Africa", "Europe"))), c(903, 908))
    expect_equal(code(c("Africa", "Europe"), family = "M49"), c(903, 908))
    expect_equal(code(c("Africa", "Europe"), family = "SDG"), c(903, 917))
})

test_that("'reg_code' and 'reg_name' return the right length results", {
    expect_equal(length(reg_code(4)), 1)
    expect_equal(length(reg_code(4, family = "SDG")), 1)
    expect_equal(length(reg_code(4, family = "WB_inc")), 1)
    expect_equal(suppressWarnings(length(reg_code(c(4, 250, 901)))), 3)
    expect_equal(suppressWarnings(length(reg_code(c(4, 250, 901), family = "SDG"))), 3)
    expect_equal(suppressWarnings(length(reg_code(c(4, 250, 901), family = "WB_inc"))), 3)

    expect_equal(length(reg_name(4)), 1)
    expect_equal(length(reg_name(4, family = "SDG")), 1)
    expect_equal(length(reg_name(4, family = "WB_inc")), 1)
    expect_equal(suppressWarnings(length(reg_name(c(4, 250, 901)))), 3)
    expect_equal(suppressWarnings(length(reg_name(c(4, 250, 901), family = "SDG"))), 3)
    expect_equal(suppressWarnings(length(reg_name(c(4, 250, 901), family = "WB_inc"))), 3)
})

test_that("'reg_code' and 'reg_name' return the right class of results", {
    expect_is(reg_code(c("france", "france", "spain")), "numeric")
    expect_is(reg_name(c(250, 724, 250)), "character")

    expect_true(is.na(reg_code(NA)))
    expect_true(is.na(reg_code(c("france", NA, "france"))[2]))
    expect_true(is.na(suppressWarnings(reg_code(c("lac", NA, "lac")))[2]))
    expect_true(is.na(reg_name(NA)))
    expect_true(is.na(reg_name(c(4, NA, 4))[2]))
})

test_that("'reg_name' returns correct answer in random cases", {
    expect_equal(reg_name("Afghanistan", family = "SDG"), "Central and Southern Asia")
    expect_equal(reg_name("Afghanistan", level = "1", family = "SDG"), "Central and Southern Asia")
    expect_equal(suppressWarnings(reg_name(c(NA, "blip", "Afghanistan", NA, "Canada", "foo"), 2, family = "SDG")),
                 c(NA, NA, "Southern Asia", NA, "Northern America", NA))
})

test_that("level 'other' is working properly", {
    expect_equal(reg_name("France", family = "SDG", level = "other"), "Europe")
    expect_equal(reg_name("Ghana", family = "WB_inc", level = "other"), "Middle-income countries")
})
