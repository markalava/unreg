test_that("list functions return correct length", {
    expect_equal(length(list_reg_names(family = "WB_inc", level = "1")), 5)
    expect_equal(length(list_reg_names(family = "Dev", level = "1")), 2)
})

test_that("list functions return 'other' level correctly", {
    expect_equal(list_reg_names(level = "other", family = "WB_inc"), "Middle-income countries")
    expect_equal(list_reg_names(level = "other", family = "SDG"), "Europe")
})

### Colours

test_that("listing region colours works", {
    expect_equal(length(list_reg_colours("SDG", "hex", add_world = FALSE)), 8)
    expect_equal(length(list_reg_colours("SDG", "rgb", add_world = FALSE)), 8)
    })

test_that("listing region colours works", {
    expect_equal(length(list_reg_colours("M49", "hex", add_world = FALSE)), 6)
    expect_equal(length(list_reg_colours("M49", "rgb", add_world = FALSE)), 6)
    })

test_that("listing region colours works", {
    expect_equal(length(list_reg_colours("Dev", "hex", add_world = FALSE)), 7)
    expect_equal(length(list_reg_colours("Dev", "rgb", add_world = FALSE)), 7)
    })

test_that("listing region colours works", {
    expect_equal(length(list_reg_colours("WB_Inc", "hex", add_world = FALSE)), 5)
    expect_equal(length(list_reg_colours("WB_Inc", "rgb", add_world = FALSE)), 5)
    })
