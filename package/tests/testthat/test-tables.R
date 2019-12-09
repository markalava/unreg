context("Test table functions")

test_that("table functions return tables of the correct dimension", {
    ## country_table
    expect_equal(dim(country_table()),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 2))
    expect_equal(dim(country_table(include_codes = FALSE)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 1))
    expect_equal(dim(country_table(include_names = FALSE)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 1))

    ## reg_table
    expect_error(reg_table(family = "WB_inc", level = 2),
                 "No regions for this 'level' and 'family' combination.")
    expect_equal(dim(reg_table(level = 1, family = c("M49", "SDG", "WB_inc", "Dev"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 10))
    expect_equal(dim(reg_table(level = 2, family = c("M49", "SDG", "WB_inc", "Dev"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 8))
    expect_equal(dim(reg_table(level = 1:2, family = c("M49", "SDG", "WB_inc", "Dev"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 16))
    expect_equal(dim(reg_table(level = "", family = c("M49", "SDG", "WB_inc", "Dev"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 16))
    expect_equal(dim(reg_table(family = c("M49", "SDG", "WB_inc", "Dev"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 16))
})


test_that("location table does not have duplicate columns after merge", {
    expect_false(any(grepl("\\.[xy]$", colnames(reg_table()))))
})
