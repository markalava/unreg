context("Test table functions")

test_that("table functions return tables of the correct dimension", {
    expect_equal(dim(M49_table()),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))
    expect_equal(dim(SDG_table()),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))

    expect_equal(dim(country_table()),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 2))
    expect_equal(dim(country_table(codes = FALSE)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 1))
    expect_equal(dim(country_table(names = FALSE)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 1))
})


test_that("location table does not have duplicate columns after merge", {
    expect_false(any(grepl("\\.[xy]$", colnames(reg_table()))))
    })
