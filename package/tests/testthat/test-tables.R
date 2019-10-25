context("Test table functions")

test_that("table functions return tables of the correct dimension", {
    ## country_table
    expect_equal(dim(country_table()),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 2))
    expect_equal(dim(country_table(codes = FALSE)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 1))
    expect_equal(dim(country_table(names = FALSE)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 1))

    ## M49_table
    expect_equal(dim(M49_table(level = 1)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(M49_table(level = 2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(M49_table(level = 1:2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))
    expect_equal(dim(M49_table(level = "")),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))

    ## SDG_table
    expect_equal(dim(agcode_table(family = "SDG", level = 1)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(agcode_table(family = "SDG", level = 2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(agcode_table(family = "SDG", level = 1:2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))
    expect_equal(dim(agcode_table(family = "SDG", level = "")),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))

    ## WB_table
    expect_equal(dim(agcode_table(family = "WB", level = 1)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(agcode_table(family = "WB", level = 2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(agcode_table(family = "WB", level = 1:2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))
    expect_equal(dim(agcode_table(family = "WB", level = "")),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))

    ## Dev_table
    expect_equal(dim(agcode_table(family = "Dev", level = 1)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(agcode_table(family = "Dev", level = 2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 4))
    expect_equal(dim(agcode_table(family = "Dev", level = 1:2)),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))
    expect_equal(dim(agcode_table(family = "Dev", level = "")),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 6))

    ## reg_table
    expect_equal(dim(reg_table(level = 1, family = c("M49", "SDG", "WB"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 8))
    expect_equal(dim(reg_table(level = 2, family = c("M49", "SDG", "WB"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 8))
    expect_equal(dim(reg_table(level = 1:2, family = c("M49", "SDG", "WB"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 14))
    expect_equal(dim(reg_table(level = "", family = c("M49", "SDG", "WB"))),
                 c(nrow(unloc_df[unloc_df$location_type == 4,]), 14))
})


test_that("location table does not have duplicate columns after merge", {
    expect_false(any(grepl("\\.[xy]$", colnames(reg_table()))))
})
