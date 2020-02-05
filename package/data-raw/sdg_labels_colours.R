###
### Colours for SDG Regions
###

internal_sdg_colours_hex <-
    c("Sub-Saharan Africa" = "#DEA0FD",
      "Northern Africa and Western Asia" = "#FF7F0E",
      "Central and Southern Asia" = "#2ED9FF",
      "Eastern and South-Eastern Asia" = "#3283FE",
      "Latin America and the Caribbean" = "#FFEE33",
      "Australia/New Zealand" = "#AAF400",
      "Oceania (excluding Australia and New Zealand)" = "#1CBE4F",
      "Europe and Northern America" = "#B33E52",
      "World" ="#5A5156")

internal_sdg_colours_rgb <-
    list(`Sub-Saharan Africa` = c(222,160,253),
      `Northern Africa and Western Asia` = c(255,127,14),
      `Central and Southern Asia` = c(46,217,255),
      `Eastern and South-Eastern Asia` = c(50,131,254),
      `Latin America and the Caribbean` = c(255,238,51),
      `Australia/New Zealand` = c(170,244,0),
      `Oceania (excluding Australia and New Zealand)` = c(28,190,79),
      `Europe and Northern America` = c(179,62,82),
      `World` = c(90,81,86))

###
### SDG region name variants
###

internal_sdg_name_variants <-
    c("Australia/New Zealand" = "Australia and New Zealand",
      "Oceania (excluding Australia and New Zealand)" = "Oceania excluding Australia and New Zealand")

###-----------------------------------------------------------------------------
### * Execute

usethis::use_data(internal_sdg_colours_hex, internal_sdg_colours_rgb,
                  internal_sdg_name_variants,
                  internal = TRUE, overwrite = TRUE)
