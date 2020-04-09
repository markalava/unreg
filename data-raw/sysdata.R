###
### Region codes for non-M49 families.
###

###-----------------------------------------------------------------------------
### * 'country_codes' of the rows containing SDG regions

## NOTE 1: These are *not* the codes of the countries in the
## regions. They are the codes of the sdg regions themselves. In
## UNlocations, these are in the column called 'country_regions'.

## NOTE 2: SDG 'subregions' 'Europe' and 'Northern America' are not
## set up as their own level (then there would be 3 sdg
## levels). Instead, 'Europe' is included in level 'OTHER' and
## 'Northern America' is set as a level 2 region.

internal_sdg_reg_L1_country_codes <-
    c(947,                      #Sub Saharan Africa
      1833,                     #N Af and W Asia
      921,                      #Central and Southern Asia
      1832,                     #Eastern and Southeastern Asia
      1830,                     #Latin American and the Caribbean
      927,                      #Australia/New Zealand
      1835,                     #Oceania excl ANZ
      1829                      #Europe and Northern America
      )

internal_sdg_reg_L1_ag_cols <- paste0("agcode_", internal_sdg_reg_L1_country_codes, "000")

internal_sdg_reg_L2_country_codes <-
    c(910,                      #Eastern africa
      911,                      #Middle Africa
      913,                      #Southern Africa
      914,                      #Western Africa
      912,                      #Nothern Africa
      922,                      #Western Asia
      5500,                     #Central Asia
      5501,                     #Southern Asia
      906,                      #Eastern Asia
      920,                      #South Eastern Asia
      915,                      #Caribbean
      916,                      #Central America
      931,                      #South America
      928,                      #Melanesia
      954,                      #Micronesia
      957,                      #Polynesia
      923,                      #Eastern Europe
      924,                      #Northern Europe
      925,                      #Southern Europe
      926,                      #Western Europe
      918                       #Northern America
      )

internal_sdg_reg_L2_ag_cols <- paste0("agcode_", internal_sdg_reg_L2_country_codes, "000")

internal_sdg_reg_Lother_country_codes <-
    c(917                       #Europe
      )

internal_sdg_reg_Lother_ag_cols <- paste0("agcode_", internal_sdg_reg_Lother_country_codes, "000")

###-----------------------------------------------------------------------------
### * SDG region name variants

internal_sdg_name_variants <-
    c("Australia/New Zealand" = "Australia and New Zealand",
      "Oceania (excluding Australia and New Zealand)" = "Oceania excluding Australia and New Zealand")

###-----------------------------------------------------------------------------
### * 'country_codes' of the rows containing WB regions

## Need to have 'OTHER' because when making tables can only have one region code per country.

internal_wb_inc_reg_L1_country_codes <-
    c(1503,                     #high income
      1502,                     #upper middle income
      1501,                     #lower middle
      1500,                     #low income
      1518                      #no income group available
      )

internal_wb_inc_reg_L1_ag_cols <- paste0("agcode_", internal_wb_inc_reg_L1_country_codes, "000")

internal_wb_inc_reg_Lother_country_codes <-
    c(1517                     #middle income
      )

internal_wb_inc_reg_Lother_ag_cols <- paste0("agcode_", internal_wb_inc_reg_Lother_country_codes, "000")

###-----------------------------------------------------------------------------
### * 'country_codes' of the rows containing Dev regions

internal_dev_reg_L1_country_codes <-
    c(901,                      #More developed regions
      902                       #Less developed regions
      )

internal_dev_reg_L1_ag_cols <- paste0("agcode_", internal_dev_reg_L1_country_codes, "000")

internal_dev_reg_L2_country_codes <-
    c(934,                      #Less developed regions excluding least developed countries
      941                       #Least developed countries
      )

internal_dev_reg_L2_ag_cols <- paste0("agcode_", internal_dev_reg_L2_country_codes, "000")

###-----------------------------------------------------------------------------
### * Colours

### World

internal_world_colour_hex <- c(`900` = "#5A5156")
internal_world_colour_rgb <- list(`900` = c(90,81,86))

### SDG Regions

internal_sdg_reg_L1_colours_hex <-
    c(`947` = "#DEA0FD", `1833` = "#FF7F0E", `921` = "#2ED9FF", `1832` = "#3283FE",
      `1830` = "#FFEE33", `927` = "#AAF400", `1835` = "#1CBE4F", `1829` = "#B33E52")

internal_sdg_reg_L1_colours_rgb <-
    list(`947` = c(222,160,253), `1833` = c(255,127,14), `921` = c(46,217,255),
         `1832` = c(50,131,254), `1830` = c(255,238,51), `927` = c(170,244,0),
         `1835` = c(28,190,79), `1829` = c(179,62,82))

### Geographic regions

internal_m49_reg_L1_colours_hex <-
    c(`903` = "#9467BD", `935` = "#17BECF", `908` = "#FF9896",
      `904` = "#FFEE33", `905` = "#882255", `909` = "#98DF8A")

internal_m49_reg_L1_colours_rgb <-
    list(`903` = c(148,103,189), `935` = c(23,190,207), `908` = c(255,152,150),
         `904` = c(255,238,51), `905` = c(136,34,85), `909` = c(152,223,138))

### Development regions

internal_dev_reg_L1_colours_hex <-
    c(`901` = "#654522", `902` = "#778B00", `941` = "#B3823E",
      `934` = "#99600F", `948` = "#BEBE00", `1636` = "#CCAA7A",
      `1637` = "#E6D2B8")

internal_dev_reg_L1_colours_rgb <-
    list(`901` = c(101,69,34), `902` = c(119,139,0), `941` = c(179,130,62),
         `934` = c(153,96,15), `948` = c(190,190,0), `1636` = c(204,170,122),
         `1637` = c(230,210,184))

### WB income groups colours

internal_wb_inc_reg_L1_colours_hex <-
    c(`1503` = "#253494", `1517` = "#2C7FB8", `1502` = "#41B6C4",
      `1501` = "#7FCDBB", `1500` = "#C7E9B4")

internal_wb_inc_reg_L1_colours_rgb <-
    list(`1503` = c(37,52,148), `1517` = c(44,127,184), `1502` = c(65,182,196),
         `1501` = c(127,205,187), `1500` = c(199,233,180))

###-----------------------------------------------------------------------------
### * Execute

usethis::use_data(
             ## Codes
             internal_sdg_reg_L1_country_codes, internal_sdg_reg_L1_ag_cols,
             internal_sdg_reg_L2_country_codes, internal_sdg_reg_L2_ag_cols,
             internal_sdg_reg_Lother_country_codes, internal_sdg_reg_Lother_ag_cols,
             internal_sdg_name_variants,
             internal_wb_inc_reg_L1_country_codes, internal_wb_inc_reg_L1_ag_cols,
             internal_wb_inc_reg_Lother_country_codes, internal_wb_inc_reg_Lother_ag_cols,
             internal_dev_reg_L1_country_codes, internal_dev_reg_L1_ag_cols,
             internal_dev_reg_L2_country_codes,internal_dev_reg_L2_ag_cols,
             ## Colours
             internal_world_colour_hex, internal_world_colour_rgb,
             internal_sdg_reg_L1_colours_hex, internal_sdg_reg_L1_colours_rgb,
             internal_m49_reg_L1_colours_hex, internal_m49_reg_L1_colours_rgb,
             internal_dev_reg_L1_colours_hex, internal_dev_reg_L1_colours_rgb,
             internal_wb_inc_reg_L1_colours_hex, internal_wb_inc_reg_L1_colours_rgb,
             ##
             internal = TRUE, overwrite = TRUE)
