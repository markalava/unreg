## code to prepare `SDG_reg` dataset goes here

## These are the 'country_codes' of the rows containing SDG regions

## NOTE 1: These are *not* the codes of the countries in the
## regions. They are the codes of the sdg regions themselves. In
## UNlocations, these are in the column called 'country_regions'.

## NOTE 2: SDG 'subregions' 'Europe' and 'Northern America' are not set
## up as their own level (then there would be 3 sdg levels). Instead,
## 'Europe' is not included and 'Northern America' is set as a level 2
## region.

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

usethis::use_data(internal_sdg_reg_L1_country_codes, internal_sdg_reg_L1_ag_cols,
                  internal_sdg_reg_L2_country_codes,
                  internal = TRUE, overwrite = TRUE)
