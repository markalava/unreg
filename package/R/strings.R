###
### String manipulation
###

### Basics
from_to_in <- function(x, from, to) {
    for(i in seq_along(from)) {
        x[x %in% from[i]] <- to[i]
    }
    x
}
from_to_grep <- function(x, from, to, ...) {
    for(i in seq_along(from)) {
        x[grep(from[i], x, ...)] <- to[i]
    }
    x
    }


### Name substitutions
name_subs <- function(x) {
    x <- tolower(x)
    from_to_in(x, from = "lac",
               to = "Latin America and the Caribbean")
}


### Make agcode variable names
make_agcode_var_name <- function(level, family) {
    paste0("internal_", tolower(family),
       "_reg_L", level, "_country_codes")
}


### Make agcode colnames
make_agcode_col_names <- function(agcodes) {
    paste0("agcode_", agcodes, "000")
}

