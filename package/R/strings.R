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

###
### SDG region name variants
###

##' SDG region name variants
##'
##' Substitutes SDG region names for a variant.
##'
##' This currently concerns only \dQuote{Australia/New Zealand} and
##' \dQuote{Oceania}.
##'
##' @param x Character vector containing names to substitute.
##' @return Character vector after substitution.
##' @author Mark Wheldon
##' @export
sub_sdg_names <- function(x) {
    nm <- names(internal_sdg_name_variants)
    for(n in 1:length(nm)) {
        x[x == nm[n]] <- internal_sdg_name_variants[n]
    }
    return(x)
    }

