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

### Make colour code variable names
make_colour_code_var_name <- function(family, model) {
    paste0("internal_", tolower(family),
           "_reg_L1_colours_", tolower(model))
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

##' Shorten region names
##'
##' Shorten region names to help with plotting, for example.
##'
##' @param names Character vector of names to shorten.
##' @return Character vector of shortened region names.
##' @author Mark Wheldon
##' @export
shortern_reg_names <- function(names) {
    sapply(names, function(z) {
        z <- gsub("excluding Australia and New Zealand",
                  "excluding Australia and NZ", z)
        z <- gsub("excluding", "excl.", z)
        z <- gsub("Northern", "N.", z)
        z <- gsub("Southern", "S.", z)
        z <- gsub("Western", "W.", z)
        z <- gsub("Eastern", "E.", z)
        z <- gsub(" and ", " & ", z)
        z
    })}
