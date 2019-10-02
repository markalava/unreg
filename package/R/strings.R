###
### String manipulation
###


## ## Need the 'capwords' function (from 'base')
## capwords <- function(s, strict = FALSE) {
##     cap <- function(s, cap_strict) {
##         paste(toupper(substring(s, 1, 1)),
##         {s <- substring(s, 2); if(cap_strict) tolower(s) else s},
##         sep = "", collapse = " " )
##     }
##     sapply(strsplit(s, split = " "), function(z, z_st = strict) {
##         z[!(z %in% c("the", "and", "of", "de", "d'"))] <-
##             cap(z, cap_strict = z_st)
##     }, USE.NAMES = !is.null(names(s)))
## }

from_to <- function(x, from, to) {
    for(i in seq_along(from)) {
        x[x %in% from[i]] <- to[i]
    }
    x
    }

capitalize_areas <- function(x) {
    from_to(x, from = c("WORLD", "AFRICA", "ASIA", "EUROPE",
               "LATIN AMERICA AND THE CARIBBEAN",
               "NORTHERN AMERICA", "OCEANIA"),
            to = c("World", "Africa", "Asia", "Europe",
               "Latin America and the Caribbean",
               "Northern America", "Oceania"))
}

name_subs <- function(x) {
    x <- tolower(x)
    x[grep("lac", x)] <- "Latin America and the Caribbean"
    return(x)
    }

