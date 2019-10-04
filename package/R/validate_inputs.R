###
### Validate various inputs
###

## Checks for invalid codes. 'NA' ignored
## If any are found, issues a warning and returns logical vector indexing invalid codes in 'x'
invalid_codes <- function(x) {
    if(any(!is.na(x))) {
        not_val_x <- rep(FALSE, length(x))
        not_val_x[!is.na(x)] <- !(x[!is.na(x)] %in% unloc_df$country_code)
        if(any(not_val_x)) warning("The following are not valid codes and will be replaced with 'NA': ",
                                   paste(x[not_val_x], collapse = ", "))
        not_val_x
    } else FALSE
}

## Checks for invalid names. 'NA' ignored
## If any are found, issues a warning and returns logical vector indexing invalid codes in 'x'
invalid_names <- function(x, clean = TRUE) {
    if(any(!is.na(x))) {
        not_val_x <- rep(FALSE, length(x))
        if(clean) x[!is.na(x)] <- tolower(name_subs(x[!is.na(x)]))
        not_val_x[!is.na(x)] <- !(x[!is.na(x)] %in% tolower(unloc_df$name))
        if(any(not_val_x)) warning("The following are not valid names and will be replaced with 'NA': ",
                                   paste(x[not_val_x], collapse = ", "))
        not_val_x
    } else FALSE
}

## Checks for invalid country codes. 'NA' ignored
## If any are found, issues a warning and returns logical vector indexing invalid codes in 'x'
invalid_country_codes <- function(x) {
    if(any(!is.na(x))) {
        not_val_x <- rep(FALSE, length(x))
        not_val_x[!is.na(x)] <- !(x[!is.na(x)] %in% list_country_codes())
        if(any(not_val_x)) warning("The following are not valid country codes and will be replaced with 'NA': ",
                                   paste(x[not_val_x], collapse = ", "))
        not_val_x
    } else FALSE
}

## Checks for invalid region codes. 'NA' ignored
## If any are found, issues a warning and returns logical vector indexing invalid codes in 'x'
invalid_reg_codes <- function(x, family) {
    if(any(!is.na(x))) {
        not_val_x <- rep(FALSE, length(x))
        not_val_x[!is.na(x)] <-
            (!(x[!is.na(x)] %in% c(list_reg_codes(level = 1, family = family),
                                   list_reg_codes(level = 2, family = family))))
        if(any(not_val_x)) warning("The following are not valid region codes in family '",
                                   family,
                                   "'and will be replaced with 'NA': ",
                                   paste(x[not_val_x], collapse = ", "))
        not_val_x
    } else FALSE
}
