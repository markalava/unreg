#' unreg: Utilities for accessing country ISO codes and UN geographic regions
#'
#' @docType package
#' @name unreg
NULL


.onAttach <- function(libname, pkgname) {
    ## START-UP MESSAGE
    desc_fields <-
        c("Package", "Title", "Version",
          "Imports", "License", "Encoding", "Maintainer", "Built")
    pkg_desc <- packageDescription(pkgname)
    if("GitHubSHA1LastCommit" %in% names(pkg_desc))
        desc_fields <- c(desc_fields, "GitHubSHA1LastCommit")
    pkg_desc <- pkg_desc[desc_fields]
    pkg_desc <- paste(names(pkg_desc), pkg_desc, sep = ": ", collapse = "\n")
    packageStartupMessage(pkg_desc)
}


### Things from other packages to load into namespace

## !!! The base data file
data("UNlocations", package = "wpp2019", envir = environment())
unloc_df <- UNlocations
unloc_df$name <- as.character(unloc_df$name)
unloc_df$reg_name <- as.character(unloc_df$reg_name)
unloc_df$area_name <- as.character(unloc_df$area_name)

## Set '-1' to 'NA'
unloc_df[unloc_df == -1] <- NA
rm(UNlocations)
