#' unreg: Utilities for accessing country ISO codes and UN geographic regions
#'
#' @docType package
#' @name unreg
NULL





.onAttach <- function(libname, pkgname) {
    ## START-UP MESSAGE
    desc_fields <-
        c("Package", "Title", "Version",
          "Depends", "License", "Encoding", "Maintainer", "Built")
    pkg_desc <- packageDescription(pkgname)
    if("GitHubSHA1LastCommit" %in% names(pkg_desc))
        desc_fields <- c(desc_fields, "GitHubSHA1LastCommit")
    pkg_desc <- pkg_desc[desc_fields]
    pkg_desc <- paste(names(pkg_desc), pkg_desc, sep = ": ", collapse = "\n")
    packageStartupMessage(pkg_desc)

    data("UNlocations", package = "wpp2019", verbose = TRUE, environment = environment())
}
