################################################################################
###
###  DATE CREATED: 2019-09-30
###
###  AUTHOR: Mark Wheldon
###
###  PROJECT: Making Family Planning Count
###
###  DESCRIPTION:
###
###     Utility functions for running 'unreg' during development.
###     These should _not_ be required for running as an end-user.
###
###-----------------------------------------------------------------------------
###
################################################################################


###-----------------------------------------------------------------------------
### * Build and Install

clean_out <- function() {
    stopifnot(require(devtools))
    if("package:unreg" %in% search()) detach("package:unreg")
    if("unreg" %in% rownames(installed.packages())) remove.packages("unreg")
    }

build_install <- function(src_dir, git_dir) {

    clean_out()            # loads devtools
    detach("package:unreg")      #<- remove from search to install
    write_sha1_DESC(src_dir = src_dir, git_dir = git_dir)
    pkg_archive <-
        (devtools::build(pkg = src_dir, path = getwd(), binary = TRUE
                        ,args = "--no-multiarch",
                         vignettes = TRUE, manual = TRUE
                         ))
    install.packages(pkg_archive
                    ,repos = NULL, type = "binary"
                     )
    clean_sha1_DESC(src_dir = src_dir)
    return(pkg_archive)
}

###-----------------------------------------------------------------------------
### * Package Data

make_data <- function(file) {
    source(file)
    }

###-----------------------------------------------------------------------------
### * Versioning

###-----------------------------------------------------------------------------
### ** Github stuff

write_sha1_DESC <- function(src_dir, git_dir) {
    ## Create new line with SHA1
    new_sha1_line <- paste0("GitHubSHA1LastCommit: ", git2r::sha(git2r::last_commit(git_dir)))

    ## Read DESC file as lines
    DESC_con <- file(file.path(src_dir, "../DESCRIPTION"))
    DESC_lines <- readLines(DESC_con)

    ## Is there already a 'GitHubSHA1LastCommit:' line? If so, replace it. If
    ## not, add one.
    sha1_line_num <- grep("GitHubSHA1LastCommit:", DESC_lines)
    if(identical(length(sha1_line_num), 1L)) DESC_lines[sha1_line_num] <- new_sha1_line
    else DESC_lines <- rbind(DESC_lines, new_sha1_line)

    ## Write DES
    writeLines(DESC_lines, con = DESC_con)
}

clean_sha1_DESC <- function(src_dir) {
    ## Read DESC file as lines
    DESC_con <- file(file.path(src_dir, "../DESCRIPTION"))
    DESC_lines <- readLines(DESC_con)

    ## Is there already a 'GitHubSHA1LastCommit:' line? If so, replace it with 'GitHubSHA1LastCommit: -'.
    sha1_line_num <- grep("GitHubSHA1LastCommit:", DESC_lines)
    if(identical(length(sha1_line_num), 1L)) DESC_lines[sha1_line_num] <- "GitHubSHA1LastCommit: -"

    ## Write DES
    writeLines(DESC_lines, con = DESC_con)
}

###-----------------------------------------------------------------------------
### * Source Package Files

## depends_libraries <- function() {
##     invisible(lapply(list("abind",
##     "coda",
##     "dplyr",
##     "foreach",
##     "gdata",
##     "ggplot2",
##     "lattice",
##     "magrittr",
##     "MCMCpack",
##     "msm",
##     "parallel",
##     "plyr",
##     "proto",
##     "xtable",
##     "RColorBrewer",
##     "reshape",
##     "reshape2",
##     "rjags",
##     "R2jags",
##     "scales",
##     "sp",
##     "rgdal",
##     "knitr",
##     "rmarkdown",
##     "bookdown"), "library", character.only = TRUE))
##     }

## source_dir <- function(path, trace = TRUE, ...) {
##     for (nm in list.files(path, pattern = "[.][RrSsQq]$")) {
##         if(!any(nm %in% c("unreg.R", "one_country_run.R"))) {
##             if(trace) cat(nm,":")
##             source(file.path(path, nm), ...)
##             if(trace) cat("\n")
##         }
##     }
## }

## from_source <- function(path) {
##     depends_libraries()
##     source_dir(path)
##     }
