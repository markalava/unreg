################################################################################
###
###  DATE CREATED: 2019-10-02
###
###  AUTHOR: Mark Wheldon
###
###  PROJECT: Making Family Planning Count
###
###  DESCRIPTION:
###
###     Build and install 'unreg' package --- mainly for interactive
###     testing and development.
###
################################################################################

###-----------------------------------------------------------------------------
### * Setup

library(git2r)
library(devtools)
example(source, echo = FALSE)

###-----------------------------------------------------------------------------
### * Functions

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
### * Test and Install

### Make sysdata.rda
sourceDir("data-raw")

### Do all tests
devtools::test(reporter = c("summary", "stop"))

### Install
devtools::install()

## or.. if needed
## devtools::install(build_vignettes = TRUE)
