#' Tag CTD features
#'
#' @param file name of file containing CTD data, which must be in a format
#' handled by [oce::read.oce()].
#'
#' @param dbname optional character value specifying the name of a sqlite
#' database used to hold tagging information.  If not provided, a file name
#' is constructed with [getDatabaseName()]. 
#' If this file does not exist, it is created, and a table named
#' `taglist` is constructed in it, according to the `taglist` parameter.
#' However, if the file already exists, then its `taglist` table is
#' not altered.
#'
#' @param taglist optional [list] containing (FILL IN).
#'
#' @return either an empty string, if the procedure worked, or a description
#' of the problem, otherwise.
#'
#' @author Dan Kelley
#' @export
ctdTag <- function(file, dbname=NULL, taglist=NULL)
{
    if (missing(file))
        stop("must give 'file'")
    if (is.null(dbname))
        dbname <- "DANNY"
    if (is.null(taglist))
        taglist <- list("MLD"=1, "iTop"=2, "iTop?"=3, "iBot"=4, "iBot?"=5, "WS"=6, "WS?"=7, "CF"=8, "CF?"=9)
    print(dbname)
    print(taglist)
    dir <- system.file("shiny", "ctdTag/app.R", package="ctd")
    if (!nchar(dir))
        stop("The app could not be located.", call.=FALSE)
    print(dir)
    ""
}
