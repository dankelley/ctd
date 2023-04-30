#' Tag CTD features
#'
#' @param file character value specifying the name of a file containing CTD data,
#' which must be in a format handled by [oce::read.oce()].
#'
#' @param dbname optional character value specifying the name of a sqlite
#' database used to hold tagging information.  If not provided, a file name
#' is constructed with [getDatabaseName()].
#' If this file does not exist, it is created, and a table named
#' `tagScheme` is constructed in it, according to the `tagScheme` parameter.
#' However, if the file already exists, then its `tagScheme` table is
#' not altered.
#'
#' @param tagScheme optional [data.frame] containing items `tagCode` (integer) and
#' `tagLabel` (character).  If not given, the labels default to
#' `c("iTop", "iTop?", "iBot", "iBot?", "WS", "WS?", "CF", "CF?")` and
#' the codes default to 1:8.
#'
#' @param height height of the plot in pixels.
#'
#' @param clickDistanceCriterion numeric value controlling whether a mouse click
#' selects, or deselects a point.  If the ratio of the distance from the mouse
#' location to the nearest point exceeds `clickDistanceCriterion` times
#' the span of the current view, then that nearest point is selected. Otherwise,
#' any existing selected point is deselected.
#'
#' @param debug integer indicating whether to print some informative information
#' to the R console.  Use 0 for mainly silent processing, 1 for more information, etc.
#'
#' @return [ctdTag()] returns either an empty string, if the procedure worked, or a description
#' of the problem, otherwise.
#'
#' @examples
#'\dontrun{
#' library(ctd)
#' file <- "~/data/arctic/beaufort/2012/d201211_0047.cnv"
#' ctdTag(file=file)
#'}
#'
#' @importFrom shiny runApp shinyOptions
#' @importFrom oce numberAsPOSIXct read.oce
#' @importFrom DBI dbClearResult dbConnect dbCreateTable dbFetch dbListTables dbReadTable dbSendQuery dbWriteTable
#' @importFrom RSQLite SQLite
#'
#' @author Dan Kelley
#'
#' @export
ctdTag <- function(file, dbname=NULL, tagScheme=NULL, height=550, clickDistanceCriterion=0.02, debug=0)
{
    if (missing(file))
        stop("must give 'file'")
    if (is.null(dbname))
        dbname <- getDatabaseName("ctdTag")
    if (is.null(tagScheme)) {
        labels <- c("iTop", "iTop?", "iBot", "iBot?", "WS", "WS?", "CF", "CF?")
        tagScheme <- data.frame(tagCode=seq_along(labels), tagLabel=labels)
    }
    dir <- system.file("shiny", "ctdTag/app.R", package="ctd")
    if (!nchar(dir))
        stop("The app could not be located.", call.=FALSE)
    shinyOptions(file=file,
        height=height,
        tagScheme=tagScheme,
        clickDistanceCriterion=clickDistanceCriterion,
        dbname=dbname,
        debug=debug)
    runApp(dir, display.mode="normal")
}
