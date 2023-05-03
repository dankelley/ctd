#' Tag CTD features
#'
#' @param file character value specifying the name of a file containing CTD data.
#' This file must be in a format that is handled by [oce::read.oce()].  You may
#' not supply `dir` if `file` is supplied.
#'
#' @param dir character value specifying the name of a directory containing
#' files that hold CTD data. You may not supply `file` if `dir` is supplied.
#' NOTE: the `dir` parameter is not actually handled yet (FIXME).
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
# NOTE: we are using these (mostly with package:: syntax) in
# inst/shiny/ctdTag/app.R and not in R/*R so there is really no benefit in
# naming all the functions.  Just having a single function seems to be enough
# to make R-check be happy.
#'
#' @importFrom DBI dbClearResult dbConnect dbCreateTable dbFetch dbListTables dbReadTable dbSendQuery dbWriteTable
#'
#' @importFrom DT renderDT
#'
#' @importFrom oce numberAsPOSIXct read.oce resizableLabel swSigmaTheta swTheta
#'
#' @importFrom RSQLite dbAppendTable dbConnect dbDisconnect dbReadTable dbWriteTable SQLite
#'
#' @importFrom shiny runApp shinyOptions
#'
#' @importFrom shinyjs runjs useShinyjs
#'
#' @author Dan Kelley
#'
#' @export
ctdTag <- function(file, dir, dbname=getDatabaseName("ctdTag"), tagScheme=NULL, height=550, clickDistanceCriterion=0.02, debug=0)
{
    if (missing(file) && missing(dir))
        stop("must give 'file' or 'dir'")
    if (!missing(file) && !missing(dir))
        stop("do not give both 'file' and 'dir'")
    if (!missing(dir)) {
        # FIXME(CFR): make this work
        print(list.files(dir, pattern="cnv", full.names=TRUE))
        stop("FIXME: code more here")
    }
    if (is.null(tagScheme)) {
        labels <- c("iTop", "iTop?", "iBot", "iBot?", "WS", "WS?", "CF", "CF?")
        tagScheme <- data.frame(tagCode=seq_along(labels), tagLabel=labels)
    }
    shinyOptions(file=file,
        height=height,
        tagScheme=tagScheme,
        clickDistanceCriterion=clickDistanceCriterion,
        dbname=dbname,
        debug=debug)
    dir <- system.file("shiny", "ctdTag/app.R", package="ctd")
    if (!nchar(dir))
        stop("The app could not be located.", call.=FALSE)
    runApp(dir, display.mode="normal")
}
