#' Tag CTD features
#'
#' This function may be used to tag features in CTD data.  It works by
#' launching an R/Shiny app, providing it with information gleaned
#' from the function parameters.  It may be used to work with a single data
#' file (specified with `file`) or with all the `.cnv` files in a directory
#' (specified with `dir`).  The user interface is meant to be reasonably
#' self-explanatory.  The information provided via the user's mouse clicks
#' and keyboard actions is recorded in an SQLite database named (by default)
#' `~/ctdTag_USERNAME.db`, where `USERNAME` is the user's login name on the
#' computer.  This files *must not* be edited or modified by the user, unless
#' that user has a high degree of expertise in SQL and in the functioning
#' of this package.  Since `ctdTag()` is still in development, sensible users
#' will make frequent backups of their `~/ctdTag_USERNAME.db` file.
#'
#' @param file character value specifying the name of a file containing CTD data.
#' This file must be in a format that is handled by [oce::read.oce()].  You may
#' not supply `dir` if `file` is supplied.
#'
#' @param dir character value specifying the name of a directory containing
#' files that hold CTD data. You may not supply `file` if `dir` is supplied.
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
#' dir <- "~/data/arctic/beaufort/2012"
#' ctdTag(dir=dir)
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
#' @importFrom shinyBS bsTooltip
#'
## @importFrom shinyjs runjs useShinyjs
#'
#' @author Dan Kelley
#'
#' @export
ctdTag <- function(file, dir, dbname=getDatabaseName("ctdTag"),
    tagScheme=NULL, height=550, clickDistanceCriterion=0.02, debug=0L)
{
    if (missing(file) && missing(dir))
        stop("Must specify 'file' or 'dir'")
    if (!missing(file) && !missing(dir))
        warning("LATER ... Cannot specify both 'file' and 'dir'")
    if (is.null(tagScheme)) {
        labels <- c("iTop", "iTop?", "iBot", "iBot?", "WS", "WS?", "CF", "CF?")
        tagScheme <- data.frame(tagCode=seq_along(labels), tagLabel=labels)
    }
    shinyOptions(file=if (missing(file)) NULL else file,
        dir=if (missing(dir)) NULL else dir,
        height=height,
        tagScheme=tagScheme,
        clickDistanceCriterion=clickDistanceCriterion,
        dbname=dbname,
        debug=debug)
    appdir <- system.file("shiny", "ctdTag/app.R", package="ctd")
    if (!nchar(appdir))
        stop("The app could not be located.", call.=FALSE)
    runApp(appdir, display.mode="normal")
}
