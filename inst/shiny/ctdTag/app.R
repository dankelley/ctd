# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(shiny)
library(shinyBS)

# Set version to be stored in the database.  If the database is older than
# this, then conversion is done first, with messages being printed even
# if debug==0.
dbVersion <- data.frame(major=1L, minor=4L) # 'minor' may not exceed 100

library(oce)
options(oceEOS="unesco") # avoid the hassle of supporting two EOSs
debug <- 1
t0 <- as.numeric(Sys.time()) # used by msg() et al.

# Utility functions

# Get a table from an SQLite database
getTableFromDatabase <- function(tableName, dbName)
{
    con <- DBI::dbConnect(RSQLite::SQLite(), dbName)
    rval <- DBI::dbReadTable(con, tableName)
    DBI::dbDisconnect(con)
    rval
}

# Convert filename to tilde notation.  Both macOS and linux
# notations are handled, e.g. the following both yield ~/A.
#     /Users/username/A
#     /home/username/A
tildeName <- function(filename)
    gsub("^/(home|Users)/[^/]+", "~", filename)

# Pin a number between limits
pin <- function(x, low, high)
    max(min(x, high), low)

pluralize <- function(n=1, singular="item", plural=NULL)
{
    singular <- paste(n, singular)
    if (is.null(plural))
        plural <- paste0(singular, "s")
    if (n == 1L) singular else plural
}

# Messages to R console (stderr() file)
dt <- function(t, t0)
    sprintf("%7.4f: ", t - t0)
msg <- function(...) {
    t <- as.numeric(Sys.time())
    cat(file=stderr(), dt(t, t0), ..., sep="")
    t0 <<- t
}
dmsg <- function(...)
    if (debug > 0) msg(...)
dmsg1 <- function(...)
    if (debug > 1) msg(...)
dmsg2 <- function(...)
    if (debug > 2) msg(...)
dprint <- function(...)
    if (debug > 0) print(file=stderr(), ...)

# Create a database, or reuse an existing one. In the latter case, update as
# appropriate (not done yet, since still at initial version).
createDatabase <- function(dbname=getDatabaseName(), tagScheme=NULL)
{
    if (file.exists(dbname)) {
        dmsg("using existing database \"", dbname, "\"\n")
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
        o <- DBI::dbReadTable(con, "version")
        versionOld <- as.numeric(sprintf("%d.%02d", o$major, o$minor))
        versionNew <- as.numeric(sprintf("%d.%02d", dbVersion$major, dbVersion$minor))
        #dmsg1("version in database: ", o$major, ".", o$minor, "\n")
        #dmsg1("most recent version: ", dbVersion$major, ".", dbVersion$minor, "\n")
        if (versionOld < versionNew) {
            msg("Database version (", o$major, ".", o$minor, ") requires updating...\n")
            if (versionOld < 1.01) { # old=1.0
                msg("  Updating database to version 1.1\n")
                msg("    Create and populate `files` table...\n")
                tags <- DBI::dbReadTable(con, "tags")
                msg("      Acquired content of `tags` table\n")
                filenames <- tildeName(sort(unique(tags$file)))
                #msg(vectorShow(filenames))
                nfilenames <- length(filenames)
                #dprint(filenames)
                # NOTE: 'fileHasTags' will get renamed in the next block.
                DBI::dbCreateTable(con, "files", c(fileId="INTEGER", fileName="TEXT", fileHasTags="INTEGER"))
                files <- data.frame(fileId=seq_len(nfilenames), fileName=filenames, fileHasTags=rep(1L, nfilenames))
                DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
                #msg("DAN 1\n");print(files,file=stderr())
                msg("      Filled `files` table with ", nfilenames, " entries\n")
                msg("      Saved `files` table in database\n")
                #msg("DEBUG: next is files table (does it have /Users)\n")
                #print(files, file=stderr())
                msg("    Update `tags` table...\n")
                tmp <- merge(files, tags, by.x="fileName", by.y="file")
                msg("      Updated `tags` table by merging with `files` table\n")
                tmp$fileName <- NULL
                tmp$fileHasTags <- NULL
                tmp$tagLabel <- NULL
                msg("      Removed redundant columns in `tags` table\n")
                DBI::dbWriteTable(con, "tags", tmp, overwrite=TRUE)
                msg("      Saved `tags` table in database\n")
            }
            if (versionOld < 1.02) { # rename files$fileHasTags as files$tagStatus
                msg("  Updating database to version 1.2\n")
                files <- DBI::dbReadTable(con, "files")
                names(files) <- gsub("fileHasTags", "tagStatus", names(files))
                DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
                #msg("DAN 2\n");print(files,file=stderr())
                msg("    Renamed `files$fileHasTags` to `files$tagStatus`\n")
            }
            if (versionOld < 1.03) { # change files$fileName to ~ notation
                msg("  Updating database to version 1.3\n")
                files <- DBI::dbReadTable(con, "files")
                tags <- DBI::dbReadTable(con, "tags")
                files$fileName <- tildeName(files$fileName)
                #print(data.frame(new=DAN, old=files$fileName))
                DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
                #msg("DAN 3\n");print(files,file=stderr())
                #msg("next is files after updating to 1.3\n")
                #print(files, file=stderr())
                msg("    Renamed `fileName` entries in `files` table from /Users/x/y to ~/y\n")
            }
            if (versionOld < 1.04) { # drop files$tagStatus
                msg("  Updating database to version 1.4\n")
                files <- DBI::dbReadTable(con, "files")
                files$tagStatus <- NULL
                DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
                #msg("DAN 4\n");print(files,file=stderr())
                #msg("next is files after updating to 1.4\n")
                #print(files, file=stderr())
                msg("    Removed `tagStatus` from `files` table\n")
            }
             DBI::dbWriteTable(con, "version", dbVersion, overwrite=TRUE)
            msg("Finished updating database\n")
        } else {
            dmsg("The database format is up-to-date\n")
        }
        DBI::dbDisconnect(con)
    } else {
        dmsg("creating database \"", dbname, "\"\n")
        if (is.null(tagScheme))
            stop("Must provide tagScheme")
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
        DBI::dbCreateTable(con, "version", c("major"="INTEGER", "minor"="INTEGER"))
        DBI::dbWriteTable(con, "version", data.frame(major=1L, minor=0L), overwrite=TRUE)
        DBI::dbCreateTable(con, "tagScheme", c(tagCode="INTEGER", tagLabel="TEXT"))
        DBI::dbWriteTable(con, "tagScheme", tagScheme, overwrite=TRUE)
        DBI::dbCreateTable(con, "files", c(fileId="INTEGER", fileName="TEXT"))
        DBI::dbCreateTable(con, "tags", c(fileId="INTEGER", level="INTEGER", tagCode="INTEGER",
                analyst="TEXT", analysisTime="TIMESTAMP"))
        DBI::dbDisconnect(con)
    }
}

getFiles <- function(dbname=NULL)
{
    dmsg1("getFiles(dbname=\"", dbname, "\", ...)\n")
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    files <- DBI::dbReadTable(con, "files")
    DBI::dbDisconnect(con)
    files
}

# Get tags for a particular file.
getTags <- function(file=NULL, dbname=NULL)
{
    rval <- NULL
    dmsg1("getTags(file=\"", file, "\", ...)\n")
    if (file.exists(dbname)) {
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
        files <- DBI::dbReadTable(con, "files")
        tags <- DBI::dbReadTable(con, "tags")
        tagScheme <- DBI::dbReadTable(con, "tagScheme")
        DBI::dbDisconnect(con)
        rval <- subset(tags, fileId==subset(files, fileName==file)$fileId)
        rval <- merge(rval, tagScheme, by="tagCode")
    }
    dmsg1("  returning ", nrow(rval), " tags\n")
    rval
}

# Remove a tag at a given level of a given file
removeTag <- function(file=NULL, level=NULL, dbname=NULL)
{
    dmsg("removeTag() at level ", level, "\n")
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    tags <- DBI::dbReadTable(con, "tags")
    files <- DBI::dbReadTable(con, "files")
    tags <- subset(tags, fileId==subset(files, fileName==file)$fileId)
    remove <- which(tags$level == level)
    if (length(remove)) {
        dmsg1("    removing ", paste(remove, collapse=" "), "-th tag\n")
        tags <- tags[-remove, ]
        DBI::dbWriteTable(con, "tags", tags, overwrite=TRUE)
    } else {
        dmsg1("    nothing to remove\n")
    }
    DBI::dbDisconnect(con)
}

# Save a tag.
addTag <- function(file=NULL, level=NULL, tagCode=NULL, tagScheme=NULL, analyst=NULL, dbname=NULL)
{
    tagLabel <- tagScheme[which(tagCode==tagScheme$tagCode), "tagLabel"]
    if (identical(length(tagLabel), 1L)) {
        #dmsg("addTag(file=", file, ", level=", level, ", tagCode=", tagCode, ", tagLabel=", tagLabel, ", analyst=", analyst, ", dbname=", dbname, ")\n")
        dmsg("saving tag ", tagCode, " at level ", level, "\n")
        # FIXME: have to look up file etc (see getTags() ...)
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
        tags <- DBI::dbReadTable(con, "tags")
        files <- DBI::dbReadTable(con, "files")
        fileId <- subset(files, fileName==file)$fileId
        # FIXME: tagLabel not needed here
        df <- data.frame(fileId=fileId, level=level, tagCode=tagCode, analyst=analyst, analysisTime=Sys.time())
        DBI::dbAppendTable(con, "tags", df)
        DBI::dbDisconnect(con)
    } else {
        showNotification(paste("tag", tagCode, "not understood"))
    }
}

# Find data level (index in e.g. state$data$pressure) closest to a given (x,y),
# typically as set by a mouse click.
findNearestLevel <- function(dblclick, data, view)
{
    dmsg("findNearestLevel() ...\n")
    dx2 <- with(dblclick$domain, right - left)^2
    dy2 <- with(dblclick$domain, top - bottom)^2
    dmsg1("  ", oce::vectorShow(dx2))
    dmsg1("  ", oce::vectorShow(dy2))
    x <- dblclick$x
    y <- dblclick$y
    if (view == "T profile") {
        d2 <- (x - data$theta)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg2(sprintf("  T=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
    } else if (view == "S profile") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg2(sprintf("  S=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
    } else if (view == "sigmaTheta profile") {
        d2 <- (x - data$sigmaTheta)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg2(sprintf("  sigmaTheta=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
     } else if (view == "TS") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$theta)^2/dy2
        nearest <- which.min(d2)
        dmsg2(sprintf("  S=%.3f, theta=%.3f -> index=%d (where S=%.1f theta=%.1f)\n",
                x, y, nearest, data$salinity[nearest], data$theta[nearest]))
    } else {
        stop("view=\"", view, "\" is not handled yet")
    }
    relativeDistance <- sqrt(d2[nearest])
    dmsg(sprintf("  mouse nearest level %d, %.3f%% from data\n", nearest, 100*relativeDistance))
    list(nearest=nearest, relativeDistance=relativeDistance)
}

# Trim an index limit to be valid for a vector of length ndata.
limitsTrim <- function(limits, ndata)
{
    limits <- as.integer(limits)
    limits[1] <- max(1L, limits[1])
    limits[2] <- min(ndata, limits[2])
    limits
}

# Create logical vector indicating whether each point is visible, based
# on the two indices stored in 'limits'.
limitsToVisible <- function(limits, ndata)
{
    visible <- rep(FALSE, ndata)
    visible[seq(limits[1], limits[2])] <- TRUE
    visible
}

visibleToLimits <- function(visible)
{
    c(which(visible)[1L], 1L + length(visible) - which(rev(visible))[1L])
}

pinVisible <- function(v, max=NULL)
{
    v[1L <= v & v <= max]
}

default <- list(
    data=list(cex=0.6, col="#333333A0", lwd=1, pch=1, type="o"),
    Tprofile=list(cex=0.7, col="#333333A0", lwd=1, pch=1),
    Sprofile=list(cex=0.7, col="#333333A0", lwd=1, pch=1),
    TS=list(cex=0.7, col=1, lwd=1, pch=1),
    highlight=list(cex=3, col="purple", lwd=4, pch=5),
    join=list(cex=1.4, col=rgb(0.8, 0, 0.8, alpha=0.5), pch=0, lwd=4.0, lwdSymbol=4.0, type="o"),
    profile=list(cex=1, col="gray", lwd=2, pch=20, type="o"),
    selected=list(cex=3, col="purple", lwd=4, pch=5),
    tagged=list(cex=1.4, col=2, lwd=2, pch=20),
    cex=1.0,
    focus=list(cex=3, col="purple", lwd=2, pch=1, minimumSpan=5L),
    tag=list(cex=2, lwd=2, pch=1, col=2))

ui <- fluidPage(
    #shinyjs::useShinyjs(),
    style="margin-bottom:1px; margin-top:0px; color:red; padding-top:0px; padding-bottom:0px;",
    tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    tabsetPanel(type="tabs", id="tabselected", selected=1,
        tabPanel("File", value=1),
        tabPanel("Analysis", value=2),
        tabPanel("Summary", value=3),
        tabPanel("Help", value=4)),
    conditionalPanel("input.tabselected==1",
        fluidRow(style="background:#e6f3ff;cursor:crosshair;col=blue;",
            column(6, uiOutput("fileSelectFocus"))),
        fluidRow(style="background:#e6f3ff;cursor:crosshair;col=blue;",
            column(6, uiOutput("fileSelect"))),
        fluidRow(column(1, actionButton("quit", "Quit")))
        ),
    conditionalPanel("input.tabselected==2",
        fluidRow(
            style="background:#e6f3ff;cursor:crosshair;col=blue;",
            column(2, selectInput("debug", label=NULL,
                    choices=c("debug=0"=0L, "debug=1"=1L, "debug=2"=2L),
                    selected=debug)),
            column(2, selectInput("view", label=NULL,
                    choices=c("S prof."="S profile",
                        "T prof."="T profile",
                        "sigmaTheta prof."="sigmaTheta profile",
                        "TS"="TS"),
                    selected="T profile")),
            conditionalPanel("input.view != 'TS'",
                column(2, selectInput("yProfile", label=NULL,
                        choices=c("pressure"="pressure", "sigma-theta"="sigmaTheta"),
                        selected="pressure"))),
            column(2, selectInput("plotType", label=NULL,
                    choices=c("type='l'"="l", "type='p'"="p", "type='o'"="o"),
                    selected="o"))),
            #column(2, uiOutput("tagControl"))),
        fluidRow(
            style="background:#e6f3ff;cursor:crosshair;color:black;margin-top:0px;",
            column(8, uiOutput("tagMsg")),
            column(4,
                actionButton("goDown", HTML("&darr;")),
                actionButton("goUp", HTML("&uarr;")),
                actionButton("zoomOut", HTML("-")), # can do e.g. img(src="/zoom_out.png")
                actionButton("fullscale", HTML("1:1")))),
        fluidRow(
            style="background:#e6f3ff;cursor:crosshair;color:red;margin-top:0px;",
            column(12, uiOutput("tagHint"))),
        fluidRow(
            uiOutput("plotPanel"))),
    conditionalPanel("input.tabselected == 3", fluidRow(uiOutput("summary"))),
    conditionalPanel("input.tabselected == 4", fluidRow(uiOutput("help"))),
    shinyBS::bsTooltip("debug",
        paste("Control how much information is displayed in the console,",
            "0 for a little, 1 for some, or 2 for a lot.")),
    shinyBS::bsTooltip("view",
        "Select the plot type."),
    shinyBS::bsTooltip("yProfile",
        "Select the quantity to show on the vertical axis."),
    shinyBS::bsTooltip("plotType",
        "Select plot style, p for points, l for lines, or o for points over lines."),
    shinyBS::bsTooltip("goDown",
        "Move view window down, closer to the ocean bottom."),
    shinyBS::bsTooltip("goUp",
        "Move view window up, closer to the surface."),
    shinyBS::bsTooltip("zoomOut",
        "Zoom the view out a little."),
    shinyBS::bsTooltip("fullscale",
        "Zoom the view all the way out, showing all the data."),
    shinyBS::bsTooltip("tagHint",
        paste("Black: file | database | tag count.",
            "<br>Red: mouse/keyboard actions."))
    )

getUserName <- function()
{
    # FIXME: maybe use Sys.info()[["user"]] ???
    res <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
    if (is.null(res) || 0L == nchar(res))
        res <- "unknown"
    res
}

getDatabaseName <- function(prefix="ctd_tag")
    file.path(paste0(prefix, "_", getUserName(), ".db"))

# Ignores a tag at level=0 (which means user indicate "nothing to tag")
countTags <- function(file, dbname)
{
    tags <- getTags(file=file, dbname=dbname)
    nrow(tags)
}


server <- function(input, output, session) {
    plotting <- FALSE
    # extract shiny options (all are required) {
    dbname <- getShinyOption("dbname", getDatabaseName())
    file <- getShinyOption("file", NULL)
    dir <- getShinyOption("dir", NULL)
    fileGiven <- !is.null(file)
    dirGiven <- !is.null(dir)
    if (dirGiven) # remove a possible trailing '/'character (for later use in constructing filenames)
        dir <- gsub("/$", "", dir)
    if (!fileGiven && !dirGiven)
        stop("Must use shinyOptions(file=) or shinyOptions(dir=)")
    # Use tilde format for filenames
    if (fileGiven) {
        dmsg("original file name \"", file, "\"\n")
        if (!is.null(file))
            file <- tildeName(file)
        dmsg("converted file name \"", file, "\"\n")
    } else {
        dmsg("file not given, using dir instead\n")
    }
    height <- getShinyOption("height", 500)
    tagScheme <- getShinyOption("tagScheme", NULL)
    if (is.null(tagScheme)) {
        tagLabels <- c("iTop", "iTop?", "iBot", "iBot?", "WS", "WS?", "CF", "CF?", "NOTAGS")
        tagScheme <- data.frame(tagCode=seq_along(labels), tagLabel=tagLabels)
    }
    clickDistanceCriterion <- getShinyOption("clickDistanceCriterion", 0.02)
    brushCriterion <- 0.05 # a brush must cover this fraction of plot area (avoids accidental brushing)
    debug <<- pin(as.integer(getShinyOption("debug", 0L)), 0L, 2L)
    updateSelectInput(session, "debug", selected=debug)
    # }
    overallHelp <- function() {
        # tailor help message to the current state (e.g. is there a focus level?)
        helpMouse <- "<p><i>Mouse</i></p>
        <ul>
        <li>Click near the data to choose a focus point, which permits tagging. Note
        that focus points are removed by clicking far from the data, or by zooming
        in/out, or by moving up/down.</li>
        </ul>"

        helpKeyboard <- "<p><i>Keyboard</i></p>
        <ul>
        <li> <b>x</b> remove tag on focus point.</li>
        <li> <b>0</b>, <b>1</b> etc (as listed) tag the focussed point.</li>
        </ul>
        "
        if (is.null(state$focusLevel)) {
            "<p><i>Possible pointer actions</i></p>
            <ul>
            <li>Click near data to focus on a level.</li>
            <li>Brush to zoom in to a specific region.</li>
            </ul>
            <p><i>Controlling the view</i></p>
            <ul>
            <li>Pulldown menus control the plot type.</li>
            <li>The up/down arrow buttons pan up and down (in pressure).</li>
            <li>The `-` button zooms out a little.</li>
            <li>The `1:1` button zooms out to a full-scale view.</li>
            </ul>"
        } else {
            if (focusIsTagged()) {
                "<p><i>Possible pointer actions</i></p>
                <ul>
                <li>Click far from data, zoom, or pan to unselect focus point.</li>
                </ul>
                <p><i>Possible keyboard actions</i></p>
                <ul>
                <li>Type 'x' to remove the tag at the focus point.</li>
                </ul>
                <p><i>Controlling the view</i></p>
                <ul>
                <li>Pulldown menus control the plot type.</li>
                <li>The up/down arrow buttons pan up and down (in pressure).</li>
                <li>The `-` button zooms out a little.</li>
                <li>The `1:1` button zooms out to a full-scale view.</li>
                </ul>"
            } else {
                "<p><i>Possible pointer actions</i></p>
                <ul>
                <li>Click far from data to unselect the focus point.</li>
                </ul>
                <p><i>Possible keyboard actions</i></p>
                <ul>
                <li>Type a digit to tag the focus point. (The red text lists possible digits.)</li>
                </ul>
                <p><i>Controlling the view</i></p>
                <ul>
                <li>Pulldown menus control the plot type.</li>
                <li>The up/down arrow buttons pan up and down (in pressure).</li>
                <li>The `-` button zooms out a little.</li>
                <li>The `1:1` button zooms out to a full-scale view.</li>
                </ul>"
            }
        }
    }

    # Save file to database and also to 'state'
    registerFile <- function(file)
    {
        msg("registerFile(\"", file, "\") ...\n")
        file <- tildeName(file)
        #msg("change file to \"", file, "\"\n")
        # Add this file to the `files` table, if it's not there already.
        con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
        files <- DBI::dbReadTable(con, "files")
        #msg("DAN 5\n");print(files,file=stderr())
        if (!(file %in% files$fileName)) {
            dmsg("  adding \"", file, "\" to the `files` database table\n")
            #dmsg("OLD `files` table\n")
            #dprint(files)
            df <- data.frame(fileId=1L+max(files$fileId), fileName=file)
            DBI::dbAppendTable(con, "files", df)
            files <- DBI::dbReadTable(con, "files")
            #dmsg("NEW `files` table\n")
            #dprint(files)
        }
        #msg("DAN 6\n");print(files,file=stderr())
        DBI::dbDisconnect(con)
        dmsg("  reading \"", file, "\"\n")
        ctd <- oce::read.oce(file)
        data <- list(pressure=ctd@data$pressure, salinity=ctd@data$salinity, temperature=ctd@data$temperature)
        dmsg("  creating data\n")
        data$theta <- oce::swTheta(ctd)
        data$yProfile <- data$pressure
        data$ylabProfile <- oce::resizableLabel("p")
        data$sigmaTheta <- oce::swSigmaTheta(ctd, eos="unesco")
        data$visible <- rep(TRUE, length(data$pressure))
        dmsg("...done\n")
        list(ctd=ctd, data=data)
    }

    focusIsVisible <- function() {
        dmsg("DAN A 1\n")
        dmsg("focusIsVisible: focusLevel=", state$focusLevel, "; returning ", state$visible[state$focusLevel], "\n")
        dmsg("DAN A 2\n")
        state$visible[state$focusLevel]
    }

    # create a new database or open an existing one
    createDatabase(dbname=dbname, tagScheme=tagScheme) # or re-use existing one
    if (!fileGiven)
        file <- list.files(dir, pattern=".cnv$", full.names=TRUE)[1]
    # Read the file.  (FIXME: add get-new-file mechanism somewhere above this spot)
    fileInfo  <- registerFile(file)

    #ctd <- oce::read.oce(file)
    #data <- list(pressure=ctd@data$pressure, salinity=ctd@data$salinity, temperature=ctd@data$temperature)
    #data$theta <- oce::swTheta(ctd)
    #data$yProfile <- data$pressure
    #data$ylabProfile <- oce::resizableLabel("p")
    #data$sigmaTheta <- oce::swSigmaTheta(ctd, eos="unesco")
    #dmsg1("About to define state.\n")
    state <- reactiveValues(
        step=0L,
        stepTag=0L, # increment with tag modification, so summary works
        file=file,
        dir=if(dirGiven) dir else NULL,
        analyst=getUserName(),
        ctd=fileInfo$ctd,
        data=fileInfo$data,
        focusLevel=NULL,
        usr=c(0, 1, 0, 1),
        visible=rep(TRUE, length(fileInfo$data$pressure)) # all points visible at the start
        )

    #canTag <- function()
    #{
    #    con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    #    files <- DBI::dbReadTable(con, "files")
    #    DBI::dbDisconnect(con)
    #    w <- which(files$fileName == state$file)
    #    dmsg(vectorShow(files[w,"tagStatus"]))
    #    if (length(w) == 1L) (files[w, "tagStatus"] != 0L) else TRUE
    #}

    focusIsSet <- function()
    {
        dmsg1("in focusIsSet()\n")
        !is.null(state$focusLevel)
    }

    focusIsTagged <- function()
    {
        dmsg1("in focusIsTagged()\n")
        focusIsSet() && (state$focusLevel %in% getTags(state$file, dbname=dbname)$level)
    }

    focusIsVisible <- function()
    {
        dmsg1("in focusIsVisible()\n")
        focusIsSet() && state$visible[state$focusLevel]
    }

    observeEvent(input$file1,
        {
            msg("observeEvent on ", vectorShow(input$file1))
            msg(vectorShow(state$dir))
            filename <- tildeName(paste0(state$dir, "/", input$file1))
            fileInfo <- registerFile(filename)
            state$ctd <<- fileInfo$ctd
            state$data <<- fileInfo$data
            state$focusLevel <<- NULL
            state$file <<- filename
        })

    #observeEvent(state$file,
    #    {
    #        msg("observeEvent on ", vectorShow(state$file))
    #    })

    #observeEvent(state$tagStatus,
    #    {
    #        msg("NEW state$tagStatus=", state$tagStatus, "\n")
    #    })

    #<> observeEvent(input$disableTagging,
    #<>     {
    #<>         msg("observing input$disableTagging\n")
    #<>         con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    #<>         files <- DBI::dbReadTable(con, "files")
    #<>         w <- which(files$fileName == file)
    #<>         if (length(w) == 0L) {
    #<>             stop("file \"", file, "\" is not present in `files` table")
    #<>         } else if (length(w) > 1L) {
    #<>             stop("file \"", file, "\" is found more than once in `files` table")
    #<>         } else {
    #<>             files[w, "tagStatus"] <- 0L
    #<>             DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
    #<>             state$tagStatus <<- 0L
    #<>             dmsg("set file[", w, ",'tagStatus'] and state$tagStatus to 0\n")
    #<>         }
    #<>         DBI::dbDisconnect(con)
    #<>     })

    #<> observeEvent(input$enableTagging,
    #<>     {
    #<>         msg("observing input$enableTagging\n")
    #<>         con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    #<>         files <- DBI::dbReadTable(con, "files")
    #<>         w <- which(files$fileName == file)
    #<>         #?dmsg1(vectorShow(w))
    #<>         if (length(w) == 0L) {
    #<>             stop("file \"", file, "\" is not present in `files` table")
    #<>         } else if (length(w) > 1L) {
    #<>             stop("file \"", file, "\" is found more than once in `files` table")
    #<>         } else {
    #<>             dmsg("changing status\n")
    #<>             files[w, "tagStatus"] <- 1L
    #<>             state$tagStatus <<- 1L
    #<>             DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
    #<>             dmsg("set file[", w, ",'tagStatus'] and state$tagStatus to 1\n")
    #<>         }
    #<>         DBI::dbDisconnect(con)
    #<>     })

    observeEvent(input$quit,
        {
            stopApp()
        })

    observeEvent(input$debug,
        {
            debug <<- max(min(2, as.integer(input$debug)), 0)
        })

    observeEvent(input$goUp,
        {
            if (!head(state$visible, 1)) { # cannot go up if already showing top level
                dmsg("responding to 'goUp'\n")
                ndata <- length(state$visible)
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits - (1/3)*span, ndata)
                state$visible <- limitsToVisible(limits, ndata)
            }
        })

    observeEvent(input$goDown,
        {
            if (!tail(state$visible, 1)) { # cannot go down if already showing bottom level
                dmsg("responding to 'goDown'\n")
                ndata <- length(state$visible)
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits + (1/3)*span, ndata)
                state$visible <- limitsToVisible(limits, ndata)
            }
         })

    observeEvent(input$fullscale,
        {
            dmsg("responding to '1:1'\n")
            state$visible <<- rep(TRUE, length(state$visible))
        })
 
    observeEvent(input$zoomOut,
        {
            dmsg("responding to 'zoomOut'\n")
            #state$focusLevel <- NULL # remove focus
            limits <- visibleToLimits(state$visible)
            ndata <- length(state$visible)
            span <- max(diff(limits) / 4, 10)
            limits <- limitsTrim(limits + c(-span, span), ndata)
            state$visible <- limitsToVisible(limits, ndata)
        })

    observeEvent(input$dblclick,
        {
            dmsg("responding to input$dblclick ...\n")
            #print(input$dblclick, file=stderr())
            closest <- findNearestLevel(input$dblclick, state$data, input$view)
            #print(closest, file=stderr())
            #dmsg(sprintf("  nearest=%d, relativeDistance=%.1f%%\n", closest$nearest, 100*closest$relativeDistance))
            state$focusLevel <<- if (closest$relativeDistance < clickDistanceCriterion) closest$nearest else NULL
            dmsg("  ... observeEvent(input$dblclick) set ", vectorShow(state$focusLevel))
        })

    # Handle brushing events.  First, clear the focus.  Then set visibility
    # based on the pressure limits of brushed region. Ignore very small brush
    # areas, which usually result from a sloppy click.  R/shiny does not make
    # it easy to distinguish between clicks that are alone, and clicks that
    # start a brushing event.  Whether the present setting work with long data
    # files is an open question -- the UI might need some alteration in the
    # brush parameters.
    observeEvent(input$brush,
        {
            dmsg("responding to input$brush ...\n")
            xBrushSpan <- input$brush$xmax - input$brush$xmin
            yBrushSpan <- input$brush$ymax - input$brush$ymin
            xFraction <- with(input$brush, (xmax-xmin) / (domain$right - domain$left))
            yFraction <- with(input$brush, (ymax-ymin) / (domain$top - domain$bottom))
            dmsg1(oce::vectorShow(xFraction))
            dmsg1(oce::vectorShow(yFraction))
            fraction <- sqrt(xFraction^2 + yFraction^2)
            dmsg1("  ", oce::vectorShow(fraction))
            if (fraction < brushCriterion) {
                dmsg1(sprintf("  ignoring brush, since it occupies only %.2f%% of plot area (criterion is %.2f%%)\n",
                        100*fraction, 100*brushCriterion))
                return()
            }
            #state$focusLevel <- NULL
            if (grepl("profile", input$view)) {
                dmsg1("  profile case: only y extent considered\n")
                visible <- with(input$brush, ymin <= state$data$yProfile & state$data$yProfile <= ymax)
            } else if (input$view == "TS") {
                dmsg1("  TS case: consider both S and T\n")
                x <- state$data$salinity
                y <- state$data$theta
                # Find first and last pressures spanning box, so TS extrema are retained.
                npoints <- length(x)
                visible <- with(input$brush, xmin <= x & x <= xmax & ymin <= y & y <= ymax)
                first <- which(visible)[1]
                dmsg2(oce::vectorShow(first))
                last <- length(visible) + 1 - which(rev(visible))[1]
                dmsg2(oce::vectorShow(last))
                if (last <= 0)
                    stop("bug: last is 0 or negative")
                if (first > last) {
                    tmp <- first
                    first <- last
                    last <- tmp
                }
                if (first <= 0)
                    stop("bug: first must be positive, but it is ", first)
                if (last <= 0)
                    stop("bug: last must be positive, but it is ", last)
                dmsg2(sprintf("  first=%d last=%d\n", first, last))
                visible <- rep(FALSE, npoints)
                visible[first:last] <- TRUE
            }
            state$visible <<- visible
            dmsg("... input$brush ended\n")
        })

    # Ignore key presses if a plot is being drawn, perhaps helping with issue 3.
    # https://github.com/dankelley/ctd/issues/3
    observeEvent(input$keypressTrigger,
        {
            if (!plotting) {
                key <- intToUtf8(input$keypress)
                msg("key=", key, "\n")
                dmsg("keypress (", key, ") is being handled, since we are not plotting (FIXME: need this?)\n")
                if (key %in% as.character(tagScheme$tagCode) && focusIsVisible() && !focusIsTagged()) {
                    dmsg("  tagging at level ", state$focusLevel, "\n")
                    addTag(file=state$file, level=state$focusLevel, tagCode=as.integer(key),
                        tagScheme=tagScheme, analyst=state$analyst, dbname=dbname)
                    state$step <<- state$step + 1 # other shiny elements notice this
                    state$stepTag <<- state$stepTag + 1 # only 'summary' notices this
                } else if (key == "x" && focusIsTagged()) {
                    dmsg("  untagging at level ", state$focusLevel, "\n")
                    removeTag(file=state$file, level=state$focusLevel, dbname=dbname)
                    state$step <<- state$step + 1 # other shiny elements notice this
                    state$stepTag <<- state$stepTag + 1 # only 'summary' notices this
                } else {
                    dmsg("  ignoring keypress (invalid key, or no focus level)\n")
                }
            } else {
                dmsg("keypress (", key, ") ignored, since still plotting\n")
            }
        })

    observeEvent(input$yProfile, {
        dmsg("observed input$yProfile=\"", input$yProfile, "\"\n")
        if (input$yProfile == "pressure") {
            state$data$yProfile <<- state$data$pressure
            state$data$ylabProfile <<- oce::resizableLabel("p")
        } else {
            state$data$yProfile <<- state$data$sigmaTheta
            state$data$ylabProfile <<- expression(sigma[theta]* " ["* kg/m^3*"]")
        }
    })

    output$fileSelectFocus <- renderUI(
        {
            msg("in output$fileSelectFocus\n")
            fluidRow(
                column(12,
                    radioButtons("fileSelectFocusChoice",
                        paste("Focus within", state$dir),
                        c("all files", "previously examined files", "unexamined files"))))
        })

    output$fileSelect <- renderUI(
        {
            dmsg("in output$fileSelect\n")
            dmsg("  input$fileSelectFocusChoice=", input$fileSelectFocusChoice, "\n")
            if (!is.null(state$dir)) {
                # The focus is empty on the first call, as the shiny interface
                # is being constructed.
                focus <- input$fileSelectFocusChoice
                if (!length(focus))
                    focus <- "all files"
                msg("  focus=\"", focus, "\"\n")
                filesInDatabase <- sort(gsub(paste0(state$dir,"/"), "", getTableFromDatabase("files", dbname)$fileName))
                filesInDir <- list.files(state$dir, pattern=".cnv$")
                choices <- if (focus == "all files")
                    filesInDir
                else if (focus == "previously examined files")
                    filesInDatabase
                else if (focus == "unexamined files")
                    filesInDir[!filesInDir %in% filesInDatabase]
                fluidRow(
                    column(12,
                        selectInput("file1", label="Choose a file", choices=choices, width="50%"))
                )
            }
        })


    # Do not show a button if the profile has tags.  If it has no tags, display
    # 'Disable Tagging' if tagStatus is -1 (which is the case for a
    # newly-opened file) or `Enable Tagging` if tagStatus is 0.  Clicking
    # on 'Disable Tagging' changes tagStatus to 0...???
    #
    # tagState = -1 Initial state for a file that has not been examined before.
    # In this state, the button says 'Disable Tagging'. If it is clicked, then
    # the button changes to read 'Enable Tagging'.
    # then
    # tagging will not be permitted.
    # that is clicked then tagStatus changes to 0, and tagging is permitted. so
    # that the button changes to say 'Enable Tagging'.
    # nothin
    # Only show the 'Do Not Tag' button if the profile has zero tags.  That is,
    # the user is forced to remove tags first.  This avoids contradictions of
    # thought and action.
    #<> output$tagControl <- renderUI(
    #<>     {
    #<>         tagStatus <- state$tagStatus # so this gets called
    #<>         #?dmsg(vectorShow(canTag()))
    #<>         #?con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    #<>         #?files <- DBI::dbReadTable(con, "files")
    #<>         #?DBI::dbDisconnect(con)
    #<>         #print(files)
    #<>         #?tagStatus <- files[files$fileName == state$file, "tagStatus"]
    #<>         msg(vectorShow(tagStatus))
    #<>         tagCount <- countTags(state$file, dbname)
    #<>         msg(vectorShow(tagCount))
    #<>         #browser()
    #<>         # Do not show either button if we have tags
    #<>         if (tagCount == 0L) {
    #<>             # -1 is the initial state for a new file.  By default, tagging
    #<>             # is allowed.
    #<>             if (tagStatus == -1L) {
    #<>                 #<> state$tagStatus <<- 0L
    #<>                 #<> files[wfile, "tagStatus"] <- 0L
    #<>                 #<> DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
    #<>                 #<> DBI::dbDisconnect(con)
    #<>                 actionButton("disableTagging", "Disable Tagging")
    #<>             } else if (tagStatus == 0) {
    #<>                 #<> state$tagStatus <<- -1L
    #<>                 #<> files[wfile, "tagStatus"] <- -1L
    #<>                 #<> DBI::dbWriteTable(con, "files", files, overwrite=TRUE)
    #<>                 #<> DBI::dbDisconnect(con)
    #<>                 #<> state$tagStatus <<- tagStatus
    #<>                 actionButton("enableTagging", "Enable Tagging")
    #<>             }
    #<>         }
    #<>     })

    output$tagMsg <- renderText(
        {
            state$stepTag # to cause shiny to update this
            tagMsg <- pluralize(countTags(state$file, dbname), "tag")
            #focusMsg <- if (focusIsTagged()) paste0("| focus, at level ", state$focusLevel, ", is tagged") else ""
            #pvisible <- data$pressure[state$visible]
            #viewMsg <- sprintf("%.1f to %.1f dbar", min(pvisible), max(pvisible))
            #paste0(file, " | ", getDatabaseName(), " | ", tagMsg, " ", focusMsg, " | ", viewMsg)
            paste0(state$file, " | ", dbname, " | ", tagMsg)
        })

    output$tagHint <- renderText(
        {
            if (!is.null(state$focusLevel)) {
                tags <- getTags(state$file, dbname=dbname)
                if (state$focusLevel %in% tags$level) {
                    focusTag <- tags[tags$level == state$focusLevel, "tagLabel"]
                    sprintf("%s %d (%.1f dbar) tagged \"%s\"; type 'x' to remove tag.",
                        if (focusIsVisible()) "Level" else "OFFSCALE level",
                        state$focusLevel,
                        data$pressure[state$focusLevel],
                        focusTag)
                } else {
                    sprintf("%s %d (%.1f dbar): may tag %s.",
                        if (focusIsVisible()) "Level" else "OFFSCALE level",
                        state$focusLevel,
                        state$data$pressure[state$focusLevel],
                        paste(tagScheme$tagCode, tagScheme$tagLabel, collapse=", ", sep=" for "))
                }
            } else {
                "Double-click mouse near a point to focus on it."
            }
        })

    output$summary <- renderUI(
        {
            state$stepTag # to cause shiny to update this
            # FIXME: how to render more info, e.g. dbname, present file, etc?
            dmsg("responding to request for a summary\n")
            con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
            files <- DBI::dbReadTable(con, "files")
            tags <- DBI::dbReadTable(con, "tags")
            tagScheme <- DBI::dbReadTable(con, "tagScheme")
            DBI::dbDisconnect(con)
            #dprint(names(tags))
            #dprint(names(files))
            tmp <- merge(merge(tags, files, by="fileId"), tagScheme, by="tagCode")
            o <- order(tmp$analysisTime, decreasing=TRUE)
            tmp <- tmp[o, ]
            # reorder and rename for clarity
            tmp <- data.frame(file=tmp$fileName, level=tmp$level, tag=tmp$tagLabel,
                analyst=tmp$analyst,
                analysisTime=format(oce::numberAsPOSIXct(tmp$analysisTime),
                    "%Y-%m-%d %H:%M:%S UTC"))
            #renderTable(tags)
            DT::renderDT(tmp)
        })

    output$help <- renderUI(
        {
            HTML(overallHelp())
        })
 
    output$plotPanel <- renderUI(
        {
            state$step # cause a shiny update
            plotOutput("plot",
                brush=brushOpts("brush", delay=2000, resetOnNew=TRUE),
                dblclick=clickOpts("dblclick"))
        })

    output$plot <- renderPlot({
        dmsg("output$plot...\n")
        dmsg(vectorShow(input$view))
        plotting <- TRUE
        state$step # cause a shiny update
        #??? input$yProfile # cause a shiny update
        if (input$view == "T profile") {
            x <- state$data$theta[state$visible]
            y <- state$data$yProfile[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
                dmsg("about to plot T profile...\n")
                par(mar=c(1, 3.3, 3, 1.5), mgp=c(1.9, 0.5, 0))
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=default$Tprofile$cex, col=default$Tprofile$col, lwd=default$Tprofile$lwd, pch=default$Tprofile$pch,
                    axes=FALSE, xlab="", ylab="")
                state$usr <<- par("usr")
                if (!is.null(state$focusLevel)) {
                    with(default$focus,
                        points(state$data$theta[state$focusLevel], state$data$yProfile[state$focusLevel],
                            cex=cex, col=col, lwd=lwd, pch=pch))
                }
                tags <- subset(getTags(state$file, dbname=dbname), level > 0L,)
                if (nrow(tags) > 0) {
                    with(default$tag,
                        points(state$data$theta[tags$level], state$data$yProfile[tags$level],
                            cex=cex, pch=pch, lwd=lwd, col=col))
                    text(state$data$theta[tags$level], state$data$yProfile[tags$level], tags$tagLabel, col=2, pos=4)
                }
                axis(side=2)
                axis(side=3)
                mtext(state$data$ylabProfile, side=2, line=1.5)
                mtext(oce::resizableLabel("theta"), side=3, line=1.5)
                box()
                dmsg("  ... finished T profile\n")
            }
        } else if (input$view == "S profile") {
            x <- state$data$salinity[state$visible]
            y <- state$data$yProfile[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
                dmsg("about to plot S profile...\n")
                par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=default$Sprofile$cex, col=default$Sprofile$col, lwd=default$Sprofile$lwd, pch=default$Sprofile$pch,
                    axes=FALSE, xlab="", ylab="")
                state$usr <<- par("usr")
                if (!is.null(state$focusLevel)) {
                    with(default$focus,
                        points(state$data$salinity[state$focusLevel], state$data$yProfile[state$focusLevel],
                            cex=cex, col=col, lwd=lwd, pch=pch))
                }
                tags <- subset(getTags(state$file, dbname=dbname), level > 0L,)
                if (nrow(tags) > 0) {
                    with(default$tag,
                        points(state$data$salinity[tags$level], state$data$yProfile[tags$level],
                            cex=cex, pch=pch, lwd=lwd, col=col))
                    text(state$data$salinity[tags$level], state$data$yProfile[tags$level], tags$tagLabel, col=2, pos=4)
                }
                axis(side=2)
                axis(side=3)
                mtext(state$data$ylabProfile, side=2, line=1.5)
                mtext(oce::resizableLabel("S"), side=3, line=1.5)
                box()
            }
        } else if (input$view == "sigmaTheta profile") {
            x <- state$data$sigmaTheta[state$visible]
            y <- state$data$yProfile[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
                dmsg("about to plot sigmaTheta profile...\n")
                par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=default$Tprofile$cex, col=default$Tprofile$col, lwd=default$Tprofile$lwd, pch=default$Tprofile$pch,
                    axes=FALSE, xlab="", ylab="")
                state$usr <<- par("usr")
                if (!is.null(state$focusLevel)) {
                    with(default$focus,
                        points(state$data$sigmaTheta[state$focusLevel], state$data$yProfile[state$focusLevel],
                            cex=cex, col=col, lwd=lwd, pch=pch))
                }
                tags <- subset(getTags(state$file, dbname=dbname), level > 0L,)
                if (nrow(tags) > 0) {
                    with(default$tag,
                        points(state$data$sigmaTheta[tags$level], state$data$yProfile[tags$level],
                            cex=cex, pch=pch, lwd=lwd, col=col))
                    text(state$data$sigmaTheta[tags$level], state$data$yProfile[tags$level], tags$tagLabel, col=2, pos=4)
                }
                axis(side=2)
                axis(side=3)
                mtext(state$data$ylabProfile, side=2, line=1.5)
                mtext(oce::resizableLabel("sigmaTheta"), side=3, line=1.5)
                box()
            }
        } else if (input$view == "TS") {
            x <- state$data$salinity[state$visible]
            y <- state$data$temperature[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
                dmsg("about to plot TS...\n")
                par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
                p <- state$data$pressure[state$visible]
                ctd <- as.ctd(x, y, p)
                # Plot empty with visible data, but then add the actual full data.
                # That way, we can see tagged points even if they are in the 4%
                # within-plot buffer zone.  (I am not using xaxs="i" etc because
                # it can put intrusions on the axis.)
                plotTS(ctd, eos="unesco", type="n")
                points(state$data$salinity, state$data$theta, type=input$plotType,
                    cex=default$TS$cex, col=default$TS$col, lwd=default$TS$lwd, pch=default$TS$pch)
                state$usr <<- par("usr")
                if (!is.null(state$focusLevel)) {
                    with(default$focus,
                        points(state$data$salinity[state$focusLevel], state$data$theta[state$focusLevel],
                            cex=cex, col=col, lwd=lwd, pch=pch))
                }
                tags <- subset(getTags(state$file, dbname=dbname), level > 0L,)
                if (nrow(tags) > 0) {
                    with(default$tag,
                        points(state$data$salinity[tags$level], state$data$theta[tags$level],
                            cex=cex, pch=pch, lwd=lwd, col=col))
                    text(state$data$salinity[tags$level], state$data$theta[tags$level], tags$tagLabel, col=2, pos=4)
                }
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
        plotting <- FALSE
    }, height=height, pointsize=14)
}
shinyApp(ui, server)
