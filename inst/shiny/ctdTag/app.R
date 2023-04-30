# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

library(oce)
options(oceEOS="unesco") # avoid the hassle of supporting two EOSs
debug <- 1

#' @importFrom shinyjs runjs

#library(shiny)
#?library(shinyBS)
#?requireNamespace(shinyjs)
#?library(shinycssloaders)
#library(oce)
#?requireNamespace(DBI)
#?requireNamespace(RSQLite)

pluralize <- function(n=1, singular="item", plural=NULL)
{
    singular <- paste(n, singular)
    if (is.null(plural))
        plural <- paste0(singular, "s")
    if (n == 1L) singular else plural
}

msg <- function(...)
    cat(file=stderr(), ..., sep="")

dmsg <- function(...)
    if (debug > 0) cat(file=stderr(), ..., sep="")

dmsg1 <- function(...)
    if (debug > 1) cat(file=stderr(), ..., sep="")

dmsg2 <- function(...)
    if (debug > 2) cat(file=stderr(), ..., sep="")

dprint <- function(...)
    if (debug > 0) print(file=stderr(), ...)

createDatabase <- function(dbname=getDatabaseName(), tagScheme=NULL)
{
    if (file.exists(dbname)) {
        dmsg("createDatabase(): using existing database \"", dbname, "\"\n")
    } else {
        dmsg("createDatabase(): creating database \"", dbname, "\"\n")
        stop("Must provide tagScheme")
        con <- dbConnect(RSQLite::SQLite(), dbname)
        dbCreateTable(con, "version", c("major"="INTEGER", "minor"="INTEGER"))
        dbWriteTable(con, "version", data.frame(major=1L, minor=0L), overwrite=TRUE)
        dbCreateTable(con, "tagScheme", c("tagCode"="INTEGER", "tagLabel"="TEXT"))
        dbWriteTable(con, "tagScheme", tagScheme, overwrite=TRUE)
        dbCreateTable(con, "tags", c("file"="TEXT", level="INT", tagCode="INT", tagLabel="TEXT",
                analyst="TEXT", analysisTime="TIMESTAMP"))
        dbDisconnect(con)
    }
}

getTags <- function(file=NULL, dbname=NULL)
{
    tags <- NULL
    #dmsg("getTags(file=\"", file, "\", dbname=\"", dbname, "\"\n")
    if (file.exists(dbname)) {
        con <- dbConnect(SQLite(), dbname)
        if (dbExistsTable(con, "tags")) {
            tags <- dbReadTable(con, "tags")
            dbDisconnect(con)
            if (!is.null(file)) {
                tags <- tags[tags$file == file, ]
            }
        }
    }
    #dmsg(vectorShow(tags))
    tags
}

removeTag <- function(file=NULL, level=NULL, dbname=NULL)
{
    dmsg("removeTag(file=\"", file, "\", level=", level, ", dbname=\"", dbname, "\") {\n")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    remove <- which(tags$file == file & tags$level == level)
    if (length(remove)) {
        dmsg("    removing ", paste(remove, collapse=" "), "-th tag\n")
        tags <- tags[-remove, ]
        RSQLite::dbWriteTable(con, "tags", tags, overwrite=TRUE)
    } else {
        dmsg("    nothing to remove\n")
    }
    RSQLite::dbDisconnect(con)
    dmsg("} removeTag()\n")
}

saveTag <- function(file=NULL, level=NULL, tagCode=NULL, tagScheme=NULL, analyst=NULL, dbname=NULL)
{
    tagLabel <- tagScheme[which(tagCode==tagScheme$tagCode), "tagLabel"]
    dmsg("saveTag(file=", file, ", level=", level, ", tagCode=", tagCode, ", tagLabel=", tagLabel, ", analyst=", analyst, ", dbname=", dbname, ")\n")
    # no checking on NULL; add that if we want to generalize
    dmsg(vectorShow(tagLabel))
    df <- data.frame(file=file, level=level, tagCode=tagCode, tagLabel=tagLabel, analyst=analyst, analysisTime=Sys.time())
    con <- dbConnect(RSQLite::SQLite(), dbname)
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}

findNearestLevel <- function(x, y, usr, data, view)
{
    dmsg("findNearestLevel(x=", x, ", y=", y, ", ..., view=", view, ")\n")
    #dmsg("  ", vectorShow(usr))
    dx2 <- (usr[2] - usr[1])^2
    dy2 <- (usr[4] - usr[3])^2
    dmsg2("  ", vectorShow(dx2))
    dmsg2("  ", vectorShow(dy2))
    if (view == "T profile") {
        d2 <- (x - data$theta)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg1(sprintf("  T=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
    } else if (view == "S profile") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg1(sprintf("  S=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
    } else if (view == "sigmaTheta profile") {
        d2 <- (x - data$sigmaTheta)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg1(sprintf("  sigmaTheta=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
     } else if (view == "TS") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$theta)^2/dy2
        nearest <- which.min(d2)
        dmsg1(sprintf("  S=%.3f, theta=%.3f -> index=%d (where S=%.1f theta=%.1f)\n",
                x, y, nearest, data$salinity[nearest], data$theta[nearest]))
    } else {
        stop("view=\"", view, "\" is not handled yet")
    }
    relativeDistance <- sqrt(d2[nearest])
    dmsg(sprintf("  returning nearest=%d, relativeDistance=%.4f\n", nearest, relativeDistance))
    list(nearest=nearest, relativeDistance=relativeDistance)
}

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
    #limits[1] <- max(1L, as.integer(limits[1]))
    #limits[2] <- min(ndata, as.integer(limits[2]))
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
    shinyjs::useShinyjs(),
    style="margin-bottom:1px; margin-top:0px; color:red; padding-top:0px; padding-bottom:0px;",
    tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    tabsetPanel(type="tabs", id="tabselected",
        tabPanel("Analysis", value=1),
        tabPanel("Summary", value=2)),
    conditionalPanel("input.tabselected==1",
        fluidRow(
            style="background:#e6f3ff;cursor:crosshair;col=blue;",
            column(1, actionButton("quit", "Quit")),
            column(1, actionButton("help", "Help")),
            column(2, selectInput("debug", label=NULL,
                    choices=c("debug=0"=0, "debug=1"=1, "debug=2"=2),
                    selected=0)),
            column(2, selectInput("view", label=NULL,
                    choices=c("S prof."="S profile",
                        "T prof."="T profile",
                        "sigmaTheta prof."="sigmaTheta profile",
                        "TS"="TS"),
                    selected="T profile")),
            column(2, selectInput("yProfile", label=NULL,
                    choices=c("pressure"="pressure", "sigma-theta"="sigmaTheta"),
                    selected="pressure")),
            column(2, selectInput("plotType", label=NULL,
                    choices=c("type='l'"="l", "type='p'"="p", "type='o'"="o"),
                    selected="o")),
            column(1,  actionButton("clear", "Clear brush"))),
        fluidRow(
            style="background:#e6f3ff;cursor:crosshair;color:black;margin-top:0px;",
            column(9, uiOutput("tagMsg")),
            actionButton("goDown", HTML("&darr;")),
            actionButton("goUp", HTML("&uarr;")),
            actionButton("zoomOut", HTML("-")),
            actionButton("zoomIn", HTML("+"))),
        fluidRow(
            style="background:#e6f3ff;cursor:crosshair;color:red;margin-top:0px;",
            column(12, uiOutput("tagHint"))),
        fluidRow(
            uiOutput("plotPanel"))),
    conditionalPanel("input.tabselected==2",
        fluidRow(uiOutput("summary"))))

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

server <- function(input, output, session) {
    plotting <- FALSE
    # extract shiny options (all are required) {
    dbname <- getShinyOption("dbname", getDatabaseName())
    file <- getShinyOption("file", NULL)
    if (is.null(file))
        stop("Must use shinyOptions(file=\"NAME OF A CTD FILE\")")
    height <- getShinyOption("height", 500)
    tagScheme <- getShinyOption("tagScheme", NULL)
    if (is.null(tagScheme)) {
        tagLabels <- c("iTop", "iTop?", "iBot", "iBot?", "WS", "WS?", "CF", "CF?")
        tagScheme <- data.frame(tagCode=seq_along(labels), tagLabel=tagLabels)
    }
    clickDistanceCriterion <- getShinyOption("clickDistanceCriterion", 0.02)
    brushCriterion <- 0.1 # a brush must cover this fraction of plot area
    debug <<- getShinyOption("debug", 0)
    # }
    overallHelp <- function() {
        # FIXME: tailor help to state, e.g. if no focus, the only thing
        # they can do is to focus, or to zoom/pan.
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
            <p><i>Possible button actions</i></p>
            <ul>
            <li>Pulldown menus alter the view.</li>
            <li>The up/down arrow buttons pan up and down (in pressure).</li>
            <li>The '-' and '+' buttons zoom in and out with respect to the present centre.</li>
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
                <p><i>Possible button actions</i></p>
                <ul>
                <li>Pulldown menus alter the view.</li>
                <li>The up/down arrow buttons pan up and down (in pressure).</li>
                <li>The '-' and '+' buttons zoom in and out with respect to the present centre.</li>
                </ul>"
            } else {
                "<p><i>Possible pointer actions</i></p>
                <ul>
                <li>Click far from data, zoom, or pan to unselect focus point.</li>
                </ul>
                <p><i>Possible keyboard actions</i></p>
                <ul>
                <li>Type a digit to tag the focus point. (The red text lists possible digits.)</li>
                </ul>
                <p><i>Possible button actions</i></p>
                <ul>
                <li>Pulldown menus alter the view.</li>
                <li>The up/down arrow buttons pan up and down (in pressure).</li>
                <li>The '-' and '+' buttons zoom in and out with respect to the present centre.</li>
                </ul>"
            }
        }
    }
    createDatabase(dbname=dbname, tagScheme=tagScheme)
    values <- reactiveValues(brush=NULL)
    ctd <- oce::read.oce(file)
    data <- list(pressure=ctd@data$pressure, salinity=ctd@data$salinity, temperature=ctd@data$temperature)
    data$theta <- swTheta(ctd)
    data$yProfile <- data$pressure
    data$ylabProfile <- resizableLabel("p")
    data$sigmaTheta <- swSigmaTheta(ctd, eos="unesco")
    dmsg1("About to define state.\n")
    state <- reactiveValues(
        step=0L,
        file=file,
        analyst=getUserName(),
        ctd=ctd,
        data=data,
        ndata=length(data$pressure),
        focusLevel=NULL,
        usr=c(0, 1, 0, 1),
        visible=rep(TRUE, length(data$pressure)) # all points visible at the start
        )

    focusIsSet <- function()
    {
        !is.null(state$focusLevel)
    }

    focusIsTagged <- function()
    {
        focusIsSet() && (state$focusLevel %in% getTags(state$file, dbname=dbname)$level)
    }

    focusIsVisible <- function()
    {
        focusIsSet() && state$visible[state$focusLevel]
    }

    observeEvent(input$clear,
        {
            values$brush <- NULL
            shinyjs::runjs("document.getElementById('plot_brush').remove()")
        })

    observeEvent(input$quit,
        {
            stopApp()
        })

    observeEvent(input$help,
        {
            showModal(modalDialog(title=NULL,
                    size="xl", HTML(overallHelp()), easyClose=TRUE))
        })

    observeEvent(input$debug,
        {
            debug <<- max(min(2, as.integer(input$debug)), 0)
        })

    observeEvent(input$goUp,
        {
            dmsg("responding to 'goUp'\n")
            state$focusLevel <- NULL # remove focus
            if (!head(state$visible, 1)) {
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits - (1/4)*span, state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
            }
        })

    observeEvent(input$goDown,
        {
            dmsg("responding to 'goDown'\n")
            state$focusLevel <- NULL # remove focus
            if (!tail(state$visible, 1)) {
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits + (1/4)*span, state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
            }
         })

    observeEvent(input$zoomIn,
        {
            dmsg("responding to 'zoomIn'\n")
            state$focusLevel <- NULL # remove focus
            limits <- visibleToLimits(state$visible)
            dmsg(vectorShow(limits))
            span <- max(diff(limits) / 4, 10)
            dmsg(vectorShow(span))
            limits <- limitsTrim(limits + c(span, -span), state$ndata)
            state$visible <- limitsToVisible(limits, state$ndata)
        })
 
    observeEvent(input$zoomOut,
        {
            dmsg("responding to 'zoomOut'\n")
            state$focusLevel <- NULL # remove focus
            limits <- visibleToLimits(state$visible)
            span <- max(diff(limits) / 4, 10)
            limits <- limitsTrim(limits + c(-span, span), state$ndata)
            state$visible <- limitsToVisible(limits, state$ndata)
        })

    observeEvent(input$click,
        {
            closest <- findNearestLevel(input$click$x, input$click$y, state$usr, state$data, input$view)
            dmsg(sprintf("click yielded nearest=%d, relativeDistance=%.1f%%\n", closest$focusLevel, 100*closest$relativeDistance))
            state$focusLevel <- if (closest$relativeDistance < clickDistanceCriterion) closest$nearest else NULL
        })

    # Handle brushing events.  First, clear the focus.  Then set visibility
    # based on the pressure limits of brushed region.
    observeEvent(input$brush,
        {
            state$focusLevel <- NULL
            if (grepl("profile", input$view)) {
                dmsg("brushed on a profile (ignoring x extend of brush)\n")
                visible <- with(input$brush, ymin <= state$data$yProfile & state$data$yProfile <= ymax)
            } else if (input$view == "TS") {
                dmsg("brushed on a TS diagram\n")
                xBrushSpan <- input$brush$xmax - input$brush$xmin
                yBrushSpan <- input$brush$ymax - input$brush$ymin
                xFraction <- with(input$brush, (xmax-xmin) / (domain$right - domain$left))
                yFraction <- with(input$brush, (ymax-ymin) / (domain$top - domain$bottom))
                dmsg1(vectorShow(xFraction))
                dmsg1(vectorShow(yFraction))
                fraction <- sqrt(xFraction^2 + yFraction^2)
                dmsg1("  ", vectorShow(fraction))
                if (fraction < brushCriterion) {
                    dmsg1(sprintf("  ignoring brush, since it occupies only %.2f%% of plot area (criterion is %.2f%%)\n",
                            100*fraction, 100*brushCriterion))
                    return()
                }
                x <- state$data$salinity
                y <- state$data$theta
                # Find first and last pressures spanning box, so TS extrema are retained.
                npoints <- length(x)
                visible <- with(input$brush, xmin <= x & x <= xmax & ymin <= y & y <= ymax)
                first <- which(visible)[1]
                dmsg2(vectorShow(first))
                last <- length(visible) + 1 - which(rev(visible))[1]
                dmsg2(vectorShow(last))
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
                dmsg1(sprintf("  first=%d last=%d\n", first, last))
                visible <- rep(FALSE, npoints)
                visible[first:last] <- TRUE
            }
            state$visible <<- visible
        })

    # Ignore key presses if a plot is being drawn, perhaps helping with issue 3.
    # https://github.com/dankelley/ctd/issues/3
    observeEvent(input$keypressTrigger,
        {
            if (!plotting) {
                msg("keypress being handled since not plotting\n")
                dmsg1(vectorShow(input$keypress))
                key <- intToUtf8(input$keypress)
                dmsg(vectorShow(key))
                if (key %in% as.character(0:9) && focusIsVisible() && !focusIsTagged()) {
                    dmsg1("responding to '", key, "' click for tagging\n")
                    dmsg("about to saveTag()\n")
                    saveTag(file=state$file, level=state$focusLevel, tagCode=as.integer(key),
                        tagScheme=tagScheme, analyst=state$analyst, dbname=dbname)
                    dmsg1("    ... done with saveTag()\n")
                    state$step <<- state$step + 1 # other shiny elements notice this
                } else if (key == "x" && focusIsTagged()) {
                    dmsg("about to untag at level ", state$focusLevel, "\n")
                    removeTag(file=state$file, level=state$focusLevel, dbname=dbname)
                    state$step <<- state$step + 1 # other shiny elements notice this
                }
                #} else if (key == "j") {
                #    dmsg("responding to 'j' click for moving down in water column\n")
                #    if (!tail(state$visible, 1)) {
                #        limits <- visibleToLimits(state$visible)
                #        span <- diff(limits)
                #        limits <- limitsTrim(limits + (1/4)*span, state$ndata)
                #        state$visible <- limitsToVisible(limits, state$ndata)
                #    }
                #} else if (key == "k") {
                #    dmsg("responding to 'k' click for moving up in water column\n")
                #    if (!head(state$visible, 1)) {
                #        limits <- visibleToLimits(state$visible)
                #        span <- diff(limits)
                #        limits <- limitsTrim(limits - (1/4)*span, state$ndata)
                #        state$visible <- limitsToVisible(limits, state$ndata)
                #    }
                #}
            } else {
                msg("keypress ignored, since still plotting\n")
            }
        })

    observeEvent(input$yProfile, {
        dmsg("observed input$yProfile=\"", input$yProfile, "\"\n")
        if (input$yProfile == "pressure") {
            state$data$yProfile <<- data$pressure
            state$data$ylabProfile <<- resizableLabel("p")
            dmsg1("    ", vectorShow(data$yProfile))
        } else {
            state$data$yProfile <<- data$sigmaTheta
            state$data$ylabProfile <<- expression(sigma[theta]* " ["* kg/m^3*"]")
            dmsg1("    ", vectorShow(data$yProfile))
        }
    })

    #output$focusLevelMsg <- renderText(
    #    {
    #        #if (!is.null(input$hover$x)) {
    #        #    if (input$view == "T profile") {
    #        #        sprintf("T=%.3f degC, p=%.3f dbar\n", input$hover$x, input$hover$y)
    #        #    } else if (input$view == "S profile") {
    #        #        sprintf("S=%.3f, p=%.3f dbar\n", input$hover$x, input$hover$y)
    #        #    } else if (input$view == "TS") {
    #        #        sprintf("S=%.3g C, T=%.3f\n", input$hover$x, input$hover$y)
    #        #    } else if (input$view == "N(z)") {
    #        #        sprintf("N=%.3g, p=%.3f dbar\n", input$hover$x, input$hover$y)
    #        #    } else {
    #        #        "FIXME"
    #        #    }
    #        #} else {
    #        pvisible <- data$pressure[state$visible]
    #        sprintf("%.1f to %.1f dbar shown", min(pvisible), max(pvisible))
    #        #}
    #    })

    output$tagMsg <- renderText(
        {
            state$step # to cause shiny to update this
            file <- state$file
            tags <- getTags(state$file, dbname=dbname)
            tags <- tags[tags$file == file, ]
            ntags <- nrow(tags)
            tagMsg <- pluralize(nrow(tags), "tag")
            #focusMsg <- if (focusIsTagged()) paste0("| focus, at level ", state$focusLevel, ", is tagged") else ""
            pvisible <- data$pressure[state$visible]
            #viewMsg <- sprintf("%.1f to %.1f dbar", min(pvisible), max(pvisible))
            #paste0(file, " | ", getDatabaseName(), " | ", tagMsg, " ", focusMsg, " | ", viewMsg)
            paste0(file, " | ", dbname, " | ", tagMsg)
        })

    output$tagHint <- renderText(
        {
            if (!is.null(state$focusLevel)) {
                tags <- getTags(state$file, dbname=dbname)
                if (state$focusLevel %in% tags$level) {
                    focusTag <- tags[tags$level == state$focusLevel, "tagLabel"]
                    sprintf("Level %d (%.1f dbar) tagged \"%s\"; type 'x' to remove tag.",
                        state$focusLevel,
                        data$pressure[state$focusLevel],
                        focusTag)
                } else {
                    sprintf("Level %d (%.1f dbar): may tag %s.",
                        state$focusLevel,
                        data$pressure[state$focusLevel],
                        paste(tagScheme$tagCode, tagScheme$tagLabel, collapse=", ", sep=" for "))
                }
            } else {
                "Click mouse near a point to focus on it (perhaps to tag it)."
            }
        })

    output$summary <- renderUI(
        {
            # FIXME: how to render more info, e.g. dbname, present file, etc?
            con <- dbConnect(SQLite(), dbname)
            tags <- dbReadTable(con, "tags")
            dbDisconnect(con)
            # Make time readible
            t <- numberAsPOSIXct(tags$analysisTime)
            tags$analysisTime <- format(t, "%Y-%m-%d %H:%M:%S UTC")
            #renderTable(tags)
            renderDataTable(tags)
        })
 
    output$plotPanel <- renderUI(
        {
            state$step # cause a shiny update
            shinyjs::runjs(sprintf("document.getElementById('plot_brush').remove()"))
            plotOutput("plot",
                brush=brushOpts("brush", delay=2000, resetOnNew=TRUE),
                click=clickOpts("click"))
        })

    output$plot <- renderPlot({
        plotting <- TRUE
        state$step # cause a shiny update
        #??? input$yProfile # cause a shiny update
        if (input$view == "T profile") {
            par(mar=c(1, 3.3, 3, 1.5), mgp=c(1.9, 0.5, 0))
            x <- state$data$theta[state$visible]
            y <- state$data$yProfile[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
                msg("about to plot. (plotting=", plotting, "\n")
                msg("    ", vectorShow(state$visible))
                msg("    ", vectorShow(x))
                msg("    ", vectorShow(y))
                msg("    range(y) = ", paste(range(y), collapse=" "), "\n")
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=default$Tprofile$cex, col=default$Tprofile$col, lwd=default$Tprofile$lwd, pch=default$Tprofile$pch,
                    axes=FALSE, xlab="", ylab="")
                msg("OK plot\n")
                state$usr <<- par("usr")
                if (!is.null(state$focusLevel)) {
                    with(default$focus,
                        points(state$data$theta[state$focusLevel], state$data$yProfile[state$focusLevel],
                            cex=cex, col=col, lwd=lwd, pch=pch))
                }
                tags <- getTags(state$file, dbname=dbname)
                if (nrow(tags) > 0) {
                    with(default$tag,
                        points(state$data$theta[tags$level], state$data$yProfile[tags$level],
                            cex=cex, pch=pch, lwd=lwd, col=col))
                    text(state$data$theta[tags$level], state$data$yProfile[tags$level], tags$tagLabel, col=2, pos=4)
                }
                axis(side=2)
                axis(side=3)
                mtext(data$ylab, side=2, line=1.5)
                mtext(resizableLabel("theta"), side=3, line=1.5)
                box()
            }
        } else if (input$view == "S profile") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            y <- state$data$yProfile[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=default$Sprofile$cex, col=default$Sprofile$col, lwd=default$Sprofile$lwd, pch=default$Sprofile$pch,
                    axes=FALSE, xlab="", ylab="")
                state$usr <<- par("usr")
                if (!is.null(state$focusLevel)) {
                    dmsg("S profile... ", vectorShow(state$focusLevel))
                    with(default$focus,
                        points(state$data$salinity[state$focusLevel], state$data$yProfile[state$focusLevel],
                            cex=cex, col=col, lwd=lwd, pch=pch))
                }
                tags <- getTags(state$file, dbname=dbname)
                if (nrow(tags) > 0) {
                    with(default$tag,
                        points(state$data$salinity[tags$level], state$data$yProfile[tags$level],
                            cex=cex, pch=pch, lwd=lwd, col=col))
                    text(state$data$salinity[tags$level], state$data$yProfile[tags$level], tags$tagLabel, col=2, pos=4)
                }
                axis(side=2)
                axis(side=3)
                mtext(data$ylab, side=2, line=1.5)
                mtext(resizableLabel("S"), side=3, line=1.5)
                box()
            }
        } else if (input$view == "sigmaTheta profile") {
            par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
            dmsg("doing sigmaTheta profile")
            x <- state$data$sigmaTheta[state$visible]
            y <- state$data$yProfile[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
                plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                    cex=default$Tprofile$cex, col=default$Tprofile$col, lwd=default$Tprofile$lwd, pch=default$Tprofile$pch,
                    axes=FALSE, xlab="", ylab="")
                state$usr <<- par("usr")
                if (!is.null(state$focusLevel)) {
                    with(default$focus,
                        points(state$data$sigmaTheta[state$focusLevel], state$data$yProfile[state$focusLevel],
                            cex=cex, col=col, lwd=lwd, pch=pch))
                }
                tags <- getTags(state$file, dbname=dbname)
                if (nrow(tags) > 0) {
                    with(default$tag,
                        points(state$data$sigmaTheta[tags$level], state$data$yProfile[tags$level],
                            cex=cex, pch=pch, lwd=lwd, col=col))
                    text(state$data$sigmaTheta[tags$level], state$data$yProfile[tags$level], tags$tagLabel, col=2, pos=4)
                }
                axis(side=2)
                axis(side=3)
                mtext(data$ylab, side=2, line=1.5)
                mtext(resizableLabel("sigmaTheta"), side=3, line=1.5)
                box()
            }
        } else if (input$view == "TS") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            y <- state$data$temperature[state$visible]
            if (length(x) > 0L && length(y) > 0L) {
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
                tags <- getTags(state$file, dbname=dbname)
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
