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

helpMouse <- "<p><i>Mouse</i></p>
<ul>
<li>Click near the data to choose a focus point.</li>
<li>Click far from the data to remove an existing focus point.</li>
<li>Brush to select a view for enlargement.</li>
</ul>"

helpKeyboard <- "<p><i>Keyboard</i></p>
<b>FIXME: much of this is now wrong.</b>
<ul>
<li> <i>Zoom and pan</i></p></li>
<ul>
<li> <b>i</b> zoom in near mouse</li>
<li> <b>o</b> zoom out</li>
<li> <b>O</b> (upper-case 'o') zoom all the way out</li>
<li> <b>j</b> move down in water column</li>
<li> <b>k</b> move up in water column</li>
</ul>
<li><i>Tagging</i></li>
<ul>
<li> <b>0</b> through <b>9</b> tag the focus point with given numeric code.
EG: 1=top of DD layer, 2=bottom of DD layer, 3=warm-salty peak, 4=cool-fresh peak.</li>
<li> <b>x</b> remove tag on focus point</li>
<li> <b>u</b> remove focus point</li>
</ul>
</ul>
"

overallHelp <- c(helpMouse, helpKeyboard)

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

dprint <- function(...)
    if (debug > 0) print(file=stderr(), ...)

createDatabase <- function(dbname=getDatabaseName(), tagScheme=NULL)
{
    dmsg("createDatabase()...\n")
    if (file.exists(dbname)) {
        dmsg("Using existing database \"", dbname, "\"\n")
    } else {
        dmsg("createDatabase is creating database \"", dbname, "\"\n")
        con <- dbConnect(RSQLite::SQLite(), dbname)
        dbCreateTable(con, "version", c("version"="INTEGER"))
        dbWriteTable(con, "version", data.frame(version=1L), overwrite=TRUE)
        dbCreateTable(con, "tagScheme", c("code"="INTEGER", "label"="TEXT"))
        tagSchemeDF <- data.frame(code=as.integer(tagScheme), label=names(tagScheme))
        dbWriteTable(con, "tagScheme", tagSchemeDF, overwrite=TRUE)
        dbCreateTable(con, "tags", c("file"="TEXT", level="INT", tag="INT", analyst="TEXT", analysisTime="TIMESTAMP"))
        dbDisconnect(con)
    }
}

getTags <- function(file=NULL, dbname=NULL)
{
    tags <- NULL
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
    tags
}

removeTag <- function(file=NULL, level=NULL, dbname=NULL)
{
    dmsg("removeTag(file=", file, ", level=", level, ", dbname=", dbname, "\n")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    tags <- RSQLite::dbReadTable(con, "tags")
    remove <- which(tags$file == file & tags$level == level)
    if (length(remove)) {
        dmsg("will remove ", paste(remove, collapse=" "), "-th tag\n")
        dmsg(" BEFORE levels are: ", paste(tags$level, collapse=" "), "\n")
        tags <- tags[-remove, ]
        dmsg(" AFTER  levels are: ", paste(tags$level, collapse=" "), "\n")
        RSQLite::dbWriteTable(con, "tags", tags, overwrite=TRUE)
    }
    RSQLite::dbDisconnect(con)
}

saveTag <- function(file=NULL, level=NULL, tag=NULL, analyst=NULL, dbname=NULL)
{
    # no checking on NULL; add that if we want to generalize
    df <- data.frame(file=file, level=level, tag=tag, analyst=analyst, analysisTime=Sys.time())
    #dprint(df)
    dmsg("saveTag(file=", file, ", level=", level, ", tag=", tag, ", analyst=", analyst, ", dbname=", dbname, ")")
    con <- dbConnect(RSQLite::SQLite(), dbname)
    RSQLite::dbAppendTable(con, "tags", df)
    RSQLite::dbDisconnect(con)
}

findNearestLevel <- function(x, y, usr, data, view)
{
    dmsg("findNearestLevel(x=", x, ", y=", y, ", ..., view=", view, ")\n")
    dmsg("  ", vectorShow(usr))
    dx2 <- (usr[2] - usr[1])^2
    dy2 <- (usr[4] - usr[3])^2
    dmsg("  ", vectorShow(dx2))
    dmsg("  ", vectorShow(dy2))
    if (view == "T profile") {
        d2 <- (x - data$theta)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg(sprintf("  T=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
    } else if (view == "S profile") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg(sprintf("  S=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
    } else if (view == "sigmaTheta profile") {
        d2 <- (x - data$sigmaTheta)^2/dx2 + (y - data$yProfile)^2/dy2
        nearest <- which.min(d2)
        dmsg(sprintf("  sigmaTheta=%.3f, y=%.3f -> index=%d (where y=%.1f)\n", x, y, nearest, data$yProfile[nearest]))
     } else if (view == "TS") {
        d2 <- (x - data$salinity)^2/dx2 + (y - data$theta)^2/dy2
        nearest <- which.min(d2)
        dmsg(sprintf("  S=%.3f, theta=%.3f -> index=%d (where S=%.1f theta=%.1f)\n",
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
    tag=list(cex=2, lwd=2, pch=1))

ui <- fluidPage(
    shinyjs::useShinyjs(),
    style="margin-bottom:1px; margin-top:0px; color:red; padding-top:0px; padding-bottom:0px;",
    tags$script('$(document).on("keypress", function (e) { Shiny.onInputChange("keypress", e.which); Shiny.onInputChange("keypressTrigger", Math.random()); });'),
    fluidRow(
        style="background:#e6f3ff;cursor:crosshair;col=blue;",
        column(1, actionButton("help", "Help")),
        column(1, actionButton("quit", "Quit")),
        column(3, selectInput("view", label=NULL,
                choices=c("S profile"="S profile",
                    "T profile"="T profile",
                    "sigmaTheta profile"="sigmaTheta profile",
                    "TS"="TS"),
                selected="T profile")),
        column(3, selectInput("yProfile", label=NULL,
                choices=c("pressure"="pressure", "sigma-theta"="sigmaTheta"),
                selected="pressure")),
        column(2, selectInput("plotType", label=NULL,
                choices=c("line"="l", "points"="p", "line+points"="o"),
                selected="o")),
        column(1,  actionButton("clear", "Clear brush"))),
    fluidRow(
        style="background:#e6f3ff;cursor:crosshair;color:black;margin-top:0px;",
        column(8, uiOutput("tagMsg")),
        actionButton("goDown", HTML("&darr;")),
        actionButton("goUp", HTML("&uarr;")),
        actionButton("zoomOut", HTML("-")),
        actionButton("zoomIn", HTML("+"))),
    fluidRow(
        style="background:#e6f3ff;cursor:crosshair;color:red;margin-top:0px;",
        column(8, uiOutput("tagHint"))),
    fluidRow(
        uiOutput("plotPanel")))

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
    # extract shiny options (all are required) {
    dbname <- getShinyOption("dbname", getDatabaseName())
    file <- getShinyOption("file", NULL)
    if (is.null(file))
        stop("Must use shinyOptions(file=\"NAME OF A CTD FILE\")")
    height <- getShinyOption("height", 500)
    tagScheme <- getShinyOption("tagScheme", list("iTop"=1, "iTop?"=2, "iBot"=3, "iBot?"=4, "WS"=5, "WS?"=6, "CF"=7, "CF?"=8))
    clickDistanceCriterion <- getShinyOption("clickDistanceCriterion", 0.02)
    debug <<- getShinyOption("debug", 0)
    # }
    dmsg("about to create database \"", dbname, "\"\n")
    createDatabase(dbname=dbname, tagScheme=tagScheme)
    dmsg("   ... done\n")
    values <- reactiveValues(brush=NULL)
    ctd <- oce::read.oce(file)
    data <- list(pressure=ctd@data$pressure, salinity=ctd@data$salinity, temperature=ctd@data$temperature)
    data$theta <- swTheta(ctd)
    data$yProfile <- data$pressure
    data$ylabProfile <- resizableLabel("p")
    data$sigmaTheta <- swSigmaTheta(ctd, eos="unesco")
    dmsg("about to define state\n")
    state <- reactiveValues(
        step=0L,
        file=file,
        analyst=getUserName(),
        ctd=ctd,
        data=data,
        ndata=length(data$pressure),
        level=NULL,
        usr=c(0, 1, 0, 1),
        visible=rep(TRUE, length(data$pressure)) # all points visible at the start
        )

    focusIsTagged <- function()
    {
        !is.null(state$level) && (state$level %in% getTags(state$file, dbname=dbname)$level)
    }

    observeEvent(input$clear,
        {
            values$brush <- NULL
            shinyjs::runjs("document.getElementById('plot_brush').remove()")
        })

    observeEvent(input$help,
        {
            showModal(modalDialog(title=NULL,
                    size="xl", HTML(overallHelp), easyClose=TRUE))
        })

    observeEvent(input$quit,
        {
            stopApp()
        })

    observeEvent(input$goUp,
        {
            dmsg("responding to 'goUp'\n")
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
            limits <- visibleToLimits(state$visible)
            span <- max(diff(limits) / 4, 10)
            limits <- limitsTrim(limits + c(-span, span), state$ndata)
            state$visible <- limitsToVisible(limits, state$ndata)
        })

    observeEvent(input$click,
        {
            closest <- findNearestLevel(input$click$x, input$click$y, state$usr, state$data, input$view)
            dmsg(sprintf("click yielded nearest=%d, relativeDistance=%.1f%%\n", closest$level, 100*closest$relativeDistance))
            state$level <- if (closest$relativeDistance < clickDistanceCriterion) closest$nearest else NULL
        })

    # Define visibility based on the pressure limits of brushed region
    observeEvent(input$brush,
        {
            #dmsg(vectorShow(input$brush$xmin))
            #dmsg(vectorShow(input$brush$xmax))
            #dmsg(vectorShow(input$brush$ymin))
            #dmsg(vectorShow(input$brush$ymax))
            #dmsg(vectorShow(input$view))
            if (grepl("profile", input$view)) {
                dmsg("brushed on a profile (ignoring x extend of brush)\n")
                visible <- with(input$brush, ymin <= data$yProfile & data$yProfile <= ymax)
            } else if (input$view == "TS") {
                dmsg("brushed on a TS diagram\n")
                x <- state$data$salinity
                y <- state$data$theta
                # Find first and last pressures spanning box, so TS extrema are retained.
                nvisible <- length(x)
                visible <- with(input$brush, xmin <= x & x <= xmax & ymin <= y & y <= ymax)
                first <- which(visible)[1]
                last <- length(visible) + 1 - which(rev(visible))[1]
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
                dmsg(sprintf("first=%d last=%d\n", first, last))
                visible <- rep(FALSE, nvisible)
                visible[first:last] <- TRUE
            }
            #print(input$brush, file=stderr())
            sumvisible <- sum(state$visible)
            sumvisiblenew <- sum(visible)
            if (sumvisiblenew > 0.1 * sumvisible) {
                state$visible <<- visible
            } else {
                dmsg(" brush region is too small (", round(100*sumvisiblenew/sumvisible, 1), "%%) to obey")
            }
        })

    observeEvent(input$keypressTrigger,
        {
            key <- intToUtf8(input$keypress)
            #dmsg(key, "\n")
            if (key %in% as.character(0:9)) {
                if (is.null(state$level)) {
                    showNotification("No focus points")
                } else {
                    dmsg("responding to '", key, "' click for tagging\n")
                    if (state$visible[state$level]) {
                        #dmsg("  visible. should tag at level ", state$level, "\n")
                        #dmsg("  analystName=\"", state$analystName, "\"\n")
                        #dmsg("  file=\"", state$file, "\"\n")
                        saveTag(file=state$file, level=state$level, tag=as.integer(key),
                            analyst=state$analyst, dbname=dbname)
                        state$step <<- state$step + 1 # other shiny elements notice this
                    } else {
                        showNotification("No focus points in current view")
                    }
                }
            } else if (key == "i") {
                if (!is.null(input$hover$x)) {
                    dmsg("responding to 'i' click for zooming in\n")
                    nearestLevel <- findNearestLevel(input$hover$x, input$hover$y, state$usr, state$data, input$view)
                    span <- sum(state$visible)
                    if (span > default$focus$minimumSpan) {
                        span <- span / 4
                        limits <- limitsTrim(nearestLevel + c(-span/2, span/2), state$ndata)
                        state$visible <- limitsToVisible(limits, state$ndata)
                    }
                }
            } else if (key == "o") {
                dmsg("responding to 'o' click for zooming out\n")
                limits <- visibleToLimits(state$visible)
                span <- diff(limits)
                limits <- limitsTrim(limits + c(-span, span), state$ndata)
                state$visible <- limitsToVisible(limits, state$ndata)
            } else if (key == "O") {
                dmsg("responding to 'O' click to return to full-scale\n")
                state$visible <- rep(TRUE, state$ndata)
            } else if (key == "j") {
                dmsg("responding to 'j' click for moving down in water column\n")
                if (!tail(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits + (1/4)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "k") {
                dmsg("responding to 'k' click for moving up in water column\n")
                if (!head(state$visible, 1)) {
                    limits <- visibleToLimits(state$visible)
                    span <- diff(limits)
                    limits <- limitsTrim(limits - (1/4)*span, state$ndata)
                    state$visible <- limitsToVisible(limits, state$ndata)
                }
            } else if (key == "x") {
                dmsg("responding to 'x' to remove tag if focussed\n")
                if (focusIsTagged()) {
                    removeTag(file=state$file, level=state$level, dbname=getDatabaseName())
                    state$step <<- state$step + 1 # other shiny elements notice this
                }
            } else if (key == "u") {
                dmsg("responding to 'u' to remove tag if focussed\n")
                state$level <<- NULL
            }
        })

    observeEvent(input$yProfile, {
        #dmsg("observed input$yProfile=\"", input$yProfile, "\"\n")
        if (input$yProfile == "pressure") {
            data$yProfile <<- data$pressure
            data$ylabProfile <<- resizableLabel("p")
        } else {
            data$yProfile <<- data$sigmaTheta
            data$ylabProfile <<- expression(sigma[theta]* " ["* kg/m^3*"]")
        }
    })

    #output$levelMsg <- renderText(
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
            tagMsg <- if (length(tags$tag) > 0L) pluralize(length(tags$tag), "tag") else "no tags yet"
            focusMsg <- if (focusIsTagged()) paste0("| focus, at level ", state$level, ", is tagged") else ""
            pvisible <- data$pressure[state$visible]
            viewMsg <- sprintf("%.1f to %.1f dbar", min(pvisible), max(pvisible))
            paste0(file, " | ", getDatabaseName(), " | ", tagMsg, " ", focusMsg, " | ", viewMsg)
        })

    output$tagHint <- renderText(
        {
            if (!is.null(state$level))
                paste("Possible tags: ", paste(tagScheme, names(tagScheme), collapse=" ", sep="="))
            else
                "Click the mouse near a point to tag it (undo by clicking far from a point)"
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
        state$step # cause a shiny update
        input$yProfile # cause a shiny update
        if (input$view == "T profile") {
            par(mar=c(1, 3.3, 3, 1.5), mgp=c(1.9, 0.5, 0))
            x <- state$data$theta[state$visible]
            y <- data$yProfile[state$visible]
            plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                cex=default$Tprofile$cex, col=default$Tprofile$col, lwd=default$Tprofile$lwd, pch=default$Tprofile$pch,
                axes=FALSE, xlab="", ylab="")
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$theta[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$theta[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(data$ylab, side=2, line=1.5)
            mtext(resizableLabel("theta"), side=3, line=1.5)
            box()
        } else if (input$view == "S profile") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            y <- state$data$yProfile[state$visible]
            plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                cex=default$Sprofile$cex, col=default$Sprofile$col, lwd=default$Sprofile$lwd, pch=default$Sprofile$pch,
                axes=FALSE, xlab="", ylab="")
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                dmsg("S profile... ", vectorShow(state$level))
                with(default$focus,
                    points(state$data$salinity[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$salinity[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(data$ylab, side=2, line=1.5)
            mtext(resizableLabel("S"), side=3, line=1.5)
            box()
        } else if (input$view == "sigmaTheta profile") {
            par(mar=c(1, 3.3, 3, 1), mgp=c(1.9, 0.5, 0))
            dmsg("doing sigmaTheta profile")
            x <- state$data$sigmaTheta[state$visible]
            y <- data$yProfile[state$visible]
            plot(x, y, ylim=rev(range(y)), yaxs="i", type=input$plotType,
                cex=default$Tprofile$cex, col=default$Tprofile$col, lwd=default$Tprofile$lwd, pch=default$Tprofile$pch,
                axes=FALSE, xlab="", ylab="")
            state$usr <<- par("usr")
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$sigmaTheta[state$level], state$data$yProfile[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$sigmaTheta[tags$level], state$data$yProfile[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
            axis(side=2)
            axis(side=3)
            mtext(data$ylab, side=2, line=1.5)
            mtext(resizableLabel("sigmaTheta"), side=3, line=1.5)
            box()
        } else if (input$view == "TS") {
            par(mar=c(1, 3, 3, 1), mgp=c(1.9, 0.5, 0))
            x <- state$data$salinity[state$visible]
            y <- state$data$temperature[state$visible]
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
            if (!is.null(state$level)) {
                with(default$focus,
                    points(state$data$salinity[state$level], state$data$theta[state$level],
                        cex=cex, col=col, lwd=lwd, pch=pch))
            }
            tags <- getTags(state$file, dbname=dbname)
            if (length(tags$tag) > 0) {
                with(default$tag,
                    points(state$data$salinity[tags$level], state$data$theta[tags$level],
                        cex=cex, pch=pch, lwd=lwd, col=1+tags$tag))
            }
        } else {
            plot(0:1, 0:1, xlab="", ylab="", axes=FALSE, type="n")
            text(0.5, 0.5, paste("ERROR: plot type", input$view, "not handled yet"))
        }
    }, height=height, pointsize=14)
}
shinyApp(ui, server)
