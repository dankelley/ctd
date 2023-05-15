#' Get a table from an SQLite database
#'
#' @param table character value naming the table of interest.
#'
#' @param dbname character value naming the database file.
#'
#' @author Dan Kelley
#' @export
getTableFromDatabase <- function(table, dbname)
{
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    rval <- DBI::dbReadTable(con, table)
    DBI::dbDisconnect(con)
    rval
}


#' Construct a database name.
#'
#' A database name is constructed to in the form `PREFIX_USERNAME[_SUFFIX].db`,
#' where the upper-case items are inferred from corresponding parameters.
#'
#' @param prefix character value indicating the prefix for the database
#' name.  Often, this will be the name of a project, or application.
#'
#' @param username character value for the user's login name. If not provided,
#' an attempt is made to construct this using [Sys.info()].
#'
#' @param suffix optional character value that may be included in the filename,
#' if provided.
#'
#' @param path character value indicating the directory to contain the file. This
#' defaults to the present directory.
#'
#' @return [getDatabaseName] returns a character value constructed by
#' stringing `prefix`, `username` (and possibly `suffix`) together, separated
#' by underscore characters, with `.db` being added to the end;
#' see \sQuote{Examples}.
#'
#' @examples
#' getDatabaseName() # ~/kelley.db, for the author
#' getDatabaseName(path="~/databases") # ~/databases/kelley.db, for the author
#' getDatabaseName("foo") # ~/foo_kelley.db, for the author
#' getDatabaseName("foo", "bar") # ~/foo_bar.db
#' getDatabaseName("foo", "bar", "01") # ~/foo_bar_01.db
#'
#' @author Dan Kelley
#'
#' @export
getDatabaseName <- function(prefix, username, suffix, path="~")
{
    res <- paste0(path, "/")
    if (!missing(prefix))
        res <- paste0(res, prefix)
    if (missing(username)) {
        # FIXME: not sure which of the following is best for general computers.
        # As seems to often be the case, Windows can be problematic.
        #? username <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
        username <- Sys.info()[["user"]]
        if (is.null(username) || 0L == nchar(username))
            username <- "unknown"
    }
    res <- paste0(res, if (!missing(prefix)) "_" else "", username)
    if (!missing(suffix))
        res <- paste0(res, "_", suffix)
    res <- paste0(res, ".db")
    file.path(res)
}

#' Read a SQLite database as a list
#'
#' This may be used, for example, to study the contents of a database
#' created with [ctdTag()].  This can be handy at the commandline, e.g.
#'```
#' Rscript -e 'ctd::dbToList("~/ctdTag_kelley.db")'
#'```
#'
#' @param dbname character value specifying the database file.
#'
#' @return a [list] containing items named after the tables.
#'
#' @examples
#'\dontrun{
#' dbToList("~/ctdTag_kelley.db")
#'}
#' @author Dan Kelley
#'
#' @export
dbToList <- function(dbname)
{
    con <- DBI::dbConnect(RSQLite::SQLite(), dbname)
    tables <- DBI::dbListTables(con)
    rval <- list()
    for (table in tables)
        rval[[table]] <- DBI::dbReadTable(con, table)
    rval
}


