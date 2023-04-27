#' Construct a database name.
#'
#' A database name is constructed to in the form `PREFIX_USERNAME[_SUFFIX].db`,
#' where the upper-case items are inferred from corresponding parameters.
#'
#' @param prefix character value indicating the prefix for the database
#' name.
#'
#' @param username character value for the user's login name. If not provided,
#' an attempt is made to construct this using [Sys.info()].
#'
#' @param suffix optional character value that may be included in the filename,
#' if provided.
#'
#' @return [getDatabaseName] returns a character value constructed by
#' stringing `prefix`, `username` (and possibly `suffix`) together, separated
#' by underscore characters, with `.db` being added to the end;
#' see \sQuote{Examples}.
#'
#' @examples
#' getDatabaseName() # ctdTag_kelley.db, for the author
#' getDatabaseName("foo", "bar", "01") # foo_bar_01.db
#'
#' @author Dan Kelley
#'
#' @export
getDatabaseName <- function(prefix="ctdTag", username, suffix)
{
    if (missing(username)) {
        # FIXME: not sure which of the following is best for general computers.
        # As seems to often be the case, Windows can be problematic.
        #? username <- if (.Platform$OS.type == "windows") Sys.getenv("USERNAME") else Sys.getenv("USER")
        username <- Sys.info()[["user"]]
        if (is.null(username) || 0L == nchar(username))
            username <- "unknown"
    }
    if (missing(suffix))
        file.path(paste0(prefix, "_", username, ".db"))
    else
        file.path(paste0(prefix, "_", username, "_", suffix, ".db"))
}

