#' toolStatusMessage
#'
#' tool to trigger status messages describing the data quality at different stages
#' of processing. Messages are directly written to the log at execution but also
#' collected to be finally returned as data report.
#'
#' The function which a message is attached to is selected automatically: it is the
#' innermost function call in the call stack whose name follows madrat function
#' naming conventions (starting with \code{download}, \code{correct}, \code{convert},
#' \code{read}, \code{calc} or \code{full}, followed by an upper case letter), thereby
#' skipping engine functions such as \code{readSource} or \code{calcOutput} as well as
#' helper functions (e.g. \code{toolExpectTrue}). This ensures that the message ends up
#' in the cache file of this function and of all functions depending on it. If no such
#' function is found in the call stack, the nearest enclosing named function call is
#' being used, or \code{.GlobalEnv} if there is none.
#'
#' @param status status indicator of the messages. Currently either "ok" (check
#' succesful / quality ok), "note" (check unsuccessful but still acceptable)
#' or "warn" (check unsuccessful / undesired result).
#' @param message message to be triggered.
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[madrat]{getMadratMessage}}, \code{\link{toolExpectLessDiff}}, \code{\link{toolStatusMessage}}
#' @examples
#' toolStatusMessage("ok", "everything is ok")
#' toolStatusMessage("note", "this is not optimal but probably acceptable")
#' toolStatusMessage("warn", "this is not ok")
#' getMadratMessage("status")
#' @export

toolStatusMessage <- function(status, message) {
  symbol <- toolSubtypeSelect(status, c(ok = "\u2713", note = "!", warn = "WARNING"))
  message <- paste0("[", symbol, "] ", message)
  vcat(if (status == "warn") 0 else 1, message, show_prefix = FALSE)
  putMadratMessage("status",
                   message,
                   fname = toolStatusFunctionFromCalls(sys.calls()),
                   add = TRUE)
}

toolStatusFunctionFromCalls <- function(calls) {
  # walk the call stack from the innermost frame outwards (excluding the
  # toolStatusMessage frame itself) to identify the function to attach to
  functionalCalls <- c("lapply", "sapply", "vapply", "apply", "tapply", "rapply", "mapply",
                       "do.call", "eval", "evalq", "source", "try", "tryCatch",
                       "withCallingHandlers", "with", "local", "suppressWarnings",
                       "suppressMessages")
  engineFunctions <- c("readSource", "downloadSource", "calcOutput", "retrieveData")
  madratFunction <- NULL
  nearestFunction <- NULL
  for (i in rev(seq_len(length(calls) - 1))) {
    name <- toolCalleeNameFromCall(calls[[i]])
    if (is.null(name)) next
    if (is.null(nearestFunction) && !name %in% c(functionalCalls, engineFunctions)) {
      nearestFunction <- name
    }
    if (grepl("^(download|correct|convert|read|calc|full)[A-Z]", name) && !name %in% engineFunctions) {
      # strip potential S3 method suffix so that the key matches the madrat graph
      madratFunction <- sub("\\.[^.]*$", "", name)
      break
    }
  }
  if (!is.null(madratFunction)) {
    return(madratFunction)
  }
  if (!is.null(nearestFunction)) {
    return(nearestFunction)
  }
  return(".GlobalEnv")
}

toolCalleeNameFromCall <- function(call) {
  if (!is.call(call)) {
    return(NULL)
  }
  fun <- call[[1]]
  if (is.name(fun)) {
    return(as.character(fun))
  }
  if (is.call(fun) && length(fun) == 3 && as.character(fun[[1]]) %in% c("::", ":::")) {
    return(as.character(fun[[3]]))
  }
  return(NULL)
}
