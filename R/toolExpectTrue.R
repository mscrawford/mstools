#' toolExpectTrue
#'
#' tool function for status reporting. It performs a logical check
#' and returns either a message via \code{toolStatusMessage}, that
#' the test was successful or that it failed.
#'
#' @param check logical check to be run (has to be either TRUE or FALSE)
#' @param description a description of the check
#' @param falseStatus the type of status that is used when the check fails (typically "note"
#' for a simple message or "warn" for a warning).
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[madrat]{getMadratMessage}}, \code{\link{toolExpectLessDiff}},
#' \code{\link{toolStatusMessage}}, \code{\link{toolWriteMadratLog}}
#' @examples
#' toolExpectTrue(is.numeric(1), "data is numeric")
#' getMadratMessage("status")
#' @export
toolExpectTrue <- function(check, description, falseStatus = "note") {
  status <- ifelse(isTRUE(check), "ok", falseStatus)
  if (!isTRUE(check)) description <- paste0("Check failed: ", description)
  toolStatusMessage(status, description)
  invisible(isTRUE(check))
}
