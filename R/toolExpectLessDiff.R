#' toolExpectLessDiff
#'
#' tool function for status reporting. It performs a difference check between
#' two objects and returns either a message via \code{toolStatusMessage}, that
#' the test was successful or that it failed.
#'
#' @param x object 1
#' @param y object 2 which has the same format as object 1
#' @param maxdiff allowed maximum difference per element between x and y.
#' @param description a description of the check
#' @param maxdiff2 optional additional threshold. If set it will serve as a second, critial threshold
#' which will throw a warning (instead of a simple note in case of \code{maxdiff}) if being surpassed.
#'
#' @author Jan Philipp Dietrich
#' @seealso \code{\link[madrat]{getMadratMessage}}, \code{\link{toolExpectTrue}}, \code{\link{toolStatusMessage}}
#' @examples
#' toolExpectLessDiff(1:3, 2:4, 10, "data is sufficiently close")
#' getMadratMessage("status")
#' @export
toolExpectLessDiff <- function(x, y, maxdiff, description, maxdiff2 = NULL) {
  value <- max(abs(x - y))
  descriptionFull <- paste0(description, " (maxdiff = ", format(value, digits = 2),
                            ", threshold = ", format(maxdiff, digits = 2), ")")
  toolExpectTrue(value <= maxdiff, descriptionFull)
  if (!is.null(maxdiff2)) {
    descriptionFull <- paste0(description, " (maxdiff = ", format(value, digits = 2),
                              ", critical threshold = ", format(maxdiff2, digits = 2), ")")
    toolExpectTrue(value <= maxdiff2, descriptionFull, falseStatus = "warn")
  }
  return(invisible(NULL))
}
