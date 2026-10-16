#' toolIso2CellCountries
#'
#' Select country names of countries which are present on cellular level
#' @param x magpie object on iso country level
#' @param cells deprecated. This function now always uses the 67420 cells; only "lpjcell" is accepted.
#' The argument is kept for one release so that calls which still pass it keep working.
#' @param absolute switch declaring the values as absolute (TRUE) or relative (FALSE)
#' for additional (type-specific) diagnostic information. If not defined (NULL) additional diagnostics
#' will not be shown.
#' @return return selected input data
#' @author Kristine Karstens, Felicitas Beier, Jan Philipp Dietrich
#'
#' @export

toolIso2CellCountries <- function(x, cells = "lpjcell", absolute = NULL) {
  # 'cells' no longer selects anything. It is accepted for one release because released
  # packages still pass cells = "lpjcell"; remove it once they have been updated.
  if (!identical(cells, "lpjcell")) {
    stop("toolIso2CellCountries: cells = \"", cells, "\" is no longer supported.")
  }
  map <- toolGetMappingCoord2Country()
  isoCellCountry <- unique(map$iso)
  y <- x[isoCellCountry, , ]

  if (isTRUE(absolute)) {
    total    <- dimSums(dimSums(x, dim = c(2, 3)), dim = 1)
    returned <- dimSums(dimSums(y, dim = c(2, 3)), dim = 1)
    lost     <- round((1 - (returned / total)) * 100, 2)
    if (lost != 0) vcat(1, paste0(lost, "% of the summed up values of the data set got lost."))
  }

  return(y)
}
