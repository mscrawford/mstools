#' @title toolAnnualiseRotation
#'
#' @description Turns the harvest of a rotation crop into a yearly rate.
#' This function spreads each harvest evenly over the years since the previous harvest
#' of the same cell. The intervals between harvests tile the series, so every year gets
#' the rate of the next harvest in or after it.
#'
#' The intervals are read from the series itself: a cell that skips a rotation has its
#' next harvest spread over a longer interval.
#'
#' The ends get special treatment. The first harvest of a cell is divided by the number
#' of years since the start of the series, or by \code{rotation} if that number is smaller,
#' and written to all years up to the harvest. After the last harvest the last yearly rate
#' is held; this adds to the total of the series.
#'
#' @param x        magpie object with yearly values for consecutive years, without NAs
#'                 and without negative values
#' @param rotation nominal rotation length in years. It is used for the first harvest
#'                 of a cell only; all later intervals come from the data
#' @return magpie object of the same dimensions as x
#' @author Michael Crawford
#'
#' @examples
#' x <- new.magpie("GLO.1", 1901:1924, "rainfed.biomass tree", fill = 0)
#' x[, c(1903, 1911, 1919), ] <- 80
#' toolAnnualiseRotation(x)
#' @export

toolAnnualiseRotation <- function(x, rotation = 8) {

  if (!is.magpie(x)) {
    stop("toolAnnualiseRotation needs a MAgPIE object.")
  }
  years <- getYears(x, as.integer = TRUE)
  if (nyears(x) < 2 || any(diff(years) != 1)) {
    stop("toolAnnualiseRotation needs yearly data for consecutive years.")
  }
  if (anyNA(x) || any(x < 0)) {
    stop("toolAnnualiseRotation needs data without NAs and without negative values.")
  }
  if (!is.numeric(rotation) || length(rotation) != 1 || !is.finite(rotation) || rotation <= 0) {
    stop("toolAnnualiseRotation needs a single positive number as rotation.")
  }

  # apply returns the year dimension first, thus we need to permutate
  # the array afterwards
  rate    <- apply(as.array(x), c(1, 3), .annualiseSeries, rotation = rotation)
  x[, , ] <- aperm(rate, c(2, 1, 3))

  return(x)
}

# yearly rates of one pulse series; conventions documented in toolAnnualiseRotation
.annualiseSeries <- function(pulse, rotation) {

  harvestYears <- which(pulse > 0)
  if (length(harvestYears) == 0) {
    return(pulse)
  }

  # years each harvest has grown over, taken from the data except for the first one
  yearsBetweenHarvests <- diff(harvestYears)
  interval <- c(max(rotation, harvestYears[1]), yearsBetweenHarvests)
  rate     <- pulse[harvestYears] / interval

  # the intervals tile the series; the rate of the last harvest is held to the end
  yearsToKeepConstantAtEnd <- length(pulse) - harvestYears[length(harvestYears)]
  return(rep(c(rate, rate[length(rate)]),
             times = c(harvestYears[1], yearsBetweenHarvests, yearsToKeepConstantAtEnd)))
}
