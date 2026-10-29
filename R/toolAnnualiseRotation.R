#' @title toolAnnualiseRotation
#'
#' @description Turns the harvest of a rotation crop into a yearly rate.
#' LPJmL harvests biomass trees once per rotation, so their yearly harvest is zero in
#' most years and large in the harvest year. A smoothing spline through such a series
#' follows the position of the last harvest instead of the trend. This function spreads
#' each harvest evenly over the years since the previous harvest of the same cell, so
#' that the sum over a rotation is kept and the series can be smoothed like any other.
#' The intervals between harvests tile the series, so every year gets the rate of the
#' next harvest in or after it.
#'
#' The interval is read from the series itself: a cell that skips a rotation has its
#' next harvest spread over the longer interval. Two conventions cover the ends of the
#' series. The first harvest of a cell is divided by the number of years since the start
#' of the series, or by \code{rotation} if that number is smaller, and written to all
#' years up to the harvest. After the last harvest the last yearly rate is held, because
#' the stand is still growing; this adds to the total of the series.
#'
#' The function must see the pulse series exactly once. A second pass is not harmless:
#' every year is positive by then, so the first year is divided by \code{rotation} again.
#'
#' @param x        magpie object with yearly values for consecutive years, without NAs
#'                 and without negative values (cells x years x items)
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

  # the intervals tile the series; after the last harvest the stand keeps growing,
  # so its rate is held to the end
  yearsToKeepConstantAtEnd <- length(pulse) - harvestYears[length(harvestYears)]
  return(rep(c(rate, rate[length(rate)]),
             times = c(harvestYears[1], yearsBetweenHarvests, yearsToKeepConstantAtEnd)))
}
