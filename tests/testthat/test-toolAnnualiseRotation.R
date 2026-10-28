# Pulse series as LPJmL reports them for short rotation trees: zero in most
# years, the whole rotation's growth in the harvest year (normal case: every
# 8 years). Cell 1 is the reference, cells 2-5 are the edge cases:
#   1: regular 8-year cycles
#   2: same cycle, first harvest 3 years later
#   3: irregular harvest years
#   4: no harvests at all
#   5: yearly harvests (already annualised)
years    <- 1901:2010
yields   <- c(40, 48, 40, 48, 56, 48, 56, 64, 56, 64, 72, 72, 64) # 13 cycles, rising
harvest1 <- seq(years[1] + 7, max(years), by = 8)
harvest2 <- seq(years[1] + 10, max(years), by = 8)
harvest3 <- c(1916, 1931, 1941, 1946, 1954, 1966, 1969, 1972, 1976, 1980, 1999, 2003)

pulses <- new.magpie(1:5, years, "betr.rainfed", fill = 0)
pulses[1, harvest1, ] <- yields
pulses[2, harvest2, ] <- yields
pulses[3, harvest3, ] <- yields[-5]                                # 12 harvest years
pulses[5, , ] <- rep(yields, length.out = length(years)) / 8

series <- function(x, cell) as.numeric(x[cell, , ])

annualised <- toolAnnualiseRotation(pulses)

test_that("input requirements are enforced", {
  expect_error(toolAnnualiseRotation(pulses[, 1901, ]), "consecutive years")
  expect_error(toolAnnualiseRotation(pulses[, c(1901, 1903:1910), ]), "consecutive years")
  withNA <- pulses
  withNA[1, 1950, ] <- NA
  expect_error(toolAnnualiseRotation(withNA), "without NAs")
  negative <- pulses
  negative[1, 1950, ] <- -1
  expect_error(toolAnnualiseRotation(negative), "negative")
})

test_that("a regular rotation becomes the yearly rate of its cycle", {
  expected <- c(rep(yields / 8, each = 8),  # 13 cycles, 1901-2004
                rep(yields[13] / 8, 6))     # last rate held over 2005-2010
  expect_equal(series(annualised, 1), expected)
})

test_that("dimensions, names and sets are kept", {
  expect_true(is.magpie(annualised))
  expect_identical(dim(annualised), dim(pulses))
  expect_identical(dimnames(annualised), dimnames(pulses))
  expect_identical(getSets(annualised), getSets(pulses))
})

test_that("the first harvest is spread over the years since the start when those exceed rotation", {
  # cell 2 harvests first in 1911, the 11th year, so its first harvest covers
  # 11 years of growth instead of the nominal 8
  expected <- c(rep(yields[1] / 11, 11),
                rep(yields[-1] / 8, each = 8), # 12 cycles, 1912-2007
                rep(yields[13] / 8, 3))        # 2008-2010
  expect_equal(series(annualised, 2), expected)
})

test_that("rotation sets the first interval if the first harvest comes earlier", {
  x <- new.magpie("GLO.1", 1901:1912, "betr.rainfed", fill = 0)
  x[, 1903, ] <- 80
  # only 3 of the 8 rotation years lie in the series, so 3/8 of the harvest is
  # allocated before it and the rate is held afterwards
  expect_equal(as.numeric(toolAnnualiseRotation(x)), rep(80 / 8, 12))
  expect_equal(as.numeric(toolAnnualiseRotation(x, rotation = 4)), rep(80 / 4, 12))
  # a rotation shorter than the first interval is overruled by the data
  expect_equal(as.numeric(toolAnnualiseRotation(x, rotation = 2)), rep(80 / 3, 12))
})

test_that("irregular rotations use the interval found in the data", {
  # intervals of cell 3: 16 (start of series), then the gaps between harvest3
  interval <- c(16, 15, 10, 5, 8, 12, 3, 3, 4, 4, 19, 4)
  rate     <- yields[-5] / interval
  expected <- c(rep(rate, times = interval),  # 1901-2003
                rep(rate[12], 7))             # 2004-2010
  expect_equal(series(annualised, 3), expected)
})

test_that("a skipped harvest is spread over the longer interval", {
  delayed <- new.magpie("GLO.1", 1901:1932, "betr.rainfed", fill = 0)
  delayed[, c(1908, 1917, 1925), ] <- c(40, 54, 48)
  expect_equal(as.numeric(toolAnnualiseRotation(delayed)),
               c(rep(40 / 8, 8),  # 1901-1908
                 rep(54 / 9, 9),  # 1909-1917, the harvest missed in 1916
                 rep(48 / 8, 8),  # 1918-1925, back on schedule
                 rep(48 / 8, 7))) # 1926-1932, held tail
})

test_that("a harvest split across two years keeps its mass but stays a spike", {
  # the missed part of a failed harvest can instead appear as a second pulse one
  # year later. Its interval is one year, so it lands in that single year - the
  # same rule whose end-of-series effect is the open question at the end of this file
  double <- new.magpie("GLO.1", 1901:1940, "betr.rainfed", fill = 0)
  double[, c(1908, 1916, 1917, 1925, 1933), ] <- c(40, 48, 16, 48, 48)
  expect_equal(as.numeric(toolAnnualiseRotation(double)),
               c(rep(40 / 8, 8),   # 1901-1908
                 rep(48 / 8, 8),   # 1909-1916
                 16,               # 1917, interval of one year
                 rep(48 / 8, 8),   # 1918-1925
                 rep(48 / 8, 8),   # 1926-1933
                 rep(48 / 8, 7)))  # 1934-1940, held tail
})

test_that("each harvest after the first is conserved within its interval", {
  for (cell in 1:3) {
    pulse        <- series(pulses, cell)
    rate         <- series(annualised, cell)
    harvestYears <- which(pulse > 0)
    interval     <- diff(harvestYears)
    # the years each harvest after the first has grown over
    window       <- sequence(interval, from = harvestYears[-length(harvestYears)] + 1)
    expect_equal(rate[window], rep(rate[harvestYears[-1]], interval))
    expect_equal(rate[harvestYears[-1]] * interval, pulse[harvestYears[-1]])
  }
})

test_that("a cell without harvests stays at zero", {
  expect_equal(series(annualised, 4), rep(0, length(years)))
  zeros <- new.magpie(1:2, years, "betr.rainfed", fill = 0)
  expect_equal(as.numeric(toolAnnualiseRotation(zeros)), rep(0, length(zeros)))
})

test_that("a yearly series is changed in its first year only", {
  # every year is a harvest here, so all intervals are one year. The first year
  # is the documented exception: it is divided by rotation, so an already
  # annualised cell does not come back unchanged
  rate  <- series(annualised, 5)
  pulse <- series(pulses, 5)
  expect_equal(rate[-1], pulse[-1])
  expect_equal(rate[1], pulse[1] / 8)
})

test_that("annualising twice is not idempotent: the first year is divided again", {
  twice <- toolAnnualiseRotation(annualised)
  expect_equal(as.numeric(twice[, 1902:2010, ]), as.numeric(annualised[, 1902:2010, ]))
  expect_equal(as.numeric(twice[, 1901, ]), as.numeric(annualised[, 1901, ]) / 8)
})

test_that("items in the third dimension are annualised independently", {
  x <- new.magpie(1, years, c("betr.rainfed", "betr.irrigated"), fill = 0)
  x[, harvest1, "betr.rainfed"]   <- yields
  x[, harvest2, "betr.irrigated"] <- yields
  out <- toolAnnualiseRotation(x)
  expect_equal(as.numeric(out[, , "betr.rainfed"]), series(annualised, 1))
  expect_equal(as.numeric(out[, , "betr.irrigated"]), series(annualised, 2))
})

test_that("a harvest in the first year is rated at the nominal rotation", {
  first <- new.magpie("GLO.1", 1901:1910, "betr.rainfed", fill = 0)
  first[, 1901, ] <- 16
  expect_equal(as.numeric(toolAnnualiseRotation(first)), rep(16 / 8, 10))
})

test_that("a harvest in the last year leaves no held tail", {
  last <- new.magpie("GLO.1", 1901:1910, "betr.rainfed", fill = 0)
  last[, 1910, ] <- 80
  expect_equal(as.numeric(toolAnnualiseRotation(last)), rep(80 / 10, 10))
})

test_that("extending the series by one rotation changes only the held tail", {
  long <- new.magpie(1, 1901:2012, "betr.rainfed", fill = 0)
  long[, seq(years[1] + 7, 2012, by = 8), ] <- c(yields, mean(yields)) # one cycle more
  outLong <- toolAnnualiseRotation(long)
  expect_equal(as.numeric(outLong[, 1901:2004, ]), as.numeric(annualised[1, 1901:2004, ]))
  # afterwards each series holds its own last rate: the short one the 13th
  # cycle's, the long one the extra cycle's
  expect_equal(as.numeric(annualised[1, 2005:2010, ]), rep(yields[13] / 8, 6))
  expect_equal(as.numeric(outLong[, 2005:2012, ]), rep(mean(yields) / 8, 8))
})

# OPEN QUESTION - skipped until the convention is decided. Two candidates -
# ignore intervals far below rotation when choosing the rate to hold, or hold
# the rate of the last full interval - both satisfy the expectations below, so
# they stay as written once one is picked.
test_that("a double harvest as the last pulse does not inflate the held tail", {
  skip("held tail after a one-year interval: convention not decided yet")

  x <- new.magpie("GLO.1", 1901:1930, "betr.rainfed", fill = 0)
  x[, c(1908, 1916, 1917), ] <- c(40, 48, 16)
  tailRate <- as.numeric(toolAnnualiseRotation(x)[, 1918:1930, ])

  # the stand keeps growing, so the tail is positive and held constant ...
  expect_true(all(tailRate > 0))
  expect_equal(tailRate, rep(tailRate[1], length(tailRate)))
  # ... but the 1917 pulse grew for one year, so its rate must not be the one
  # held: at the spike rate the tail adds 208 instead of at most 78
  expect_true(all(tailRate <= 48 / 8))
})
