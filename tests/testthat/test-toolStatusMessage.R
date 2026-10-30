test_that("toolStatusMessage attaches messages to the innermost madrat function", {
  withr::local_options(list(madratMessage = NULL))
  statusKeys <- function() names(getOption("madratMessage")$status)

  # direct call inside a madrat-style function
  calcDirect <- function() toolStatusMessage("ok", "direct call")
  calcDirect()
  expect_identical(statusKeys(), "calcDirect")
  madrat::resetMadratMessages("status")

  # wrapper chain via toolExpectTrue
  readExpect <- function() toolExpectTrue(TRUE, "check ok")
  readExpect()
  expect_identical(statusKeys(), "readExpect")
  madrat::resetMadratMessages("status")

  # wrapper chain via toolExpectLessDiff
  calcDiff <- function() toolExpectLessDiff(1:3, 2:4, 10, "close enough")
  calcDiff()
  expect_identical(statusKeys(), "calcDiff")
  madrat::resetMadratMessages("status")

  # non-madrat helper in between
  myHelper <- function() toolExpectTrue(TRUE, "check via helper")
  readHelper <- function() myHelper()
  readHelper()
  expect_identical(statusKeys(), "readHelper")
  madrat::resetMadratMessages("status")

  # nested madrat functions: innermost one wins
  calcInner <- function() toolStatusMessage("ok", "inner")
  readOuter <- function() calcInner()
  readOuter()
  expect_identical(statusKeys(), "calcInner")
  madrat::resetMadratMessages("status")

  # anonymous functions and functional calls are skipped
  calcLapply <- function() invisible(lapply(1:2, function(i) toolStatusMessage("ok", paste0("message ", i))))
  calcLapply()
  expect_identical(statusKeys(), "calcLapply")
  madrat::resetMadratMessages("status")

  # do.call does not hide the calling function
  calcDoCall <- function() toolStatusMessage("ok", "via do.call")
  do.call("calcDoCall", list())
  expect_identical(statusKeys(), "calcDoCall")
  madrat::resetMadratMessages("status")
})

test_that("toolStatusMessage falls back sensibly if no madrat function is in the call stack", {
  withr::local_options(list(madratMessage = NULL))
  statusKeys <- function() names(getOption("madratMessage")$status)

  # nearest enclosing named function call is used
  plainInner <- function() toolStatusMessage("ok", "inner")
  plainOuter <- function() plainInner()
  plainOuter()
  expect_identical(statusKeys(), "plainInner")
  madrat::resetMadratMessages("status")
})

test_that("toolStatusFunctionFromCalls resolves the call stack correctly", {
  self <- quote(toolStatusMessage("ok", "m"))

  # no other frame at all
  expect_identical(mstools:::toolStatusFunctionFromCalls(list(self)), ".GlobalEnv")

  # innermost madrat function wins over outer ones
  calls <- list(quote(calcOuter()),
                quote(readMiddle()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "readMiddle")

  # engine functions are skipped
  calls <- list(quote(calcOuter()),
                quote(readSource()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "calcOuter")

  # engine functions are not used as fallback key either
  calls <- list(quote(retrieveData()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), ".GlobalEnv")
  calls <- list(quote(myFun()),
                quote(retrieveData()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "myFun")

  # S3 method suffixes are stripped
  calls <- list(quote(readFoo.default()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "readFoo")

  # namespaced calls are resolved to the plain function name
  calls <- list(quote(yaml:::readBar()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "readBar")
  calls <- list(quote(yaml::calcBaz()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "calcBaz")

  # lower case names are no madrat functions, fallback to nearest named call
  calls <- list(quote(calculateStuff()),
                quote(somethingElse()),
                quote(toolStatusMessage("ok", "m")))
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "somethingElse")

  # functional and anonymous frames are skipped for the fallback
  anonymous <- as.call(list(function(x) x,
                            quote(i)))
  calls <- list(quote(myFun()),
                quote(lapply(1:2, FUN)), anonymous, self)
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "myFun")

  # NULL frames are ignored
  calls <- list(quote(myFun()), NULL, self)
  expect_identical(mstools:::toolStatusFunctionFromCalls(calls), "myFun")
})
