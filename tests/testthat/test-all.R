context("all")


test_that("tryCatchLog::tryCatchLog is functional", {

  expect_true( (function() { tryCatchLog::tryCatchLog({ log("a") }, error = function(e) { "message" %in%  names(e) && "call" %in% names(e) } ) })() )

})
