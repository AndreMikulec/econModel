context("all")


test_that("tryCatchLog::tryCatchLog is functional", {

  expect_true( (function() { tryCatchLog::tryCatchLog({ log("a") }, error = function(e) { "message" %in%  names(e) && "call" %in% names(e) } ) })() )

})

test_that("interleave", {

  expect_equal(interleave(letters[1:2], LETTERS[1:2]), c("a","A","b","B"))

})


test_that("pairWise", {

  expect_equal(
    pairWise( iris[1:2,1:2], airquality[1:2,1:2] ),
    list( list(Sepal.Length = c(5.1, 4.9), Ozone = c(41L, 36L)),
          list(Sepal.Width = c(3.5, 3), Solar.R = c(190L, 118L))
        )
  )

  expect_equal(
    pairWise(iris[1:2,1:2], airquality[1:2,1:2]),
    list( list(Sepal.Length = c(5.1, 4.9), Ozone = c(41L, 36L)),
          list(Sepal.Width = c(3.5, 3), Solar.R = c(190L, 118L))
        )
  )

  expect_equal(
    pairWise(iris[1:2,1:2], airquality[1:2,1, drop = F]),
    list(list(Sepal.Length = c(5.1, 4.9), Ozone = c(41L, 36L)),
         list(Sepal.Width = c(3.5, 3), Ozone = c(41L, 36L))
        )
  )
})

