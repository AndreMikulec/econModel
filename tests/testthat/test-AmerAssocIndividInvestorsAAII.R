

# testthat::test_file("./tests/testthat/test-AmerAssocIndividInvestorsAAII.R")

ops <- options()

withr::local_options(list(
  econmodel_db_storage_name = "postgres",
  econmodel_db_user = "postgres",
  econmodel_passord = "postgres",
  econmodel_dbname  = "postgres"
))

e <- new.env(parent =  emptyenv())

test_that("basic pg in 1", {
  expect_equal(dbLoginEM(env = e), data.frame(DBLOGINEM = TRUE))
})

test_that("basic pg test con 1", {
  expect_equal(dbIsConnectedEM(env = e), data.frame(DBISCONNECTEDEM = TRUE))
})

test_that("basic pg curr user 1", {
  expect_equal(dbGetCurrentUserEM(env = e), data.frame(DBCURRENTUSEREM = "postgres"))
})

test_that("basic pg curr extra 1", {
  expect_equal(as.vector(unlist(dbGetInfoExtraEM(env = e))["CURRENT_DBNAME"]), "postgres")
})

test_that("basic pg create user", {
  expect_equal(dbCreateUserEM(user = "r_user_test", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"), env = e), data.frame(DBCREATEUSEREM = TRUE))
})

test_that("basic pg exist user", {
  expect_equal(dbExistsUserEM(user = "r_user_test", env = e), data.frame(DBEXISTSUSEREM = TRUE))
})

test_that("basic pg create db", {
  expect_equal(dbCreateDbaseEM(dbname = "r_user_test", env = e), data.frame(DBCREATEDBASEEM = TRUE))
})

test_that("basic pg out 1", {
  expect_equal(dbLogoutEM(env = e), data.frame(DBLOGOUTEM = TRUE))
})

test_that("basic pg in 2", {
  expect_equal(dbLoginEM(user = "r_user_test", env = e), DBLOGINEM = TRUE)
})

test_that("basic pg curr user 2", {
  expect_equal(dbGetCurrentUserEM(env = e), data.frame(DBCURRENTUSEREM = "r_user_test"))
})

test_that("basic pg test con 2", {
  expect_equal(dbIsConnectedEM(env = e), data.frame(DBISCONNECTEDEM = TRUE))
})

test_that("basic pg curr extra 2", {
  expect_equal(as.vector(unlist(dbGetInfoExtraEM(env = e))["CURRENT_SCHEMA"], "public"))
})

test_that("basic pg create schema", {
  expect_equal(dbCreateSchemaEM(schema = "r_user_test", env = e), data.frame(DBEXISTSSCHEMAEM = TRUE))
})


test_that("basic pg curr extra 2", {
  expect_equal(as.vector(unlist(dbGetInfoExtraEM(env = e))["CURRENT_SCHEMA"]), "r_user_test")
})


test_that("basic pg exec drop schema", {
  expect_true(dbExecuteEM(Statement = "DROP SCHEMA r_user_test;", env = e), data.frame(DBEXECUTEEM = TRUE))
})


test_that("basic pg out 2", {
  expect_equal(dbLogoutEM(env = e), data.frame(DBLOGOUTEM = TRUE))
})

withr::local_options(ops)

withr::local_options(list(
  econmodel_db_storage_name = "postgres",
  econmodel_db_user = "postgres",
  econmodel_passord = "postgres",
  econmodel_dbname  = "postgres"
))

test_that("basic pg in 3", {
  expect_equal(dbLoginEM(env = e), data.frame(DBLOGINEM = TRUE))
})

test_that("basic pg curr user 2", {
  expect_equal(dbGetCurrentUserEM(env = e), data.frame(DBCURRENTUSEREM = "postgres"))
})

test_that("basic pg curr extra 2", {
  expect_equal(as.vector(unlist(dbGetInfoExtraEM(env = e))["CURRENT_SCHEMA"]), "public")
})

test_that("basic pg exec drop db", {
  expect_equal(dbExecuteEM(Statement = "DROP DATABASE r_user_test;", env = e), data.frame(DBEXECUTEEM = TRUE))
})

test_that("basic pg exec drop user", {
  expect_equal(dbExecuteEM(Statement = "DROP USER r_user_test;", env = e), data.frame(DBEXECUTEEM = TRUE))
})


test_that("basic pg out 3", {
  expect_equal(dbLogoutEM(env = e), data.frame(DBLOGOUTEM = TRUE))
})


withr::local_options(ops)
