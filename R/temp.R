
#' Connect to the econModel database.
#'
#' Currently is only implemented to work on PostgreSQL or PostgreSQL-like databases.
#'
#' This function tries to return a new "DBI" connection object to the environment "env".
#'
#' First, this function will try to connect to the database using the user name "user" specified from getOption("econmodel_db_*") parameters listed in the "parameters" section.
#'
#' If the connection can be made, then next the connection is tried to be made using the user/password/dbname of "r_user_econmodel".
#' If the connection is made, then next the schema "r_user_econmodel" is attempted to be made..
#'
#' If the connection can not be made, then next the connection is tried to be made using the user/password/dbname of "postgres".
#' If the connection can be made, the the user/password/dbname/schema of "r_user_econmodel" will be made.
#' Next, disconnect. Connect as "r_user_econmodel".
#' Create the user/password/dbname of "user"
#' Next, disconnect. Connect as "user".
#' Create the schema "user".
#' Store the new connection object connName in "env".
#'
#' @param driver String. Defaults to getOption("econmodel_db_driver"). String.  Default is "PostgreSQL".  Currently only an implementation exists using PostgreSQL and PostgreSQL-like databases.
#' @param user String. Defaults to getOption("econmodel_db_user").
#' @param password String. Defaults to "user". If missing, then defaults to getOption("econmodel_db_password") .
#' @param host String. Defaults to getOption("econmodel_db_host").
#' @param dbname String. Defaults to "user". If missing, then defaults to getOption("econmodel_db_dbname").
#' @param port Integer. Defaults to getOption("econmodel_db_port").
#' @param tty Default to getOption("econmodel_db_tty").
#' @param options Defaults to getOption("econmodel_db_dboptions").
#' @param forceISOdate Logical. Default is getOption("econmodel_db_forceISOdate").
#' @param connName String.  Name of the database connection object.  The default is "connEM".
#' @param env Environment.  Default is the .Global environment.  This is the environment to return the connection object "connEM".
#' @param auto.assign Logical. Should the results be loaded to "env" as variable "connEM" If FALSE, then results are returned from the function  normally.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns DBI connection object named "connEM" is created, connected and assigned to the environment "env".
#' @examples
#' \dontrun{
#' dbLoginEM()
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbConnect
#' @export
dbLoginEM2 <- function(driver, user, password = user, host, dbname = user, port,
                      tty, options, forceISOdate, connName, env, auto.assign = TRUE, display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  #correct for TZ
  oldtz <- Sys.getenv('TZ')
  if(oldtz=='') {
    Sys.setenv(TZ="UTC")
  }
  on.exit({Sys.setenv(TZ=oldtz)})

  if(missing(driver)) {
    driver <- getOption("econmodel_db_driver")
  }
  if(!driver %in% "PostgreSQL") {
    stop("Parameter \"driver\" must be \"PostreSQL\".  No other driver is implemented at this time.")
  }

  if(missing(user)) {
    user <- getOption("econmodel_db_user")
  }
  # note: "password" is never missing
  if(missing(password)) {
    password <- getOption("econmodel_db_password")
  }
  if(missing(host)) {
    host <- getOption("econmodel_db_host")
  }
  # note: "dbname" is never missing
  if(missing(dbname)) {
    dbname <- getOption("econmodel_db_dbname")
  }
  if(missing(port)) {
    port <- getOption("econmodel_db_port")
  }
  if(missing(tty)) {
    tty <- getOption("econmodel_db_tty")
  }
  if(missing(options)) {
    dboptions <- getOption("econmodel_db_dboptions")
  }
  if(missing(forceISOdate)) {
    forceISOdate <- getOption("econmodel_db_forceISOdate")
  }

  if(missing(env)) {
    env <- .GlobalEnv
    ReturnTo <- "the Global environment"
  }
  if(identical(env, .GlobalEnv)) {
    ReturnTo <- "the Global environment"
  } else {
    ReturnTo <- paste0("environment ", capture.output(env))
  }

  if(missing(connName)) {
    connName <- "connEM"
  }

  drv <- try({drv <- DBI::dbDriver(getOption("econmodel_db_driver"))}, silent = TRUE)
  if(!inherits(drv, "try-error")) {

    connNameConnected <- FALSE
    connNameExists      <- exists(connName, envir = env, inherits = FALSE)
    if(connNameExists) {
      connNameConnected <- dbIsConnectedEM(get(connName, envir = env, inherits = FALSE), display = display, exec = exec)
    }

    haveConnLocal <- FALSE
    if(connNameExists && connNameConnected) {
      # creates a reference
      conn <- try({get(connName, envir = env, inherits = FALSE)}, silent = TRUE)
    }
    if(!inherits(conn, "try-error")) {
      haveConnLocal <- TRUE

      tmp.query <- "SET client_encoding TO 'UTF8';"
      try({dbExecuteEM(conn, statement = tmp.query, display = display, exec = exec)})

    }

    if( !haveConnLocal
        ||
        (
          !connNameExists
          ||
          (connNameExists && !connNameConnected)
        )
    ) {
      conn <- try({DBI::dbConnect(drv,
                                  user = user, password = password,
                                  host = host, dbname = dbname, port = port,
                                  tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                  silent = TRUE)
    }
    if(!inherits(conn, "try-error") && dbGetInfo(conn)$user == user) {
      message(paste0("Successfully connected to user \"", user, "\"."))

      tmp.query <- "SET client_encoding TO 'UTF8';"
      try({dbExecuteEM(conn, statement = tmp.query, display = display, exec = exec)})


      if(!dbExistsSchemaEM(conn, schema = user, display = display, exec = exec)) dbCreateSchemaEM(conn, schema = user, display = display, exec = exec)
      # creates a reference
      if(auto.assign) {
        assign(connName, conn, envir = env)
        message(paste0("Connection R object \"connEM\" has been returned to ", ReturnTo, "."))
      } else {
        return(conn)
      }
      # "r_user_econmodel" in the "r_user_econmodel" database
      # tries to create the schema "r_user_econmodel"
      # do not Disconnect
    }
    else {
      # try another connection
      if(haveConnLocal) {
        dbDisconnectEM(conn)
      }
      conn <- try({DBI::dbConnect(drv,
                                  user = "r_user_econmodel", password = "r_user_econmodel",
                                  host = host, dbname = "r_user_econmodel", port = port,
                                  tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                  silent = TRUE)

      if(!inherits(conn, "try-error")) {

        message(paste0("Successfully connected to user \"", "r_user_econmodel", "."))

        tmp.query <- "SET client_encoding TO 'UTF8';"
        try({dbExecuteEM(conn, statement = tmp.query, display = display, exec = exec)})

        if(!dbExistsSchemaEM(conn, schema = "r_user_econmodel", display = display, exec = exec)) dbCreateSchemaEM(conn, schema = "r_user_econmodel", display = display, exec = exec)
        # user = user

        if(!dbExistsUserEM( conn, user   = user,   display = display, exec = exec))  dbCreateUserEM( conn,  user   = user, display = display, exec = exec)
        if(!dbExistsDbaseEM(conn, dbname = dbname, display = display, exec = exec))  dbCreateDbaseEM(conn,  dbname = user, display = display, exec = exec)
        dbDisconnectEM(conn)
        #
        # as "ruser"
        # login to the database "user" and and create the "user" schema
        #
        conn <- try({DBI::dbConnect(drv,
                                    user = user, password = password,
                                    host = host, dbname = user, port = port,
                                    tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                    silent = TRUE)
        if(!inherits(conn, "try-error")) {
          # "user" in the "user" database

          tmp.query <- "SET client_encoding TO 'UTF8';"
          try({dbExecuteEM(conn, statement = tmp.query, display = display, exec = exec)})

          tmp.query <- "SET timezone TO 'UTC';"
          try({dbExecuteEM(conn, statement = tmp.query, display = display, exec = exec)})

          # tries to create the schema "user"
          if(!dbExistsSchemaEM(conn, schema = user, display = display, exec = exec)) dbCreateSchemaEM(conn, schema = user, display = display, exec = exec)
          dbDisconnectEM(conn)
          # login as "user" to its own database "user"
          dbLoginEM(connName = connName, env = env, display = display, exec = exec)
        } else {
          stop(paste0("\"r_user_econmodel\" could not connect to the user database ", user))
        }

      } else {
        # try another connection
        conn <- try({DBI::dbConnect(drv,
                                    user = "postgres", password = "postgres",
                                    host = host, dbname = "postgres", port = port,
                                    tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                    silent = TRUE)
        if(!inherits(conn, "try-error")) {
          message(paste0("Successfully connected to user \"", "postgres", "\"."))

          tmp.query <- "SET client_encoding TO 'UTF8';"
          try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})

          tmp.query <- "SET timezone TO 'UTC';"
          try({dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)})

          if(!dbExistsUserEM( conn, user   = "r_user_econmodel", display = display, exec = exec)) dbCreateUserEM(conn, user = "r_user_econmodel", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"), display = display, exec = exec)
          if(!dbExistsDbaseEM(conn, dbname = "r_user_econmodel", display = display, exec = exec)) dbCreateDbaseEM(conn, dbname = "r_user_econmodel", display = display, exec = exec)
          dbDisconnectEM(conn)
          DBI::dbConnect(driver, user = "r_user_econmodel", password = "r_user_econmodel", host = host, dbname = "r_user_econmodel", port = port,
                         tty = tty, options = dboptions, forceISOdate = forceISOdate, connName = connName, env = env)
        } else {
          stop("Failed to login as getOption(\"econmodel_db_user\"), \"r_user_econmodel\", and \"postgres.\"  Please set options()")
        }
      }
    }
  } else {
    stop(paste0("Parameter \"driver\" is specified as \"", driver, "\".  But the driver failed to load."))
  }

  invisible()

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
