#' Connect to the econModel database.
#'
#' Currently is only implemented to work on PostgreSQL or PostgreSQL-like databases.
#'
#' Try to connect or try to connect as "user".  If the user (or its support structure) does not exists, then create the support structure and the "user".
#'
#' @param driver String. Defaults to getOption("econmodel_db_driver"). String.  Default is "PostgreSQL".  Currently only an implementation exists using PostgreSQL and PostgreSQL-like databases.
#' @param connName String.  Default is "connEM". Contains the name of the variable that contains the name of the "connection" in the environment "env".
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
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @returns 1. invisibly a DBI connection 2. Object named "connEM" is created, connected, and assigned to the environment "env".
#' @examples
#' \dontrun{
#' # by default, this the lowercase name of the tempdir()
#' options(econmodel_db_storage_name = "postgres")
#' getOption("econmodel_db_user")
#' # In rstudio to a Restart of R
#' devtools::load_all(".")
#' getOption("econmodel_db_user")
#' [1] "postgres"
#' dbLoginEM()
#' # User login succeeded
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbConnect
#' @export
dbLoginEM <- function(driver, connName, user, password = user, host, dbname = user, port,
                      tty, options, forceISOdate, env, display = TRUE, exec = TRUE) {
  tryCatchLog::tryCatchLog({

    if(missing(connName)) {
      connName <- "connEM"
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

    # if(!missing(conn)) {
    #   Results <- dbIsConnectedEM(conn, display = display, exec = exec)
    #   if(exec) {
    #     if(unlist(Results)) {
    #       if(unlist(dbIsConnectedEM(get(connName, envir = env, inherits = FALSE), display = display, exec = exec))) {
    #         dbDisconnectEM(connName = connName, env = env)
    #       }
    #       # re-use
    #       assign(connName, conn, envir = env)
    #       return(invisible(conn))
    #     }
    #
    #   }
    # }

    if(missing(driver)) {
      driver <- getOption("econmodel_db_driver")
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


    # # (survey of the) current connection, if any
    #
    # connNameExists  <- exists(connName, envir = env, inherits = FALSE)
    # connNameConnected <- FALSE
    # if(connNameExists) {
    #   connNameConnected <- dbIsConnectedEM(get(connName, envir = env, inherits = FALSE), display = display, exec = exec)
    # }
    # if(connNameConnected) {
    #   CurrentUser <- dbGetCurrentuserEM(get(connName, envir = env, inherits = FALSE), display = display, exec = exec)
    #   if(exec) {
    #     if(unlist(CurrentUser) == user) {
    #       if(!unlist(dbExistsSchemaEM(get(connName, envir = env, inherits = FALSE), schema = user, display = display, exec = exec)))
    #         dbCreateSchemaEM(get(connName, envir = env, inherits = FALSE), schema = user, display = display, exec = exec)
    #       return(invisible(get(connName, envir = env, inherits = FALSE)))
    #     }
    #   }
    # }

    # # try to find or create a connection . . .
    #
    # SessionConn <- try({DBI::dbConnect(driver,
    #                                    user = user, password = password,
    #                                    host = host, dbname = dbname, port = port,
    #                                    tty = tty, options = dboptions, forceISOdate = forceISOdate)},
    #                    silent = TRUE)
    # if(inherits(SessionConn, "try-error")) {
    #   SessionConnLoginWorks <- FALSE
    #   message("User login failed.")
    # } else {
    #   SessionConnLoginWorks <- TRUE
    #   message("User login succeeded.")
    #   if(exec) {
    #     if(!unlist(dbExistsSchemaEM(SessionConn, schema = user, display = display, exec = exec)))
    #       dbCreateSchemaEM(SessionConn, schema = user, display = display, exec = exec)
    #     assign(connName, SessionConn, envir = env)
    #     return(invisible(SessionConn))
    #   }
    # }


    #
    # Connections are TOO volatile
    # and the DBI API is too unconsistent in the return value
    # to be able to write smart wrappers around it.
    # Therefore, just try to create a connection and return it
    # to the environment.
    #
    # try create a connection . . .

    SessionConn <- try({DBI::dbConnect(driver,
                                       user = user, password = password,
                                       host = host, dbname = dbname, port = port,
                                       tty = tty, options = dboptions, forceISOdate = forceISOdate)},
                       silent = TRUE)
    if(inherits(SessionConn, "try-error")) {
      SessionConnLoginWorks <- FALSE
      message("User login failed.")
    } else {
      SessionConnLoginWorks <- TRUE
      message("User login succeeded.")
      if(exec) {
        # if(!unlist(dbExistsSchemaEM(SessionConn, schema = user, display = display, exec = exec)))
        #   dbCreateSchemaEM(SessionConn, schema = user, display = display, exec = exec)
        assign(connName, SessionConn, envir = env)
        return(invisible(SessionConn))
      }
    }


    # PostgresConn <- try({DBI::dbConnect(driver,
    #                                     user = "postgres", password = "postgres",
    #                                     host = host, dbname = "postgres", port = port,
    #                                     tty = tty, options = dboptions, forceISOdate = forceISOdate)},
    #                     silent = TRUE)
    # if(inherits(PostgresConn, "try-error")) {
    #   PostgresConnLoginWorks <- FALSE
    #   message("Postgres login failed.")
    # } else {
    #   PostgresConnLoginWorks <- TRUE
    #   message("Postgres login succeeded.")
    #   if(exec) {
    #     # dispatcher database
    #     if(!unlist(dbExistsUserEM(PostgresConn, user   = "r_user_econmodel", display = display, exec = exec)))
    #       dbCreateUserEM(PostgresConn, user = "r_user_econmodel", attributes = c("LOGIN", "CREATEDB", "CREATEROLE"), display = display, exec = exec)
    #     if(!unlist(dbExistsDbaseEM(PostgresConn, dbname = "r_user_econmodel", display = display, exec = exec)))
    #       dbCreateDbaseEM(PostgresConn, dbname = "r_user_econmodel", display = display, exec = exec)
    #
    #     dbDisconnectEM(PostgresConn)
    #   }
    #
    # }


    # DispatcherConn <- try({DBI::dbConnect(driver,
    #                                       user = "r_user_econmodel", password = "r_user_econmodel",
    #                                       host = host, dbname = "r_user_econmodel", port = port,
    #                                       tty = tty, options = dboptions, forceISOdate = forceISOdate)},
    #                       silent = TRUE)
    # if(inherits(DispatcherConn, "try-error")) {
    #   DispatcherConnLoginWorks <- FALSE
    #   message("Dipatcher login failed.")
    # } else {
    #   DispatcherConnLoginWorks <- TRUE
    #   message("Dipatcher login succeeded.")
    #   if(exec) {
    #     if(!unlist(dbExistsSchemaEM(DispatcherConn, schema = "r_user_econmodel", display = display, exec = exec)))
    #       dbCreateSchemaEM(DispatcherConn, schema = "r_user_econmodel", display = display, exec = exec)
    #     # user database
    #     if(!unlist(dbExistsUserEM(DispatcherConn, user = user, display = display, exec = exec)))
    #       dbCreateUserEM(DispatcherConn, user = user, display = display, exec = exec)
    #     if(!unlist(dbExistsDbaseEM(DispatcherConn, dbname = user, display = display, exec = exec)))
    #       dbCreateDbaseEM(DispatcherConn, dbname = user, display = display, exec = exec)
    #     dbDisconnectEM(DispatcherConn)
    #     dbLoginEM(user = user)
    #   }
    # }

    return(invisible(data.frame(DBLOGINEM = logical())))

  }, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}

