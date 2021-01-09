

#' Of a PostgreSQL Partioned Table Get Partitions
#'
#' Of the PostgreSQL (r)egular table or (p)artitioned table get the information of "partition child tables" (if any) (with schema name).
#'
#' @param conn PostgreSQLConnection . Required.
#' @param name String. Required. Name of the table.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns Dataframe of information of "partition child tables" (if any) (with schema name).
#' @examples
#' \dontrun{
#' dbListPartitionsEM(conn, name = "sample")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DataCombine FillDown
#' @export hierarchy
dbListPartitionsEM <- function(conn, name, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(conn)) {
    Stop("Parameter \"conn\" is required.")
  }

  if(missing(name)) {
    Stop("Parameter \"name\" is required.")
  }

  SchemaAndName <- dbTableNameFix(conn, t.nm = name)

  Statement <- paste0("
  SELECT
      -- nmsp_parent.nspname AS parent_schema,
      -- parent.relname      AS parent,
      nmsp_child.nspname  AS part_schema,
      child.relname       AS part_table
  FROM pg_inherits
      JOIN pg_class parent        ON pg_inherits.inhparent = parent.oid
      JOIN pg_class child         ON pg_inherits.inhrelid = child.oid
      JOIN pg_namespace nmsp_parent   ON nmsp_parent.oid  = parent.relnamespace
      JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
  AND nmsp_parent.nspname = ", first(SchemaAndName),"
  AND      parent.relname = ", last(SchemaAndName),
  ";")

  Results <- dbGetQueryEM(conn, Statement = Statement, display = display, exec = exec)

  if(NROW(Results)) {
    return(Results[, , drop = FALSE])
  } else {
    return(data.frame(list(PART_SCHEMA = "", PART_TABLE = ""))[FALSE, , drop = F])
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}



#' Of a PostgreSQL Table Get Partition Metadata
#'
#' Of the PostgreSQL (r)egular table or (p)artitioned table get the "partition key definition" (if any) and/or "partition bound" (if any).
#'
#' @param conn PostgreSQLConnection . Required.
#' @param name String. Required. Name of the table.
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns Dataframe of information of "partition key definition" (if any) and/or "partition bound" (if any).
#' @examples
#' \dontrun{
#' dbGetPartMetaEM(conn, name = "sample")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DataCombine FillDown
#' @export
dbListPartMetaEM <- function(conn, name, display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(conn)) {
    Stop("Parameter \"conn\" is required.")
  }

  if(missing(name)) {
    Stop("Parameter \"name\" is required.")
  }

  SchemaAndName <- dbTableNameFix(conn, t.nm = name)

  Statement <- paste0("
  SELECT -- nsp.nspname,
         -- c.relname,
         -- c.relkind,
         pg_catalog.pg_get_partkeydef(c.oid) AS part_key_def,
         pg_catalog.pg_get_expr(c.relpartbound,c.oid) AS part_bound
  FROM pg_catalog.pg_class c
    JOIN pg_catalog.pg_namespace nsp ON c.relnamespace = nsp.oid
    LEFT JOIN pg_catalog.pg_description d
           ON d.objoid = c.oid
          AND d.objsubid = 0
          AND d.classoid = 'pg_class'::regclass
  WHERE c.relkind IN ('r','p')
  AND  nsp.nspname NOT IN ('information_schema','pg_toast','pg_catalog')
  AND nsp.nspname = ", first(SchemaAndName),"
  AND   c.relname = ",  last(SchemaAndName),"
  ")

  Results <- dbGetQueryEM(conn, Statement = Statement, display = display, exec = exec)

  if(NROW(Results)) {
    Result <- DataCombine::FillDown(Results, Var = "PART_KEY_DEF")
    Result <- DataCombine::FillDown(Results, Var = "PART_BOUND")
    return(Results[NROW(Results), , drop = FALSE])
  } else {
    return(data.frame(list(PART_KEY_DEF = "", PART_BOUND = ""))[FALSE, , drop = F])
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
