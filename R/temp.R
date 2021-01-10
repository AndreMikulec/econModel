

#' Of a PostgreSQL Object, Get Inherited or Inherit_From Objects
#'
#' Of the PostgreSQL object, get the child/parent objects (if any) with the schema.
#'
#' Of column results named *_RELKIND, see the table pg_namespace column relkind.
#' These values are the following: r = ordinary table, i = index, S = sequence, t = TOAST table, v = view, m = materialized view, c = composite type, f = foreign table, p = partitioned table, I = partitioned index.
#'
#' Of column results named like '_PART_KEY_DEF' and '_PART_BOUND', these are the partition object key (LISTs, RANGEs, and HASHs) and the partitions (child objects) bounds (FOR VALUES . . .) (of the partition object).  See the references.
#'
#' @param conn PostgreSQLConnection . Required.
#' @param name String. Required. Name of the object.
#' @param side String. View from the "parent side. Default is "parent".  Alternately, the view from the "child side" is the value "child".
#' @param display Logical. Whether to display the query (defaults to \code{TRUE}).
#' @param exec Logical. Whether to execute the query (defaults to \code{TRUE}).
#' @param ... Dots passed.
#' @returns Dataframe of inherited or inherit_from "objects" (if any) (with schema name).
#' @references
#' \cite{Get number of partitions in PostgreSQL database
#' \url{https://stackoverflow.com/questions/8634144/get-number-of-partitions-in-postgresql-database }
#' }
#' @references
#' \cite{How to identify the ranges over which a postgres table was partitioned?
#' \url{https://dba.stackexchange.com/questions/221277/how-to-identify-the-ranges-over-which-a-postgres-table-was-partitioned}
#' }
#' @examples
#' \dontrun{
#' dbListInheritEM(conn, name = "sample")
#' }
#' @importFrom tryCatchLog tryCatchLog
#' @export
dbListInheritEM <- function(conn, name, side = "parent", display = TRUE, exec = TRUE, ...) {
tryCatchLog::tryCatchLog({

  Dots <- list(...)

  if(missing(conn)) {
    stop("Parameter \"conn\" is required.")
  }

  if(missing(name)) {
    stop("Parameter \"name\" is required.")
  }

  SchemaAndName <- dbTableNameFix(conn, t.nm = name, as.identifier = TRUE, dbQuote = "Literal")

  if(side == "parent") {
    Restriction <-
      paste0("
          parent_schema = ", first(SchemaAndName), "
      AND        parent = ", last(SchemaAndName)
      )
  }
  if(side == "child") {
    Restriction <-
      paste0("
           child_schema = ", first(SchemaAndName), "
       AND child        = ", last(SchemaAndName)
      )
  }

  Statement <- paste0("
  SELECT q.* FROM (
    SELECT
        nmsp_parent.nspname::text AS parent_schema,
        parent.relname::text      AS parent,
        parent.relkind::text      AS parent_relkind,
        pg_catalog.pg_get_partkeydef(parent.oid) AS parent_part_key_def,
        pg_catalog.pg_get_expr(parent.relpartbound, parent.oid) AS parent_part_bound,
        pg_inherits.inhseqno,
        nmsp_child.nspname::text  AS child_schema,
        child.relname::text       AS child,
        child.relkind::text       AS child_relkind,
        pg_catalog.pg_get_partkeydef(child.oid) AS child_part_key_def,
        pg_catalog.pg_get_expr(child.relpartbound, child.oid) AS child_part_bound
    FROM pg_inherits
    FULL JOIN pg_class parent        ON pg_inherits.inhparent = parent.oid
    FULL JOIN pg_class child         ON pg_inherits.inhrelid = child.oid
    FULL JOIN pg_namespace nmsp_parent   ON nmsp_parent.oid  = parent.relnamespace
    FULL JOIN pg_namespace nmsp_child    ON nmsp_child.oid   = child.relnamespace
  ) q
  WHERE", Restriction, ";")

  Results <- dbGetQueryEM(conn, Statement = Statement, display = display, exec = exec)

  if(NROW(Results)) {
    return(Results[, , drop = FALSE])
  } else {
    return(data.frame(list(PARENT_SCHEMA = "", PARENT = "", PARENT_RELKIND = "", PARENT_PART_KEY_DEF = "", PARENT_PART_BOUND = "", INHSEQNO = 0L, CHILD_SCHEMA = "", CHILD = "", CHILD_REL_KIND = "", CHILD_PART_KEY_DEF = "", CHILD_PART_BOUND = ""))[FALSE, , drop = F])
  }

}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}


