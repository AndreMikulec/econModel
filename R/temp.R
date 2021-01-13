#' Add or Remove a Partition
#'
#' From a partitioned table or partitioned index add or remove a partition.
#'
#' @param conn A connection object.
#' @param dbobject A character string specifying if the the type of the parent database object.  This can be "table"(default) or "index".
#' @param name A character string specifying a PostgreSQL already partitioned table or index name. Note, a table can not be converted to a partitioned table and vice-versa.  ALTER has no option.
#' @param partition A character string specifying the name of the partition.
#' @param part.bound.value Of the attaching the partition, the partitions partition bound single value.
#' @param action A character string specifying if the column is to be added ("attach", default) or removed ("drop").
#' @param display Logical. Whether to display the query (defaults to TRUE).
#' @param exec Logical. Whether to execute the query (defaults to TRUE).
#' @returns TRUE if the partition was successfully added or removed.
#' @importFrom tryCatchLog tryCatchLog
#' @importFrom DBI dbQuoteIdentifier
#' @export
dbAttachPartEM <- function (conn, dbobject = "table", name, partition, part.bound.value, action = c("attach", "drop"),
                        display = TRUE, exec = TRUE) {
tryCatchLog::tryCatchLog({

  name <- dbObjectNameFix(conn, o.nm = name)
  nameque <- paste(name, collapse = ".")

  partition <- dbObjectNameFix(conn, o.nm = partition)
  partitionque <- paste(partition, collapse = ".")

  action <- toupper(match.arg(action))
  args <- ifelse(action == "ATTACH", paste0(" FOR VALUES IN (" , part.bound.value , ") "), "")

  tmp.query <- paste0("ALTER ", toupper(dbobject) , " ", nameque, " ", action,
                      " PARTITION ", partitionque, " ", args, ";")
  if (display) {
    message(paste0("Query ", ifelse(exec, "", "not "), "executed:"))
    message(tmp.query)
  }
  if (exec) {
    dbExecuteEM(conn, Statement = tmp.query, display = display, exec = exec)
  }
  if (exec)
    return(TRUE)
}, write.error.dump.folder = getOption("econModel.tryCatchLog.write.error.dump.folder"))}
