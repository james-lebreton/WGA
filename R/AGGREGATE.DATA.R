#' AGGREGATE.DATA: aggregate data and merge into combined data frame
#'
#' This function augments the aggregate.R base function.
#'
#' The input consists of a data frame, and the names of the grouping variable and item.
#' The output consists of a data frame containing the group names, the variance within each group, and
#' estimates of rwg based on multiple null response distributions (see LeBreton and Senter, 2008).
#'
#' @param grpid Name of the grouping/clustering variable
#' @param x Name(s) of the item(s)/variable(s) to be aggregated
#' @param data Name of data frame containing group and vars
#' @param aggr.stat Name of the function used to aggregate data
#' @return Generates two data files; df.aggr is the aggregate-level file & df.combined is the merged file
#' @export
#' @examples
#' data(lq2002, package = "multilevel")
#' AGGREGATE.DATA(grpid = "COMPID", x = c("LEAD", "TSIG", "HOSTILE"),
#'                data = lq2002, aggr.stat = "mean")

AGGREGATE.DATA <- function(grpid, x, data, aggr.stat) {
  # create aggregate data frame
  data.aggr <- stats::aggregate(data[,x],
                                by=list(data[,grpid]),
                                FUN=aggr.stat)
  colnames(data.aggr)[1] = grpid
  if(length(x) == 1)
    colnames(data.aggr)[2] = x
  names(data.aggr)
  if(aggr.stat=="mean") {
    suffix <- c("", ".mn")
  }
  if(aggr.stat=="median") {
    suffix <- c("", ".md")
  }
  if(aggr.stat=="var") {
    suffix <- c("", ".var")
  }
  if(aggr.stat=="sd") {
    suffix <- c("", ".sd")
  }
  if(aggr.stat=="min") {
    suffix <- c("", ".min")
  }
  if(aggr.stat=="max") {
    suffix <- c("", ".max")
  }
  df.combined <- merge(data, data.aggr, by = grpid,
                        suffixes = suffix)
  colnames(data.aggr)[-1] <- paste0(colnames(data.aggr)[-1], suffix[2])
  df.aggr <- data.aggr
  .GlobalEnv$df.combined <- df.combined
  .GlobalEnv$df.aggr <- df.aggr

}
