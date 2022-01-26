#' AGGREGATE.DATA: aggregate data and merge into combined data frame
#'
#' This function augments the aggregate.R base function.
#'
#' The input consists of a data frame, and the names of the grouping variable and item.
#' The output consists of a data frame containing the group names, the variance within each group, and
#' estimates of rwg based on multiple null response distributions (see LeBreton and Senter, 2008).
#'
#' @param grpid Grouping/clustering variable
#' @param x Items/variables to be aggregated
#' @param data Name of data frame containing group and vars
#' @param method Name of the function used to aggregate data
#' @return
#' @export
#' @examples
#' df <- data.frame(subject = c(1:25),
#'                  grp = c(rep(1,5), rep(2,5),
#'                          rep(3,8), rep(4,3),
#'                          rep(5,4)),
#'                  x1 = c(2, 1, 1, 1, 1,
#'                         1, 2, 3, 4, 5,
#'                         1, 2, 1, 2, 1, 2, 1, 2,
#'                         1, 3, 5,
#'                         2, 3, 3, 4),
#'                  x2 = c(1, 1, 1, 2, 2,
#'                         1, 2, 3, 4, 5,
#'                         2, 2, 2, 2, 1, 1, 1, 1,
#'                         2, 3, 4,
#'                         2, 3, 4, 5))
#'
#' data.aggregation(group = "grp", vars = c("x1", "x2"), data = df, aggr.stat = "mean")
#' data.combined

AGGREGATE.DATA <- function(grpid, x, data, aggr.stat) {
  # create aggregate data frame
  data.aggr <- stats::aggregate(data[,x],
                         by=list(data[,grpid]),
                         FUN=aggr.stat)
  names(data.aggr)[1] = grpid
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
  df.combined <<- merge(data, data.aggr, by = grpid,
                        suffixes = suffix)
}
