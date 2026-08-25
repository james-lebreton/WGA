#' RWGJ: Within-Group Agreement (multiple items)
#'
#' This function estimates James, Demaree, & Wolf's (1984) within-group agreement statistic
#' for a multi-item scale, denoted RWGJ.
#'
#' The input consists of a data frame, and the names of the grouping variable and item.
#' The output consists of a data frame containing the group names, the variance within each group, and
#' estimates of rwg based on multiple null response distributions (see LeBreton and Senter, 2008).
#'
#' Link to James, Demaree, & Wolf (1984):
#' Link to LeBreton & Senter (2008):
#' Link to LeBreton, Moeller, & Senter (2021):
#'
#' @param x Items to use when estimating RWGJ
#' @param grpid Grouping/clustering variable
#' @param scale Number of scale points ranging from 5 to 11
#' @param model User-supplied description of multilevel measurement model (e.g., consensus)
#' @param reset Logical option for handling negative estimates of RWG; FALSE retains negative values; TRUE resets values to 0
#' @param cutoff User-supplied cutoff value for justifying data aggregation
#' @return List containing results of analysis.
#' @export
#' @examples
#' data(lq2002, package = "multilevel")
#' RWGJ(x = lq2002[,c(3:13)], grpid = lq2002$COMPID, model = "consensus",
#'      scale = c(1,5), reset = TRUE, cutoff=0.50)
      
RWGJ <- function (x, grpid, scale, model, reset = FALSE, cutoff) {
  df.all <- data.frame(grpid, x)
  df.all <- stats::na.exclude(df.all)
  df.grp <- split(df.all[, 2:ncol(df.all)], df.all$grpid)
  J <- ncol(x)
  
  grp.name <- unique(df.all$grpid)
  grp.size <- vapply(df.grp, nrow, integer(1))
  scale.points <- scale[2] - scale[1] + 1
  null.var <- as.data.frame(matrix(c( 5, 2, 1.34, 0.9, 0.44, 1.32, 1.04,
                                      6, 2.92, 1.85, 1.26, 0.69, 1.45, 1.25,
                                      7, 4, 2.9, 2.14, 1.39, 2.1, 1.4,
                                      8, 5.25, 3.47, 2.79, 2.35, 2.81, 1.73,
                                      9, 6.67, 5.66, 4.73, 3.16, 3, 1.58,
                                      10, 8.25, 6.3, 5.09, 3.46, 2.89, 1.45,
                                      11, 10, 7.31, 6.32, 4.02, 3.32, 1.4),
                                   ncol = 7, byrow = TRUE,
                                   dimnames = list(NULL, c("scale.points","uni","ss","ms","hs","tri","nor"))))
  
  # mean of item variances within each group
  mn.var <- vapply(df.grp, function(Q) {
    if (nrow(Q) > 1) mean(apply(Q, 2, stats::var, na.rm = TRUE)) else NA_real_
  }, numeric(1))
  
  nv_row <- which(null.var$scale.points == scale.points)
  
  output1 <- data.frame(
    grp.name = grp.name,
    grp.size = grp.size,
    aggr.model = model,
    num.items = J,
    mean.item.var = round(mn.var, 2),
    rwgj.un  = round((J * (1 - mn.var / null.var[nv_row, "uni"])) /
                       (J * (1 - mn.var / null.var[nv_row, "uni"]) + mn.var / null.var[nv_row, "uni"]), 2),
    rwgj.ss  = round((J * (1 - mn.var / null.var[nv_row, "ss"])) /
                       (J * (1 - mn.var / null.var[nv_row, "ss"]) + mn.var / null.var[nv_row, "ss"]), 2),
    rwgj.ms  = round((J * (1 - mn.var / null.var[nv_row, "ms"])) /
                       (J * (1 - mn.var / null.var[nv_row, "ms"]) + mn.var / null.var[nv_row, "ms"]), 2),
    rwgj.hs  = round((J * (1 - mn.var / null.var[nv_row, "hs"])) /
                       (J * (1 - mn.var / null.var[nv_row, "hs"]) + mn.var / null.var[nv_row, "hs"]), 2),
    rwgj.tri = round((J * (1 - mn.var / null.var[nv_row, "tri"])) /
                       (J * (1 - mn.var / null.var[nv_row, "tri"]) + mn.var / null.var[nv_row, "tri"]), 2),
    rwgj.nor = round((J * (1 - mn.var / null.var[nv_row, "nor"])) /
                       (J * (1 - mn.var / null.var[nv_row, "nor"]) + mn.var / null.var[nv_row, "nor"]), 2)
  )
  
  # count out-of-range RWG(J) by name (safer than numeric columns)
  oor <- function(v) sum(v < 0 | v > 1, na.rm = TRUE)
  output2 <- data.frame(
    num.oor.un  = oor(output1$rwgj.un),
    num.oor.ss  = oor(output1$rwgj.ss),
    num.oor.ms  = oor(output1$rwgj.ms),
    num.oor.hs  = oor(output1$rwgj.hs),
    num.oor.tri = oor(output1$rwgj.tri),
    num.oor.nor = oor(output1$rwgj.nor),
    reset.to.zero = if (isTRUE(reset)) "Yes" else "No"
  )
  
  if (reset==TRUE) {
    rwg_cols <- c("rwgj.un","rwgj.ss","rwgj.ms","rwgj.hs","rwgj.tri","rwgj.nor")
    output1[rwg_cols] <- lapply(output1[rwg_cols], function(v) {
      v[v < 0 | v > 1] <- 0
      v
    })
  }
  
  # hist() returns "histogram" objects (and plots). Keep the objects in a list.
  d.un  <- graphics::hist(output1$rwgj.un,  xlab="RWG(J)", ylab="Frequency",
                          main="Distribution of RWG(J)\nUsing Uniform Null")
  d.ss  <- graphics::hist(output1$rwgj.ss,  xlab="RWG(J)", ylab="Frequency",
                          main="Distribution of RWG(J)\nUsing Slightly Skewed Null")
  d.ms  <- graphics::hist(output1$rwgj.ms,  xlab="RWG(J)", ylab="Frequency",
                          main="Distribution of RWG(J)\nUsing Moderately Skewed Null")
  d.hs  <- graphics::hist(output1$rwgj.hs,  xlab="RWG(J)", ylab="Frequency",
                          main="Distribution of RWG(J)\nUsing Heavily Skewed Null")
  d.tri <- graphics::hist(output1$rwgj.tri, xlab="RWG(J)", ylab="Frequency",
                          main="Distribution of RWG(J)\nUsing Triangular Null")
  d.nor <- graphics::hist(output1$rwgj.nor, xlab="RWG(J)", ylab="Frequency",
                          main="Distribution of RWG(J)\nUsing Normal Null")
  output3 <- list(un = d.un, ss = d.ss, ms = d.ms, hs = d.hs, tri = d.tri, nor = d.nor)
  invisible(lapply(output3, plot))
  output4 <- psych::describe(output1[, c("grp.size", "num.items",
                                         "mean.item.var","rwgj.un","rwgj.ss",
                                         "rwgj.ms","rwgj.hs","rwgj.tri","rwgj.nor")])
  
  qtiles <- c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1)
  output5 <- list(
    rwgj.un  = stats::quantile(output1$rwgj.un,  probs = qtiles, na.rm = TRUE),
    rwgj.ss  = stats::quantile(output1$rwgj.ss,  probs = qtiles, na.rm = TRUE),
    rwgj.ms  = stats::quantile(output1$rwgj.ms,  probs = qtiles, na.rm = TRUE),
    rwgj.hs  = stats::quantile(output1$rwgj.hs,  probs = qtiles, na.rm = TRUE),
    rwgj.tri = stats::quantile(output1$rwgj.tri, probs = qtiles, na.rm = TRUE),
    rwgj.nor = stats::quantile(output1$rwgj.nor, probs = qtiles, na.rm = TRUE)
  )
  
  output6 <- list(
    rwgj.un.cutoff  = round(mean(output1$rwgj.un  >= cutoff, na.rm = TRUE), 2),
    rwgj.ss.cutoff  = round(mean(output1$rwgj.ss  >= cutoff, na.rm = TRUE), 2),
    rwgj.ms.cutoff  = round(mean(output1$rwgj.ms  >= cutoff, na.rm = TRUE), 2),
    rwgj.hs.cutoff  = round(mean(output1$rwgj.hs  >= cutoff, na.rm = TRUE), 2),
    rwgj.tri.cutoff = round(mean(output1$rwgj.tri >= cutoff, na.rm = TRUE), 2),
    rwgj.nor.cutoff = round(mean(output1$rwgj.nor >= cutoff, na.rm = TRUE), 2)
  )
  
  return(list(
    rwgj.descriptives   = output4,
    rwgj.over.cutoff    = output6,
    rwgj.percentiles    = output5,
    rwgj.out.of.bounds  = output2,
    rwgj.error.variances= null.var[which(null.var$scale.points == scale.points), ],
    rwgj.results        = output1,
    rwgj.plots          = output3      # <- return the whole list (no [[]])
  ))
}


