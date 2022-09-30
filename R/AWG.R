#' AWG: Agreement Within-Groups
#'
#' This function estimates Brown & Hauenstein's (2005) within-group agreement statistic.
#' For a single item the statistic is denoted awg. For a scaled based on J items, the
#' statistic is denoted awg.j.
#'
#' The input consists of the item(s), the grouping or clustering variable, a description
#' of the multilevel measurement model used to aggregate the data (e.g., additive, consensus),
#' and the low and high values of the response scale.
#'
#' The output consists of a list containing: a) descriptive statistics summarizing the results
#' across all of the groups, b) a data frame containing the group names, group sizes, and
#' the group-level estimates of awg/awg.j, and c) a histogram plotting the distribution of
#' awg/awg.j values.
#'
#' Link to Brown and Hauenstein:
#'
#' @param x     Either a vector representing a single item used to estimate awg or a matrix
#'              representing a set of items used to estimate awg.j.
#' @param grpid Grouping/clustering variable
#' @param model User-supplied description of multilevel measurement model (e.g., consensus)
#' @param scale A vector containing the lowest and highest response options on the scale.
#' @return      List containing a matrix of the null error values, estimates of RWG, and
#'              summary of how many negative values were obtained
#' @export
#' @examples
#' data(lq2002, package = "multilevel")
#' AWG(x=lq2002[,c(3)], grpid = lq2002$COMPID, model = "Consensus", scale=c(1,5))

AWG <- function(x, grpid, model, scale){
  df <- data.frame(grpid = grpid, x)
  df <- stats::na.exclude(df)
  num.items <- ncol(as.matrix(x))
  df.grp <- split(df[, 2:ncol(df)], df$grpid)
  if (ncol(as.matrix(x)) > 1) {
    awg.j <- lapply(df.grp, function(Q) {
      if (nrow(Q) > 1) {
        mean(apply(Q, 2, function(AW) {
          H <- scale[2]
          L <- scale[1]
          M <- mean(AW)
          k <- length(AW)
          awg <- 1 - ((2 * stats::var(AW))/(((H + L) * M - (M^2) - (H * L)) * (k/(k - 1))))
          if (M < ((L * (k - 1) + H)/k))
            awg <- NA
          if (M > ((H * (k - 1) + L)/k))
            awg <- NA
          if (M == H | M == L)
            awg = 1
          awg}), na.rm = T)
        }
      else
        {
          NA
          }
      })
    grp.size <- lapply(df.grp, nrow)
    mean.item.var <- lapply(lapply(df.grp, stats::var, na.rm=T), mean, na.rm = T)
    awg.j <- unlist(awg.j)
    grp.name <- names(df.grp)
    grp.size <- unlist(grp.size)
    mean.item.var <- unlist(mean.item.var)
    output1 <- data.frame(grp.name = grp.name,
                          grp.size = grp.size,
                          aggr.model = model,
                          num.items = num.items,
                          mean.item.var = round(mean.item.var,2),
                          awg.j = round(awg.j,2))
    output2 <- psych::describe(output1[,c(2,4:ncol(output1))])
    output3 <- graphics::hist(output1$awg.j,
                    xlab = "AWG(J)",
                    ylab = "Frequency",
                    main = "Distribution of AWG(J)")
    output4 <- stats::quantile(output1$awg.j,probs = c(.00, .10, .20, .30, .40, .50,
                                                .60, .70, .80, .90, 1.00))
    return(list(awgj.descriptives = output2,
                awgj.percentiles = output4,
                awgj.results = output1,
                awgj.plot = output3[[]]))
    stop()
    }
  awg.1 <- lapply(df.grp, function(AW) {
    H <- scale[2]
    L <- scale[1]
    M <- mean(AW)
    k <- length(AW)
    awg <- 1 - ((2 * stats::var(AW))/(((H + L) * M - (M^2) - (H * L)) * (k/(k - 1))))
    if (M < ((L * (k - 1) + H)/k))
      awg <- NA
    if (M > ((H * (k - 1) + L)/k))
      awg <- NA
    if (M == H | M == L)
      awg.1 = 1
    awg
    })
  grp.size <- lapply(df.grp, length)
  grp.name <- names(df.grp)
  item.var <- lapply(df.grp, stats::var)
  awg.1 <- unlist(awg.1)
  grp.size <- unlist(grp.size)
  item.var <- unlist(item.var)
  awg.1[grp.size == 1] <- NA
  output1 <- data.frame(grp.name = grp.name,
                        grp.size = grp.size,
                        aggr.model = model,
                        num.items = num.items,
                        item.var = round(item.var,2),
                        awg = round(awg.1,2))
  output2 <- psych::describe(output1[,c(2,4:ncol(output1))])
  output3 <- graphics::hist(output1$awg,
                  xlab = "AWG",
                  ylab = "Frequency",
                  main = "Distribution of AWG")
  output4 <- stats::quantile(output1$awg,probs = c(.00, .10, .20, .30, .40, .50,
                                              .60, .70, .80, .90, 1.00),
                      na.rm = T)
  return(list(awg.descriptives = output2,
              awg.percentiles = output4,
              awg.results = output1,
              awg.plot = output3[[]]))
}

