#' AD: Average Deviation Within-Groups
#'
#' This function estimates Burke, Finkelstein, & Dusig's (1999) within-group agreement statistic
#' for a single item, denoted AD.
#'
#' The input consists of a data frame, and the names of the grouping variable and item.
#' The output consists of a data frame containing the group names, the group means, group medians, and the
#' estimates of AD(j) about means and medians.
#'
#' Link to Burke, Finkelstein, & Dusig (1999):
#'
#' @param grpid Grouping/clustering variable
#' @param x Item on which to estimate AD
#' @param model User-supplied description of multilevel measurement model (e.g., consensus)
#' @param scale Minimum and Maximum Values of the Scale
#' @return Estimates AD
#' @export
#' @examples
#' data(lq2002, package = "multilevel")
#' AD(x = lq2002[ ,c(3:13)], grpid = lq2002$COMPID, scale = c(1,5), model = "consensus")

AD <- function (x, grpid, model, scale)
{
  results.ad.mean <- multilevel::ad.m(x=x, grpid = grpid, type = "mean")
  results.ad.median <- multilevel::ad.m(x=x, grpid = grpid, type = "median")
  results.ad.mean$AD.M <- round(results.ad.mean$AD.M, 2)
  results.ad.median$AD.M <- round(results.ad.median$AD.M, 2)
  scale.points <- scale[2] - scale[1] + 1
  # Combine the two sets of average deviation results
  results.ad <- cbind(results.ad.mean[,c(1,3,2)], results.ad.median[,c(2)])
  names(results.ad) <- c("grp.name", "grp.size", "AD.mean", "AD.median")
  # Combine the remaining sets of output
  output1 <- results.ad
  output1$model <- model
  output1$scale.points <- scale.points
  output1 <- output1[c("grp.name",  "model", "grp.size", "scale.points", "AD.mean", "AD.median")]
  d.mean <- graphics::hist(output1$AD.mean,
               xlab = "AD.mean",
               ylab = "Frequency",
               main = "Distribution of AD.mean")
  d.median <- graphics::hist(output1$AD.median,
               xlab = "AD.median",
               ylab = "Frequency",
               main = "Distribution of AD.median")
  output2 = psych::describe(output1[,c(3,5:ncol(output1))])
  output3 = stats::quantile(output1$AD.mean, probs = c(.00, .10, .20, .30, .40, .50,
                                               .60, .70, .80, .90, 1.00))
  output4 = stats::quantile(output1$AD.median, probs = c(.00, .10, .20, .30, .40, .50,
                                               .60, .70, .80, .90, 1.00))
  output5 <- list(d.mean, d.median)

  return(list(ad.descriptives = output2,
              ad.percentiles = list(ad.mean = output3, ad.median = output4),
              ad.results = output1,
              ad.plots = output5[[]]))
}
