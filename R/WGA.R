#' WGA: Within-Group Agreement
#'
#'  This function computes multiple estimates of withing-group agreement including:
#'  James, Demaree, & Wolf's (1984) RWG(J), Burke, Finkelstein, & Dusig's (1999) AD(J),
#'  and Brown & Hauenstein's (2005) AWG(J).
#'  Estimates of RWG(J) are computed for multiple null distributions.
#'
#' The input consists of a data frame, the names of the grouping variable, and the
#' names of items.  The output consists of a data frame containing the group names,
#' group sample sizes, the variance within each group, and
#' the various estimates of agreement.
#'
#' @param x A vector representing a single item or a matrix representing a scale of interest.
#'          If a matrix, each column of the matrix represents a scale item, and each row
#'          represents an individual respondent.
#' @param grpid A vector identifying the groups from which x originated.
#' @param scale A vector with the lower and upper response options (e.g., c(1,5))
#'              for a five-point scale from strongly disagree to strongly agree.
#' @param model User-supplied description of multilevel measurement model (e.g., consensus)
#' @param reset Logical option for handling negative estimates of RWG;
#'              FALSE retains negative values; TRUE resets values to 0
#' @return Estimates of within-group agreement
#' @export
#' @examples
#' jdw84 <- read.csv("data/jdw84.csv")
#' wga.J(data=jdw84, group = "group", items = c("x1", "x2", "x3"))

WGA <- function(x, grpid, scale, model, reset = F) {
  source("R/RWG.R")
  source("R/AWG.R")
  # Run the RWG function & Extract the Group-Level Results
  output.rwg <- RWG(x=x, grpid = grpid, model = model, scale = scale, reset = reset)
  results.rwg <- output.rwg$rwg.results

  # Run the AWG function & extract the group-level results
  output.awg <- AWG(x=x, grpid = grpid, scale = scale, model = model)
  results.awg <- output.awg$awg.results

  # Run Bliese's ad.m function from his multilevel package
  # And, extract the group-level results
  results.ad.mean <- ad.m(x=x, grpid = grpid, type = "mean")
  results.ad.median <- ad.m(x=x, grpid = grpid, type = "median")
  results.ad.mean$AD.M <- round(results.ad.mean$AD.M, 2)
  results.ad.median$AD.M <- round(results.ad.median$AD.M, 2)

  # Combine the two sets of average deviation results
  results.ad <- cbind(results.ad.mean[,c(1,3,2)], results.ad.median[,c(2)])
  names(results.ad) <- c("grp.name", "grp.size", "AD.mean", "AD.median")

  # Combine the remaining sets of output
  output1 <- cbind(results.ad[,], results.awg[,])
  output1<- cbind(output1[,], results.rwg[,])
  output1 <- output1[,c("grp.name", "grp.size", "aggr.model", "num.items", "item.var",
                        "rwg.un", "rwg.ss", "rwg.ms", "rwg.hs", "rwg.tri", "rwg.nor",
                        "awg", "AD.mean", "AD.median")]
  output2 <- psych::describe(output1[,c(2, 4:ncol(output1))])
  output3 <- output.rwg$rwg.out.of.bounds
  output4 <- output.rwg$rwg.error.variances
  output5 <- list(output.rwg$rwg.plots[[]], output.rwg$awg.plots[[]])
  output6 <- list("rwg.un" = output.rwg$rwg.un.percentiles,
                  "awg.un" = output.awg$awg.percentiles)
  return(list(data.aggreation.model = model,
              wga.descriptives = output2,
              rwg.out.of.bounds = output3,
              rwg.error.variances = output4,
              wga.percentiles = output6,
              wga.results = output1,
              wga.plots = output5))
}

