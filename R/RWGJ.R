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
#' @param grpid Grouping/clustering variable
#' @param x Items to use when estimating RWGJ
#' @param model User-supplied description of multilevel measurement model (e.g., consensus)
#' @param scale Number of scale points ranging from 5 to 11
#' @param reset Logical option for handling negative estimates of RWG;
#'              FALSE retains negative values; TRUE resets values to 0
#' @return List containing a matrix of the null error values, estimates of RWG, and
#'         summary of how many negative values were obtained
#' @export
#' @examples
#' data(lq2002, package = "multilevel")
#' RWGJ(x = lq2002[,c(3:13)], grpid = lq2002$COMPID, scale = 5, reset = F)
#'

RWGJ <- function (x, grpid, model, scale, reset=F){
  df.all <- data.frame(grpid,x)
  df.all <- na.exclude(df.all)
  df.grp <- split(df.all[, 2:(ncol(df.all))], df.all$grpid)
  number.of.groups <- length(unique(df.all$grpid))
  grp.name <- unique(df.all$grpid)
  grp.size <- unlist(lapply(df.grp, nrow))
  scale.points <- scale[2] - scale[1] + 1
  null.var <- as.data.frame(matrix(c( 5,	 2.00,	1.34,	0.90,	0.44,	1.32,	1.04,
                                      6,	 2.92,	1.85,	1.26,	0.69,	1.45,	1.25,
                                      7,	 4.00,	2.90,	2.14,	1.39,	2.10,	1.40,
                                      8,	 5.25,	3.47,	2.79,	2.35,	2.81,	1.73,
                                      9,	 6.67,	5.66,	4.73,	3.16,	3.00,	1.58,
                                      10,   8.25,	6.30,	5.09,	3.46,	2.89,	1.45,
                                      11,	10.00,	7.31,	6.32,	4.02,	3.32,	1.40),
                                   ncol=7, nrow=7, byrow=T,
                                   dimnames = list(rownames = NULL,
                                                   colnames = c("scale.points", "uni", "ss",
                                                                "ms", "hs", "tri", "nor"))))
  J <- ncol(x)
  mn.var <- lapply(df.grp, function(Q) {
    if (nrow(Q) > 1) {
      S.mn <- mean(apply(Q, 2, var, na.rm = T))
      S.mn
      }
    else {
      S.mn <- NA
      S.mn
    }
  })
  mn.var <- unlist(mn.var)
  output1 <- data.frame(grp.name = grp.name,
                        grp.size = grp.size,
                        aggr.model = model,
                        num.items = J,
                        mean.item.var=round(mn.var,2))
  output1$rwgj.un <- round((J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 2]))/
                             (J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 2]) +
                                mn.var/null.var[which(null.var$scale.points == scale.points), 2]),2)
  output1$rwgj.ss <- round((J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 3]))/
                             (J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 3]) +
                                mn.var/null.var[which(null.var$scale.points == scale.points), 3]),2)
  output1$rwgj.ms <- round((J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 4]))/
                             (J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 4]) +
                                mn.var/null.var[which(null.var$scale.points == scale.points), 4]),2)
  output1$rwgj.hs <- round((J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 5]))/
                             (J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 5]) +
                                mn.var/null.var[which(null.var$scale.points == scale.points), 5]),2)
  output1$rwgj.tri <- round((J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 6]))/
                              (J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 6]) +
                                 mn.var/null.var[which(null.var$scale.points == scale.points), 6]),2)
  output1$rwgj.nor <- round((J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 7]))/
                              (J*(1-mn.var/null.var[which(null.var$scale.points == scale.points), 7]) +
                                 mn.var/null.var[which(null.var$scale.points == scale.points), 7]),2)
  num.oor.un <- sum(output1[,5] < 0 | output1[,5] > 1)
  num.oor.ss <- sum(output1[,6] < 0 | output1[,6] > 1)
  num.oor.ms <- sum(output1[,7] < 0 | output1[,7] > 1)
  num.oor.hs <- sum(output1[,8] < 0 | output1[,8] > 1)
  num.oor.tri <- sum(output1[,9] < 0 | output1[,9] > 1)
  num.oor.nor<- sum(output1[,10] < 0 | output1[,10] > 1)
  output2 <- data.frame(num.oor.un = num.oor.un,
                        num.oor.ss = num.oor.ss,
                        num.oor.ms = num.oor.ms,
                        num.oor.hs = num.oor.hs,
                        num.oor.tri = num.oor.tri,
                        num.oor.nor = num.oor.nor)
  if(reset ==  F) {
    output1 <- output1
    output2$reset.to.zero = "No"
    }
  else
    {
      output1[,-c(1:4)][output1[, -c(1:4)] < 0] <- 0
      output1[,-c(1:4)][output1[, -c(1:4)] > 1] <- 0
      output2$reset.to.zero = "Yes"
      }
   d.un <- hist(output1$rwgj.un, xlab = "RWG(J)", ylab = "Frequency",
                main = "Distribution of RWG(J) \n Using Uniform Null")
   d.ss <- hist(output1$rwgj.ss, xlab = "RWG(J)", ylab = "Frequency",
                main = "Distribution of RWG(J) \n Using Slightly Skewed Null")
   d.ms <- hist(output1$rwgj.ms, xlab = "RWG(J)", ylab = "Frequency",
                main = "Distribution of RWG(J) \n Using Moderately Skewed Null")
   d.hs <- hist(output1$rwgj.hs, xlab = "RWG(J)", ylab = "Frequency",
                main = "Distribution of RWG(J) \n Using Heavily Skewed Null")
  d.tri <- hist(output1$rwgj.tri, xlab = "RWG(J)", ylab = "Frequency",
                main = "Distribution of RWG(J) \n Using Triangular Null")
  d.nor <- hist(output1$rwgj.nor, xlab = "RWG(J)", ylab = "Frequency",
                main = "Distribution of RWG(J) \n Using Normal Null")
  output3 <- list(d.un, d.ss, d.ms, d.hs, d.tri, d.nor)
  output4 = psych::describe(output1[,c(2,4:ncol(output1))])
  output5 = quantile(output1$rwgj.un, probs = c(.00, .10, .20, .30, .40, .50,
                                               .60, .70, .80, .90, 1.00))
  return(list(rwgj.descriptives = output4,
              rwgj.un.percentiles = output5,
              rwgj.out.of.bounds = output2,
              rwgj.error.variances = null.var[which(null.var == scale.points),],
              rwgj.results = output1,
              rwgj.plots = output3[[]]))
}
