install.packages(c("devtools", "roxygen2"))
library(devtools)

devtools::document()
devtools::build()
devtools::load_all()

install_github("james-lebreton/WGA")
data(lq2002, package = "multilevel")
AD(x = lq2002[,c(3)],
   grpid = lq2002$COMPID, scale = c(1,5),
   model = "consensus")
AWG(x = lq2002[,c(3)],
    grpid = lq2002$COMPID, scale = c(1,5),
    model = "consensus")
RWG(x = lq2002[,c(3)],
    grpid = lq2002$COMPID, scale = c(1,5),
    model = "consensus", reset=F)
RWG(x = lq2002[,c(3)],
    grpid = lq2002$COMPID, scale = c(1,5),
    model = "consensus", reset=F)
RWGJ(x = lq2002[,c(3:13)],
         grpid = lq2002$COMPID, scale = c(1,5),
         model = "consensus", reset=F)
RWGJ(x = lq2002[,c(3:13)],
     grpid = lq2002$COMPID, scale = c(1,5),
     model = "consensus", reset=F)
WGA(x = lq2002[,c(3)],
    grpid = lq2002$COMPID, scale = c(1,5),
    model = "consensus", reset = T)
