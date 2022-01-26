# 2022-01-25
# James creating package to post to github.
# 1. Create new repo online at github.com

# 2. Follow instructions from github.com
### basically, just copy & paste the code from
###   github.com into the shell -- "RStudio --> tools --> shell"
install.packages("multilevel")
install.packages("psych")
library(multilevel)
data(lq2002, package = "multilevel")
WGA(x = lq2002[,c("LEAD01")], grpid = lq2002$COMPID, scale = c(1,5),
               model = "consensus", reset = T)
