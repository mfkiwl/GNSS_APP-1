#source("https://install-github.me/gaborcsardi/after")
library(after)

# source("sp3_downloader.R")
# source("readSP3.R")

a = 1

handle <- function() {
  a <<- a+1;
  print(a)
}

task <- after(60*60*1000, handle, args = list(), redo = Inf)


 # cancel
after$cancel(task)

save(a, file = "slined_SP3.RData")
