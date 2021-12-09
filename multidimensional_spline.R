##################################################################
#
#  Function to spline in parallel seperate dimension
#
# Input :
#   - x : data which need spline
#   - dimension: number of dimension
#   - step: the number of part which will be divided into
# Output :
#   - splined data
#
#   Nov 2021 - Nguyen Phuong Bac
#
##################################################################

spline.Nd.Par <- function(x, dimension, step) {
  library(foreach)
  library(doParallel)
  cl <- makeCluster(6)
  registerDoParallel(cl)
  spline.df <-
    foreach (loop = 1:dimension, .combine = cbind) %dopar% {
      spline(as.numeric(x[, loop]), n = (nrow(x) - 1) * step + 1)$y
    }
  stopCluster(cl)
  colnames(spline.df) <- colnames(x)
  return(as.data.frame(spline.df))
}

spline.Nd.NoPar <- function(x, dimension, step) {
  spline.df <- apply(x, 2, function(pts) {
    spline(pts, n = (length(pts) - 1) * step + 1)$y
  })
  colnames(spline.df) <- colnames(x)
  return(as.data.frame(spline.df))
}
