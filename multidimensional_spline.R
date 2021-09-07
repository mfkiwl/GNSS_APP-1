library(foreach)
library(doParallel)

splineNd <- function(x, dimension, step) {
  cl <- makeCluster(4)
  registerDoParallel(cl)
  spline.df <- foreach (loop = 1:dimension, .combine = cbind) %dopar%{
    spline(as.numeric(x[,loop]), n = (nrow(x)-1)*step+1)$y
  }
  stopCluster(cl)
  colnames(spline.df) <- colnames(x)
  x = return(as.data.frame(spline.df))
}

