#################################################################
# this is the main function that extract and analysis emerpheris#
# data along with process satellite product data                #
#                                                               #
#################################################################


###############################################################
# INCLUDE LIBS HERE 
###############################################################
#source("https://install-github.me/gaborcsardi/after")
library(after)
library(callr)
###############################################################
# INCLUDE OTHER FILEs/ FUNCTIONs HERE
###############################################################
source("sp3_handler.R")
source("calculate_azimuth_elevation.R")
source("skytraq.R")

###############################################################
# DECLARE GLOBAL VARIABLEs HERE
###############################################################
# time.start = 
new.sp3 = F
satellite.product.data = 0

mongo_url = "mongodb://GNSS:GNSS12345@112.137.134.7:27018/GNSS"
mongo.database = mongo("data", url = mongo_url)

reciever.coor = c("x" =  -1621816.051830534357578,
                  "y" =  5732436.208981316536665,
                  "z" =  2270380.964045653119683)
###############################################################
# HERE TO AUTOMATIC INTERPOLATE PRODUCT DATA AFTER A PERIOD OF
# TIME
###############################################################

iterpolate.product.data <- function() {
  source("sp3_handler.R")
  #sp3.downloader()
  saveRDS(spline.sp3(), file="splined_SP3.RData")
  return(T)
}
reset.new.sp3 <- function() {
  return(F)
}
func.to.call <- function() {
  new.sp3 <<- r_bg(iterpolate.product.data)
}

# do iterpolate.production.data infinitively after each 6 hour
task <- after(15*60*1000, func.to.call, args = list(), redo = Inf)

# to cancel, run it below
# after$cancel(task) 
###############################################################
# END
###############################################################


###############################################################
###############################################################
## MAIN FUNCTION HERE 
###############################################################
###############################################################

while (T) {
  # update sp3 data
  print(new.sp3$is_alive())
  if(!new.sp3$is_alive()) {
    print("hi")
    if (new.sp3$get_result() ) {
    # satellite.product.data <<- readRDS("splined_SP3.RData")
      new.sp3 <<- r_bg(reset.new.sp3)
    }
  }
  # querry for emerpheris data
  # s = paste(
  #   '{"time": {"$gt":',
  #   as.numeric(start) - 1,
  #   '}, "time":{"$lt": ',
  #   as.numeric(end) + 1,
  #   '}}',
  #   sep = ""
  # )
  # data.df = mongo.database$find(query = s)  
  Sys.sleep(3)
  
}


###############################################################
###############################################################
# END OF MAIN FUNCTION
###############################################################
###############################################################
