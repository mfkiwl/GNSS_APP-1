#################################################################
# this is the main function that extract and analysis emerpheris#
# data along with process satellite product data                #
#                                                               #
#################################################################


################# CONFIGURATION ########################3

number.data = 300


#########################################################

###############################################################
# INCLUDE LIBS HERE
###############################################################
#source("https://install-github.me/gaborcsardi/after")
library(after)
library(callr)
library(httr)
#library(wkb)
#library(tibble)
library(ggplot2)
#library(dplyr)
#library(parallel)

###############################################################
# INCLUDE OTHER FILEs/ FUNCTIONs HERE
#############.##################################################
source("sp3_handler.R")
source("calculate_azimuth_elevation.R")
source("skytraq.R")
source("msg_parser.R")

###############################################################
# DECLARE GLOBAL VARIABLEs HERE
###############################################################
new.sp3 = F
satellite.product.data = 0
cluster.number = 6

reciever.coor = c("x" = -1845473,
                  "y" = 5828914,
                  "z" = 1810095,
                  "h" = 7)

###############################################################
# HERE TO AUTOMATIC INTERPOLATE PRODUCT DATA AFTER A PERIOD OF
# TIME
###############################################################

iterpolate.product.data <- function() {
  source("sp3_handler.R")
  #sp3.downloader()
  saveRDS(spline.sp3.par(), file = "splined_SP3.RData")
  return(T)
}

func.to.call <- function() {
  new.sp3 <<- r_bg(iterpolate.product.data)
}

# do iterpolate.production.data infinitively after each 6 hour
#task <- after(60*60*1000, func.to.call, args = list(), redo = Inf)

# to cancel, run it below
# after$cancel(task)

###############################################################
# END
###############################################################





###############################################################
# ANALYSIS
temp =list()
###############################################################

analysis <- function() {
  # ###################### read snr #############################
  # prn.data = readRDS(file.path)
  # snr = as.numeric(prn.data$`CNR`)
  # if (length(snr)<100) return(F);
  # date = prn.data$timestamp
  # snr.detrend = detrend_snr(date, snr)
  # temp = data.frame(snr, snr.detrend, date)
  # ggplot(temp) + geom_line(aes(x=date, y = snr, color="red"))+ 
  #                    geom_line(aes(x=date, y = snr.detrend, color="blue"))
  
  ###################### determine f ########################
  list_extract_file = list.files(path = "Extraction")
  if(length(list_extract_file)==0){
    print('No file in /Extraction folder',quote=FALSE); 
  }
  
  #for each file in the extraction folder, we determine f, A, phi
  # for (extract_file in list_extract_file) {
  #   source("determine_f.R");
  #   print(determine_f(file.name = extract_file))
  #   
  # }
  temp <<- append(temp, parallel::mclapply(list_extract_file, determine_f, mc.cores=8))
}

###############################################################
# END
###############################################################

###############################################################
###############################################################
## MAIN FUNCTION HERE
###############################################################
###############################################################
#func.to.call() # fist iterpolate sp3 file once

satellite.product.data <<- readRDS("splined_SP3.RData")

while (T) {
  # update sp3 data
  # print(new.sp3$is_alive())
  # if (!new.sp3$is_alive()) {
  #   print("hi")
  #   if (new.sp3$get_result()) {
  #     satellite.product.data <<- readRDS("splined_SP3.RData")
  #     new.sp3 <<- F
  #   }
  # }
  for (i in 1320771150:(1320771150+300)) {
    x = Sys.time()
    print(paste("start at:", x, i))
    start = i
    data.df <- rev(get.data(start.time = start, n = number.data))
    parse.data.n(data = data.df)
    analysis()
    print(paste("End in: ", Sys.time() - x, "secs"))
  }
}

###############################################################
###############################################################
# END OF MAIN FUNCTION
###############################################################
###############################################################
