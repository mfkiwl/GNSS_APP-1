###############################################################
# some function that needed 
###############################################################
dublicate <- function(a, n){
  x = NA
  for(i in (1:length(a))){
    for(j in (1:n)){
      x[(i-1)*n +j ]= a[i]
    }
  }
  return(x)
}

to.GPS.timestamp <- function(year, month, day, hour, min, sec) {
  return(as.numeric(strptime(paste(year, "-", month, "-", day, " ", hour, ":", min, ":", sec, sep = ""),format="%Y-%m-%d %H:%M:%OS",tz = "+0")) - 315964782)
}

###############################################################
# function to read sp3 file (production data)
###############################################################

readSP3 <- function(path_input = "sp3.fileuz") {
  sp3_p = ""
  file <- file(paste(path_input, sp3_p, sep = ''), open = "r")
  lines_p <-
    strsplit(gsub("\\s+", " ", gsub(
      "^\\s+|\\s+$", "", readLines(file, warn = FALSE)
    )), ' ')
  close(file)
  
  lines_date <- lines_p[substr(sapply(lines_p,'[[',1),1,1)=='*']
  annee <- sapply(lines_date,'[[', 2)
  mois <- sapply(lines_date,'[[',3)
  jour <- sapply(lines_date,'[[',4)
  heure <- sapply(lines_date,'[[',5)
  minute <- sapply(lines_date,'[[',6)
  seconde <- sapply(lines_date,'[[',7)
  
  ts <- mapply(to.GPS.timestamp, annee, mois, jour, heure, minute, seconde)
  
  lines_sat <- lines_p[substr(sapply(lines_p,'[[',1),1,1)=='P']
  ID <- sapply(lines_sat,'[[', 1)
  ts <- dublicate(ts, length(unique(ID)))
  x <- sapply(lines_sat,'[[', 2) 
  y <- sapply(lines_sat,'[[', 3) 
  z <- sapply(lines_sat, '[[', 4)
  
  
  return(cbind("TimeStamp" = as.numeric(ts), "X"= as.numeric(x) * 1000, "Y" = as.numeric(y)*1000, "Z" = as.numeric(z)*1000, "ID" = ID))
}

###############################################################
# function to iterpolate GNSS production data
###############################################################

library(dplyr)
source("multidimensional_spline.R")

spline.sp3 <- function(path_input = "sp3.fileuz") {
  sp3.reader.df = readSP3(path_input = path_input)
  list.sat = unique(sp3.reader.df[,"ID"])
  splined.df = data.frame()
  for (sat in list.sat){
    sub.df = which(sp3.reader.df[,"ID"] == sat)
    sub.df = sp3.reader.df[sub.df,]
    splined.sub.df = splineNd(sub.df[,c(1,2,3,4)], dimension=ncol(sub.df) - 1, step =300)
    splined.sub.df$ID <- sat
    splined.df <- rbind(splined.df, splined.sub.df)
  }
  colnames(splined.df) <- c( "TimeStamp", "X", "Y", "Z", "ID")
  return(as.data.frame(splined.df))
}

###############################################################
# function to download sp3
###############################################################

library(RCurl)
sp3.downloader <- function(url = "ftp://igs.ign.fr/pub/igs/products/mgex/") {
  wn = (as.numeric(Sys.time())-315964782) %/% 60 %/% 60 %/% 24 %/% 7
  url = paste(url, wn,"/", sep="")
  result <- getURL(url,verbose=TRUE,ftp.use.epsv=FALSE, dirlistonly = TRUE, crlf = TRUE)
  result2 <- paste(url, strsplit(result, "\r*\n")[[1]], sep = "")
  download.file(tail(result2, n=1),"sp3.file",mode='wb',quiet=TRUE)
  system("7z e sp3.file")
  system("rm sp3.file")
  system("mv *SP3 sp3.fileuz")
  return(tail(result2, n=1))
}

