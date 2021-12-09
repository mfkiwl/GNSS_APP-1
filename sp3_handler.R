##################################################################
#
# This file contain function that download and spline sp3
# satellite product data
#
# Nov 2020 - Nguyen Phuong Bac
#
##################################################################

###############################################################

#' Read SP3 file
#'
#' @param path_input SP3 file path
#' @return A dataframe of satellite position and the corresponding time
readSP3 <- function(path_input = "sp3.fileuz") {
  sp3_p = ""
  file <- file(paste(path_input, sp3_p, sep = ''), open = "r")
  lines_p <-
    strsplit(gsub("\\s+", " ", gsub(
      "^\\s+|\\s+$", "", readLines(file, warn = FALSE)
    )), ' ')
  close(file)
  
  lines_date <-
    lines_p[substr(sapply(lines_p, '[[', 1), 1, 1) == '*']
  annee <- sapply(lines_date, '[[', 2)
  mois <- sapply(lines_date, '[[', 3)
  jour <- sapply(lines_date, '[[', 4)
  heure <- sapply(lines_date, '[[', 5)
  minute <- sapply(lines_date, '[[', 6)
  seconde <- sapply(lines_date, '[[', 7)
  
  ts <-
    mapply(
      FUN =  function(year, month, day, hour, min, sec) {
        return(as.numeric(strptime(
          paste(year, "-", month, "-", day, " ", hour, ":", min, ":", sec, sep = ""),
          format = "%Y-%m-%d %H:%M:%OS",
          tz = "+0"
        )) - 315964782)
      },
      annee,
      mois,
      jour,
      heure,
      minute,
      seconde
    )
  
  lines_sat <-
    lines_p[substr(sapply(lines_p, '[[', 1), 1, 1) == 'P']
  ID <- sapply(lines_sat, '[[', 1)
  ts <- rep(ts, each = length(unique(ID)))
  x <- sapply(lines_sat, '[[', 2)
  y <- sapply(lines_sat, '[[', 3)
  z <- sapply(lines_sat, '[[', 4)
  
  
  return(
    cbind(
      "TimeStamp" = as.numeric(ts),
      "X" = as.numeric(x) * 1000,
      "Y" = as.numeric(y) * 1000,
      "Z" = as.numeric(z) * 1000,
      "ID" = ID
    )
  )
}


library(dplyr)
source("multidimensional_spline.R")

#' Iterpolate GNSS production data
#'
#' @param path_input SP3 file path
#' @return A dataframe of satellite position and the corresponding time each 1s
spline.sp3.par <- function(path_input = "sp3.fileuz",
                           n.core = 8) {
  x = Sys.time()
  sp3.reader.df = readSP3(path_input = path_input)
  list.sat = unique(sp3.reader.df[, "ID"])
  splined.df <- parallel::mclapply(
    list.sat,
    FUN = function(sat, ...) {
      sub.df = which(sp3.reader.df[, "ID"] == sat)
      sub.df = sp3.reader.df[sub.df, ]
      splined.sub.df = spline.Nd.NoPar(sub.df[, c(1, 2, 3, 4)], dimension = ncol(sub.df) - 1, step =
                                         300)
      splined.sub.df[,"ID"] <- sat
      return(splined.sub.df)
    },
    mc.cores = n.core
  ) %>% bind_rows()
  print(Sys.time() -x)
  return(splined.df)
}


#' Download SP3 file from specified link
#'
#' @param url SP3 ftp link
#' @return Ftp url to the downloaded file
sp3.downloader <-
  function(url = "ftp://igs.ign.fr/pub/igs/products/mgex/") {
    wn = (as.numeric(Sys.time()) - 315964782) %/% 60 %/% 60 %/% 24 %/% 7
    url = paste0(url, wn, "/")
    result <-
      RCurl::getURL(
        url,
        verbose = TRUE,
        ftp.use.epsv = FALSE,
        dirlistonly = TRUE,
        crlf = TRUE
      )
    result2 <- paste(url, strsplit(result, "\r*\n")[[1]], sep = "")
    download.file(tail(result2, n = 1),
                  "sp3.file",
                  mode = 'wb',
                  quiet = TRUE)
    system("7z e sp3.file")
    system("rm sp3.file")
    system("mv *SP3 sp3.fileuz")
    return(tail(result2, n = 1))
  }
