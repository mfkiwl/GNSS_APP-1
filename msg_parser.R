##################################################################
#
# This file contain function that download and extract Skytraq 
# message and return a dataframe 
#
# please refer AN0039 for bit order https://bitly.com.vn/hs03dh
# Nov 2020 - Nguyen Phuong Bac
#
##################################################################

#some dependances
source("skytraq.R")


#' Download Skytraq message in n second 
#' @param station.id ID of station to get data from
#' @param start.time the begin timestamp to of n second
#' @param n number of seconds to get
#' @return A list of Skytraq message in Hex string
get.data <- function(station.id = "60cc71512c850461b8693d93", start.time, n) {
  request.url = "http://112.137.134.7:5000/data/inTime?"
  request.str = paste0(
    request.url,
    "stationID=",
    station.id,
    "&start=",
    start.time,
    "&time=",
    n,
    "&separate=true"
  )
  
  data.df <- httr::content(httr::GET(request.str))$data
  return(data.df)
}

#' Parse ONE message
#' @param msg message in hex string
#' @param sat.orbit satellite posision in sp3 data
#' @return A dataframe contain satellite azimuth, elevation data for the corresponding message
parse.data.1 <- function(msg, sat.orbit = satellite.product.data) {
  time = skytraq.parseDF(msg = msg)
  time = as.integer(time[1]) * 7 * 24 * 60 * 60 + round(as.numeric(time[2]))
  if(is.na(sat.orbit) || nrow(sat.orbit) <= 0) {
    print("No satellite product data (SP3) found! Please check again")
  }
  satellite.data = dplyr::filter(sat.orbit, TimeStamp >= time - 1 &
                                   TimeStamp < time) # don't know why == return fail =((
  
  function_cal_el_az <- function(x) {
    cbind(
      calculate_azimuth_elevation(
        as.numeric(x[2]),
        as.numeric(x[3]),
        as.numeric(x[4]),
        reciever.coor[1],
        reciever.coor[2],
        reciever.coor[3]
      ),
      "ID" = x[5]
    )
  }
  data.el.az = data.frame(t(apply(satellite.data, 1, function_cal_el_az)))
  colnames(data.el.az) <- c("azimuth", "elevation", "distance", "ID")
  e5.parsed = skytraq.parseE5(msg)$DataFrame
  merge(e5.parsed, data.el.az, by.x = "Observation Type", by.y = "ID") %>%
    tibble::add_column(timestamp = time)
}

#' Parse N message base on one message and export to /Exrtaction folder
#' @param data message in hex string
#' @return 
#' @export satellite.product.data sp3 data
parse.data.n <- function(data) {
  x = Sys.time()
  list_extract_file = list.files(path = "Extraction")
  for (file in list_extract_file) {
    unlink(paste0("Extraction/",file))
  }
  sat.data <-
    dplyr::filter(satellite.product.data,
                  TimeStamp >= (start - 50) &
                    TimeStamp <= (start + 10050))
  data <- lapply(data, wkb::hex2raw)
  data <-
    parallel::mclapply(data, parse.data.1, sat.orbit = sat.data, mc.cores = 8) %>% dplyr::bind_rows()
  PRN <- unique(data$`Observation Type`)
  lapply(
    PRN,
    FUN = function(prn) {
      sig.type = unique(data$`Signal Type`)
      lapply(
        sig.type,
        FUN = function(st) {
          data.on.prn = dplyr::filter(data, `Observation Type` == prn &
                                        `Signal Type` == st)
          if (nrow(data.on.prn>0))
            saveRDS(
              data.on.prn,
              paste0("Extraction/", prn, "_", st, ".RDS")
            )
        }
      )
    }
  )
  print(Sys.time() - x)
}

###############################################################
# END
###############################################################