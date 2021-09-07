source("sp3_downloader.R")
source("readSP3.R")
source("calculate_azimuth_elevation.R")
#source("cal_el_az.R")
source("skytraq.R")

library(wkb)
library(httr)
library(after)
library(mongolite)

mongo_url = "mongodb://GNSS:GNSS12345@112.137.134.7:27018/GNSS"
mongo.database = mongo("data", url = mongo_url)

start = 1313173197 - 300
end =   1313173197
s = paste(
  '{"time": {"$gt":',
  as.numeric(start) - 1,
  '}, "time":{"$lt": ',
  as.numeric(end) + 1,
  '}}',
  sep = ""
)
data.df = mongo.database$find(query = s)


reciever.coor = c("x" =  -1621816.051830534357578,
                  "y" =  5732436.208981316536665,
                  "z" =  2270380.964045653119683)

# processing on SP3 Observation product file
sp3.downloader()
satellite.coor.df <- splineSP3()


#get newest data from server
r <- GET("http://112.137.134.7:5000/data/last?stationID=pipi")
data <- data.df[1,]
msg = hex2raw(data$data)
satellite.rec.data = skytraq.parseE5(msg = msg)


time = skytraq.parseDF(msg = msg)
time = as.integer(time[1]) * 7 * 24 * 60 * 60 + round(as.numeric(time[2]))
satellite.data = dplyr::filter(satellite.coor.df, TimeStamp >= time - 1 &
                                 TimeStamp < time) # don't know why == return fail =((

function_cal_el_az <- function(x) {
    cbind(calculate_azimuth_elevation(
      as.numeric(x[2]),
      as.numeric(x[3]),
      as.numeric(x[4]),
      reciever.coor[1],
      reciever.coor[2],
      reciever.coor[3]
    ), "ID" =x[5])
}
data.el.az =   data.frame(t(apply(satellite.data, 1, function_cal_el_az)))
colnames(data.el.az) <- c("azimuth", "elevation","distance", "ID")
e5.parsed = skytraq.parseE5(msg)$DataFrame

View(merge(e5.parsed, data.el.az, by.x= "Observation Type", by.y="ID"))
