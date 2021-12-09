library(leaflet)
source("pos2geo.R")
x =  -1845473
y =  5828914
z =  1810095
h = 10
pos = pos2geo(pos_xyz = c(x, y, z))
reciever.coor = c(x, y,z, h)
sf.map = leaflet() %>%
  addTiles() %>%
  addMarkers(lat = pos[2], lng = pos[1], popup = "QB")

temp.pos = data.frame()
R_0 = 6378137
R_p = 6356752.314245179497563967

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
satellite.coor.df <<- readRDS("splined_SP3.RData")

source("calculate_azimuth_elevation.R")
source("skytraq.R")
while (TRUE) {
  library(httr)
  r <- GET("http://112.137.134.7:5000/data/last?stationID=60c753f02c850461b8693d92")
  data <- content(r)
  data$data
  # library(dplyr)
  library(wkb)
  msg = hex2raw(data$data)
  df = skytraq.parseE8(msg = msg)
  time= skytraq.parseDF(msg=msg)
  temp_pos = data.frame()
  
  time_need = time[[1]]*7*24*60*60+round(time[[2]])
  satellite.data = dplyr::filter(satellite.coor.df,
                                 TimeStamp >= time_need - 1 &
                                   TimeStamp < time_need) # don't know why == return fail =((
  
  data.el.az =   data.frame(t(apply(satellite.data, 1, function_cal_el_az)))
  colnames(data.el.az) <- c("azimuth", "elevation","distance", "ID")
  
  for (i in 1:nrow(data.el.az)) {
    dist_rp = h / sin(as.numeric(data.el.az$elevation[i]))
    dx = dist_rp * sin(as.numeric(data.el.az$azimuth[i]))
    dy = dist_rp * cos(as.numeric(data.el.az$azimuth[i]))
    dLon = dx / (111320 * cos(pos[2]))
    dLat = dy / 110540
    lat_rp = pos[2] + dLat
    lon_rp = pos[1] + dLon
    h_rp = pos[3] - h
    temp.pos = rbind(temp.pos, c(lon_rp, lat_rp, h_rp))
  }
  colnames(temp.pos) <- c("long", "lat", "height")
  Lon = temp.pos[, 1]
  Lat = temp.pos[, 2]
  leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>%
    setView(lng = pos[1], lat= pos[2], zoom = 17) %>%
    addMarkers(lng = pos[1], lat = pos[2]) %>%
    addCircleMarkers(lng = Lon, lat = Lat, radius = 1)
  print(time_need)
  Sys.sleep(5)
}
