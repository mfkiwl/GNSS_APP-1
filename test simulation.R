library(leaflet)
source("pos2geo.R")
x =  -1621816.051830534357578
y =  5732436.208981316536665
z =  2270380.964045653119683
h = 7
pos = pos2geo(pos_xyz = c(x, y, z))

sf.map = leaflet() %>%
  addTiles() %>%
  addMarkers(lat = pos[2], lng = pos[1], popup = "QB")

temp.pos = data.frame()
R_0 = 6378137
R_p = 6356752.314245179497563967

source("skytraq.R")
# while (TRUE) {
  library(httr)
  r <- GET("http://112.137.134.7:5000/data/last?stationID=pipi")
  data <- content(r)
  data$data
  # library(dplyr)
  library(wkb)
  msg = hex2raw(data$data)
  df = skytraq.parseE8(msg = msg)
  time= skytraq.parseDF(msg=msg)
  temp_pos = data.frame()
  time_need = time[[1]]*7*24*60*60+round(time[[2]])
  for (i in 1:nrow(df)) {
    coor.sat = subset(out.df, ID==df[i,5] & round(result.1[1])==time_need)
    dist_rp = h / sin(as.numeric(df$Elevation[i]))
    dx = dist_rp * sin(as.numeric(df$Azimuth[i]))
    dy = dist_rp * cos(as.numeric(df$Azimuth[i]))
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
  leaflet() %>% addTiles() %>%
    addMarkers(lng = pos[1], lat = pos[2]) %>%
    addCircleMarkers(lng = Lon, lat = Lat, radius = 1)
  
