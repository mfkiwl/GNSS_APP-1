#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(mapview)
#source("https://install-github.me/gaborcsardi/after")
library(after)

source("skytraq.R")
source("sp3_handler.R")
source("calculate_azimuth_elevation.R")
source("pos2geo.R")
reciever.coor = c("x" =  -1621816.051830534357578,
                  "y" =  5732436.208981316536665,
                  "z" =  2270380.964045653119683,
                  "h" = 7)
pos = pos2geo(pos_xyz = reciever.coor)
#satellite.coor.df <- spline.sp3()
satellite.coor.df <<- readRDS("splined_SP3.RData")
time_need = 0;

###############################################################
# HERE TO AUTOMATIC INTERPOLATE PRODUCT DATA AFTER A PERIOD OF
# TIME
###############################################################

iterpolate.product.data <- function() {
    source("../sp3_handler.R")
    #sp3.downloader()
    satellite.coor.df <<- spline.sp3()
    return(T)
}
# do iterpolate.production.data infinitively after each 6 hour
task <- after(15*60*1000, iterpolate.product.data, args = list(), redo = Inf)

# to cancel, run it below
# after$cancel(task) 
###############################################################
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
###############################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    
        map.data <- reactivePoll(
        10*1000,
        session,
        # This function returns the time that log_file was last modified
        checkFunc = function() {
            return(time_need)
        },
        # This function returns the content of log_file
        valueFunc = function() {
            library(httr)
            r <-
                GET("http://112.137.134.7:5000/data/last?stationID=pipi")
            data <- content(r)
            data$data
            # library(dplyr)
            library(wkb)
            msg = hex2raw(data$data)
            df = skytraq.parseE8(msg = msg)
            time = skytraq.parseDF(msg = msg)
            temp_pos = data.frame()
            
            time_need <<- time[[1]] * 7 * 24 * 60 * 60 +
                round(time[[2]])
            satellite.data = dplyr::filter(satellite.coor.df,
                                           TimeStamp >= time_need - 1 &
                                               TimeStamp < time_need) # don't know why == return fail =((
            
            data.el.az =   data.frame(t(apply(
                satellite.data, 1, function_cal_el_az
            )))
            colnames(data.el.az) <-
                c("azimuth", "elevation", "distance", "ID")
            temp.pos = data.frame()
            for (i in 1:nrow(data.el.az)) {
                dist_rp = reciever.coor["h"] / sin(as.numeric(data.el.az$elevation[i]))
                dx = dist_rp * sin(as.numeric(data.el.az$azimuth[i]))
                dy = dist_rp * cos(as.numeric(data.el.az$azimuth[i]))
                dLon = dx / (111320 * cos(pos[2]))
                dLat = dy / 110540
                lat_rp = pos[2] + dLat
                lon_rp = pos[1] + dLon
                h_rp = pos[3] - reciever.coor["h"]
                temp.pos = rbind(temp.pos, c(lon_rp, lat_rp, h_rp, time_need))
            }
            colnames(temp.pos) <-
                c("Lng", "Lat", "Height", "TimeStamp")
            print(time_need)
            return(temp.pos)
        }
    )
    
    
    output$map <- renderLeaflet({
        temp.pos <- as.data.frame(map.data())
        Lon = temp.pos[, "Lng"]
        Lat = temp.pos[, "Lat"]
        leaflet() %>% addTiles() %>%
            setView(lng = pos[[1]], lat= pos[[2]], zoom = 25) %>%
            addMarkers(lng = pos[[1]], lat = pos[[2]]) %>%
            addCircleMarkers(lng = Lon, lat = Lat, radius = 1)
        # mapshot(map, file = paste0("img/", time_need,".png"))
        # map
    })
    
    output$r.table <- renderTable({
        map.data()
    })
    

})
