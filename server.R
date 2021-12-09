#
# This is the server logic of a Shiny web application. Y
#
#THIS WEB APP IS FOR VISUALIZATION REFLEX POINTS
#
#    
#


library(shiny)
library(leaflet)
library(mapview)
library(stringr)
#source("https://install-github.me/gaborcsardi/after")
library(after)
library(RColorBrewer)

source("skytraq.R")
source("sp3_handler.R")
source("calculate_azimuth_elevation.R")
source("pos2geo.R")
reciever.coor = c("x" = -1845473,
                  "y" = 5828914,
                  "z" = 1810095,
                  "h" = 20)
pos = as.numeric(pos2geo(pos_xyz = reciever.coor))
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
PRN = c("PG01","PG02","PG03","PG04","PG05","PG06","PG07","PG08","PG09","PG10","PG12","PG13","PG14","PG15","PG16","PG17","PG18","PG19","PG20","PG21","PG22","PG23","PG24","PG25","PG26","PG27","PG29","PG31","PG32","PR01","PR02","PR03","PR04","PR05","PR07","PR08","PR09","PR12","PR13","PR14","PR15","PR16","PR17","PR18","PR19","PR20","PR21","PR22","PR24","PC01","PC02","PC03","PC04","PC05","PC06","PC07","PC08","PC09","PC10","PC11","PC12","PC13","PC14","PC19","PC20","PC21","PC22","PC23","PC24","PC25","PC26","PC27","PC28","PC29","PC30","PC32","PC33","PC34","PC35","PC36","PC37","PC38","PC39","PC40","PC41","PC42","PC43","PC44","PC45","PC46","PE01","PE02","PE03","PE04","PE05","PE07","PE08","PE09","PE11","PE12","PE13","PE15","PE19","PE21","PE24","PE25","PE26","PE27","PE30","PE31","PE33","PE36")
palette1 <- rainbow(length(PRN)) 

color.by.prn <- function(x) {
    # if (str_detect(x, "PG") > 0) return(color.blue(which(PRN == x)))
    # if (str_detect(x, "PE") > 0) return(color.red(which(PRN == x)))
    # if (str_detect(x, "PC") > 0) return(color.green(which(PRN == x)))
    # if (str_detect(x, "PR") > 0) return(color.yellow(which(PRN == x)))
    return(palette1[which(PRN==x)])
    
}
###############################################################

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    pos.df = data.frame()
    print("new")
    map <- reactivePoll(
        30*1000,
        session,
        # This function returns the time that log_file was last modified
        checkFunc = function() {
            return(time_need)
        },
        # This function returns the content of log_file
        valueFunc = function() {
            library(httr)
            r <-
                GET("http://112.137.134.7:5000/data/last?stationID=60cc71512c850461b8693d93")
            data <- content(r)
            # library(dplyr)
            library(wkb)
            msg = hex2raw(data$data)
            df = skytraq.parseE5(msg = msg)
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
            # data.el.az = data.frame(df$Azimuth, df$Elevation, df$`SV ID`, df$`Observation Type`)
            colnames(data.el.az) <-
                c("azimuth", "elevation", "distance", "ID")
            temp.pos = data.frame()
            for (i in 1:nrow(data.el.az)) {
                if (!(data.el.az$ID[i] %in% df$DataFrame$`Observation Type`)) next
                dist_rp = reciever.coor["h"] / sin(as.numeric(data.el.az$elevation[i]))
                dx = dist_rp * sin(as.numeric(data.el.az$azimuth[i]))
                dy = dist_rp * cos(as.numeric(data.el.az$azimuth[i]))
                dLon = dx / (111320 * cos(pos[2]))
                dLat = dy / 110540
                lat_rp = pos[2] + dLat
                lon_rp = pos[1] + dLon
                h_rp = pos[3] - reciever.coor["h"]
                temp.pos = rbind(temp.pos, c(lon_rp, lat_rp, h_rp, time_need, data.el.az$ID[i]))
            }
            colnames(temp.pos) <-
                c("Lng", "Lat", "Height", "TimeStamp", "ID")
            
            pos.df <<- rbind(temp.pos, pos.df)
            print(time_need)
            print(nrow(pos.df))
            
            m <- leaflet() %>% addProviderTiles(providers$Esri.WorldImagery) %>%
                setView(pos[1], pos[2], zoom =16) %>%
                addCircleMarkers(pos[1], pos[2], color="red", radius = 2) 
            for (row in 1:nrow(pos.df)){
                m <- addCircleMarkers(
                    m,
                    lng = as.numeric(pos.df[row, "Lng"]),
                    lat = as.numeric(pos.df[row, "Lat"]),
                    radius = 1,
                    color = "red"#color.by.prn(pos.df[row, "ID"])
                )
            }
            return(m)
        }
    )
    
    
    output$map <- renderLeaflet({
        map()
    })
    
    output$r.table <- renderTable({
        map()
        return(pos.df)
    })
    
    output$txt <- renderText({
        print("start to save...")
        map() %>% mapshot(url = NULL, file = paste0("img/img_",time_need, ".png"))
        return("saved...")
    })
    
})

