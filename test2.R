source("multidimensional_spline.R")
spline.df <- readRDS("Extraction/PR18_2.RDS")
df = as.data.frame(matrix(name, nrow = 172500))

library(rgl)
plot3d(spline.df[, c(1, 2, 3)])
plot3d(pg01[, 3], pg01[, 4], pg01[, 5])
spheres3d(x = 0,
          y = 0,
          z = 0,
          radius = 6378000)



# # ##################
# # #PLOT EARTH #####
# library(globe)
# globeearth(eye = c(105, 21), top = place("northpole"))
# globepoints(
#   loc = cbind(105.8342, 21.0278),
#   colour = 0xFFFF,
#   do.plot = TRUE
# )
# # ################

library(threejs)
globejs(
        img = "m.jpg",
        lat = temp.pos$long,
        long = temp.pos$lat,
        val = temp.pos$height,
        pointsize = 5,
        atmosphere = FALSE,
        bg = "white"
)