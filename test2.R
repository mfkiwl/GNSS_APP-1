source("multidimensional_spline.R")
spline.df <- splineNd(x = pg01[,c(1,3,4,5)], dimension = 4, name = pg01[1,2], step = 172500)

df = as.data.frame(matrix(name, nrow = 172500))

library(rgl)
plot3d(satellite.coor.df[,c(1,2,3)]) 
plot3d(pg01[,3], pg01[,4], pg01[,5])
spheres3d(x = 0,
          y = 0,
          z = 0,
          radius = 6371)



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
