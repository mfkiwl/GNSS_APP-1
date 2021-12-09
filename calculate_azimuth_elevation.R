##################################################################
#
# This file contain function that calculate azimuth and elevation
# as well as distance from reciever and satellite coordinate
#
# Nov 2020 - Nguyen Phuong Bac
#
##################################################################

################# DEPENDANCES ##############
source("geo2pos.R")
source("pos2geo.R")

library(geosphere)
library(pracma)


#' @function: to calculate azimuth and elevation from satellite and reciever coordinates in ECEF
#' @param:    x, y, z: satellite coodinates
#' @param:    xr, yr, zr: reciever coodinates
#' @param:        R_p, R_0: semi=mojor and semi-minor axis of elipsoid
#'           f ....
#' @return:   cbind(azimuth, elevation)
#' @usage: source("calculate_azimuth_elevation.R")
calculate_azimuth_elevation <-
  function(x,
           y,
           z,
           xr,
           yr,
           zr,
           R_0 = 6378137,
           R_p = 6356752.314245179497563967,
           f = 1 / 298.257223563) {
    ########### CALCULATE AZIMUTH ##################
    coord_sphere_sat <- cart2sph(cbind(x, y, z)) * 180 / pi
    coord_sphere_rec <- cart2sph(cbind(xr, yr, zr)) * 180 / pi
    coord_sphere_rec <<- coord_sphere_rec
    coord_sphere_rec <- matrix(coord_sphere_rec, ncol = 3)
    azimuth <-
      (bearing(
        c(coord_sphere_rec[1], coord_sphere_rec[2]),
        c(coord_sphere_sat[1], coord_sphere_sat[2])
      ))
    azimuth[azimuth < 0] <- 360 + azimuth[azimuth < 0]
    azimuth = azimuth / 180 * pi
    
    ############# CALCULATE ELEVATION ##############
    pos_R_xyz <<- c(xr, yr, zr)
    result <- pos2geo(pos_R_xyz, R_0, R_p)
    pos_R_geo <<-  c(result[1] * pi / 180, result[2] * pi / 180, result[3])
    pos_R_geo_cur <-
      cbind(pos_R_geo[1] * 180 / pi, pos_R_geo[2] * 180 / pi, pos_R_geo[3]) # = result
    
    pos_R_xyz_cur <- geo2pos(pos_R_geo_cur) # = c(xr,yr,zr)
    pos_R_0_xyz <-
      geo2pos((pos_R_geo_cur + c(
        rep(0, dim(pos_R_geo_cur)[1]), rep(0, dim(pos_R_geo_cur)[1]), rep(-5, dim(pos_R_geo_cur)[1])
      )))
    vec_rec_0 <- rbind(pos_R_0_xyz - pos_R_xyz_cur)
    vec_rec_sat <- cbind(x - xr, y - yr, z - zr)
    scalaire <-
      vec_rec_sat[1] * vec_rec_0[1] + vec_rec_sat[2] * vec_rec_0[2] + vec_rec_sat[3] *
      vec_rec_0[3]
    
    cos_elevation_1 <-
      scalaire / sqrt(vec_rec_0[, 3] ^ 2 + vec_rec_0[, 2] ^ 2 + vec_rec_0[, 1] ^
                        2)
    cos_elevation_2 <-
      cos_elevation_1 / sqrt(vec_rec_sat[, 3] ^ 2 + vec_rec_sat[, 2] ^ 2 + vec_rec_sat[, 1] ^
                               2)
    acos_elevation <- acos(cos_elevation_2)
    elevation <- acos_elevation - pi / 2
    elevation[elevation >= 90 * pi / 180] <-
      180 * pi / 180 - elevation[elevation >= 90 * pi / 180]
    
    distance <- sqrt(apply(vec_rec_sat ^ 2, 1, sum))
    
    return(cbind(
      "azimuth" = azimuth,
      "elevation" = elevation,
      "distance" = distance
    ))
  }
