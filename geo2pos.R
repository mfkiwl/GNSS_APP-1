##################################################################
#
# Conversion from the geographic coordinates (geo) in a given ellipsoid to WGS84 cartesian coordinates (xyz)
#
# Launch: source("geo2pos.R")
#
# 2nd semester 2012
#
# Author: Nicolas ROUSSEL - nicolas.roussel@get.obs-mip.fr
#
##################################################################

geo2pos <- function(pos_geo, R_0=6378137,R_p=6356752.314245179497563967){
  
  #
  # R_0 : semi-major axis of the considered ellipsoid
  # R_p : semi-minor axis of the considered ellipsoid
  # pos_geo : geographic coordinates of the considered point in the considered ellipsoid
  #
  
  options(digits=22)   # to have enough digits pour calculations 
  
  if(R_0==R_p){
    e <- 0
    f <- 999999999999999999999999999999999999999999999999999999
  }
  else{
    e <-  sqrt((R_0^2-R_p^2)/R_0^2) # excentricity of the considered ellipsoid
    f <- R_0/e # flatness of the considered ellipsoid
  }
  
  lon <- pos_geo[,1]*pi/180 # longitude of the considered point in the considered ellipsoid
  lat <- pos_geo[,2]*pi/180 # latitude of the considered point in the considered ellipsoid
  
  N <- R_0/(sqrt(1-e^2*sin(lat)*sin(lat))) # major-normal N of the considered point in the considered ellipsoid
  n <- e^2*N
  
  alt <- pos_geo[,3] + N # distance between the point and its projection in the direction of the major-normal N, on the projection of the meridian in the equator plane
  
  #
  # Determination of the WGS84 cartesian coordinates
  #
  
  x <- alt*cos(lon)*cos(lat)
  y <- alt*sin(lon)*cos(lat)
  z <- sin(lat)*(alt - n)
  
  coord_xyz <- matrix(c(x,y,z),ncol=3)
  
  #
  # End of the function
  #
  
  return(coord_xyz)
  
}
