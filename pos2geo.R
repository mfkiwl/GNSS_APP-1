##################################################################
#
# Conversion from the cartesian coordinates (xyz) to geographical coordinates (geo) into a given ellipsoid
# 
# launch: source("pos2geo.R")
#
# 2nd semester 2012 
#
# Author: Nicolas ROUSSEL - nicolas.roussel@get.obs-mip.fr
#
#
##################################################################

pos2geo <- function(pos_xyz, R_0=6378137,R_p=6356752.314245179497563967){
  
  #
  # R_0 : semi-major axis of the given ellipsoid
  # R_p : semi-minor axis of the given ellipsoid
  # pos_xyz : cartesian coordinates of the considered point
  #
  
  options(digits=22)   # To get enough digits for calculations
  
  # cartesian coordinates of the point                
  x <- pos_xyz[1]
  y <- pos_xyz[2]
  z <- pos_xyz[3]
  
  if(R_0==R_p){ # if the ellipsoid is a sphere
    e <- 0
    f <- 999999999999999999999999999999999999999999999999999999   # flatness = infinity
  }
  
  else{
    e <-  sqrt((R_0^2-R_p^2)/R_0^2) # excentricity of the ellipsoid
    f <- R_0/e # flatness of the ellipsoid
  }
  if(x != 0 & y != 0){ # If the point is not the North pole or the South pole
    
    lon <- atan2(y, x) * 180/pi # longitude of the considered point in the considered ellipsoid
    
    #
    # Determining the latitude iteratively
    #
    
    lat_1 <- atan2(z, (1-e^2)*sqrt((x^2)+(y^2)))
    N_1 <- R_0/sqrt(1-e^2*sin(lat_1)*sin(lat_1))
    lat_2 <- atan2(z+(e^2*N_1*(sin(lat_1))),sqrt((x^2)+(y^2)))
    
    while (abs(lat_2 - lat_1) > 2*10^-10)
    {
      lat_int <- lat_1
      N_2 <- R_0/sqrt(1-e^2*sin(lat_2)*sin(lat_2))
      lat_1 <- atan2(z+(e^2*N_2*(sin(lat_2))),sqrt((x^2)+(y^2)))
      lat_2 <- lat_int
    }
    
    lat_geographique <- lat_1 # latitude of the considered point in the considered ellipsoid
    
    N <- R_0/sqrt(1-e^2*sin(lat_geographique)*sin(lat_geographique))  # normal of the considered point in the considered ellipsoid
    
    #
    # Determination of the ellipsoid height
    #
    
    he <- sqrt(x^2+y^2)/cos(lat_geographique) - N # ellipsoid height of the considered point in the considered ellipsoid
  }
  else{   # if the point is located in the North or South pole
    lon <- 0
    lat_geographique <- 90
    he <- R_0/sqrt(1-e^2)
  }
  coord_geo <- c(lon,lat_geographique*180/pi,he) # geographic coordinates of the considered point in the considered ellipsoid
  
  return(coord_geo)
  
}

#
# End of the function
#

