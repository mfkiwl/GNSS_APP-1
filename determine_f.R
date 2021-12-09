######### configuration
#
elevation_min = 0
elevation_max = 90 * pi
azimuth_to_take = c(0, 4.90016377339064, 4.91040748911152, 2 * pi)
minimum_length_series = 200
max_hole = 30
h_min <<- 50
h_max <<- 300
h_fixed <<- TRUE
H0 = 7.5
#

source("calcul_lsp.R")
source("cal.fresnel.ellipse.R")
calcul_f_a_phi <-
  function(ma_date,
           snr,
           elevation,
           elevation_point,
           longueur_onde,
           f_min,
           f_max,
           longueur_minimum_serie) {
    # Detrendage du snr  --> on enleve la contribution du signal direct.
    if (length(snr) > longueur_minimum_serie) {
      snr_detrended <- detrend_snr(ma_date, snr)
    }
    else{
      snr_detrended <- snr
    }
    f <- NaN
    amplitude <- NaN
    phase <- NaN
    f <-
      calcul_temps_frequence(sin(elevation), snr_detrended, f_min, f_max, 'RIAA') #methode possible : 'lsp' ou 'RIAA'
    
    if (h_fixed) {
      f_calcul <- 2 * H0 / longueur_onde
    }
    else{
      f_calcul <- f
    }
    # Determination de l'amplitude et du dephasage par les moindres carres
    K1 <- cos(2 * pi * f_calcul * sin(elevation))
    K2 <- -sin(2 * pi * f_calcul * sin(elevation))
    K3 <- snr_detrended
    P <- diag(length(K1))
    matA <- mat.or.vec(length(K1), 2)
    matA[, 1] <- K1
    matA[, 2] <- K2
    mamat <- NA
    library(MASS)
    try(mamat <- ginv(t(matA) %*% P %*% matA), silent = TRUE)
    if (length(mamat) > 1) {
      result <- mamat %*% (t(matA) %*% P %*% K3)
      Acos_phi <- result[1]
      Asin_phi <- result[2]
      
      v <- matA %*% c(result[1], result[2]) - K3
      emq_0 <- sqrt((t(v) %*% P %*% v) / (length(K1) - 2))
      amplitude <- sqrt(Acos_phi ^ 2 + Asin_phi ^ 2)
      
      acos_phi <- acos(Acos_phi / amplitude)
      asin_phi <- asin(Asin_phi / amplitude)
      if (asin_phi < 0) {
        phase <- -acos_phi
      }
      else if (asin_phi > 0) {
        phase <- acos_phi
      }
      phase <- phase %% (2 * pi)
      
    }
    else{
      phase <- NaN
      amplitude <- NaN
    }
    return(c(amplitude, phase, f))
  }

determine_f <- function(file.name) {
  source('detrend_snr.R')
  source('decoupe.R')
  library(MASS, quietly = FALSE)
  library(lubridate, quietly = FALSE)
  library(tidyr)
  file.path = paste0("Extraction/", file.name)
  
  # date_final <- 0
  # nom_sat_final <- 0
  # elevation_final <- 0
  # azimuth_final <- 0
  # elevation_point_final <- 0
  # amplitude_final <- 0
  # phase_final <- 0
  # longueur_onde_final <- 0
  # f_final <- 0
  # incr <- 1
  
  
  emepheris.data = readRDS(file.path) %>% drop_na()
  
  # on apply filter on elevation
  emepheris.data = subset(emepheris.data,
                          elevation < elevation_max & elevation > elevation_min)
  
  # on apply filer on azimuth
  temp.index = 0
  for (i in seq(1, length(azimuth_to_take), 2)) {
    temp.index <-
      c(
        temp.index,
        which(
          emepheris.data$azimuth >= azimuth_to_take[i] &
            emepheris.data$azimuth <= azimuth_to_take[i + 1]
        )
      )
  }
  emepheris.data <- emepheris.data[unique(temp.index), ]
  rm(temp.index)
  
  
  # determine f
  snr = as.numeric(emepheris.data$CNR)
  if (length(snr) < minimum_length_series) {
    print("Not enough suitable data (too big hole or too short) check it again")
  }
  
  snr = 10 ^ (snr / 20) # convert dbHz to Volt
  
  ma_date <- as.numeric(emepheris.data$timestamp)
  elevation <- as.numeric(emepheris.data$elevation)
  azimuth <- as.numeric(emepheris.data$azimuth) %% (2 * pi)
  longueur_onde <- as.numeric(unique(emepheris.data$Wavelength))
  if (length(longueur_onde) > 1) {
    print("determine_f.R: Fail when extract file lead to this error.")
  }
  elevation <- smooth.spline(ma_date, elevation)$y
  elevation_point <- diff(elevation)
  
  ma_date <- ma_date[1:(length(ma_date) - 1)]
  elevation <- as.numeric(elevation[1:(length(elevation) - 1)])
  azimuth <- azimuth[1:(length(azimuth) - 1)]
  snr <- snr[1:(length(snr) - 1)]
  
  
  a_prendre <-
    decoupe(ma_date, elevation, TRUE, 30, minimum_length_series)
  if (length(a_prendre) != 0) {
    for (u in 1:length(a_prendre)) {
      ma_date_cur <- ma_date[a_prendre[[u]]]
      elevation_cur <- elevation[a_prendre[[u]]]
      elevation_point_cur <- elevation_point[a_prendre[[u]]]
      snr_cur <- snr[a_prendre[[u]]]
      azimuth_cur <- azimuth[a_prendre[[u]]]
      
      f_min <- 2 / longueur_onde * h_min
      f_max <- 2 / longueur_onde * h_max
      result <-
        calcul_f_a_phi(
          ma_date_cur,
          snr_cur,
          elevation_cur,
          elevation_point_cur,
          longueur_onde,
          f_min,
          f_max,
          minimum_length_series
        )
      return(c("timestamp" = mean(as.numeric(ma_date_cur)),
               "PNR" = substr(file.name, 1,6),
               "elevation" = mean(elevation_cur),
               "azimuth" = mean(azimuth_cur),
               "elevation.point" = mean(elevation_point_cur),
               "amplitude" = result[1],
               "phase" = result[2],
               "wavelength" = longueur_onde,
               "frequency" = result[3],
               "height" = result[3] *longueur_onde /2),
               cal.fresnel.ellipse(h = result[3] *longueur_onde /2, mean(elevation_cur), longueur_onde))
    }
  }
}
