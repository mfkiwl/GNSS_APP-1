##################################################################
#
# This file contain function that download and spline sp3
# satellite product data
#
# Nov 2020 - Nguyen Phuong Bac
#
##################################################################

#' cut out 
#' @param ma_datee time stamp corresponding to elevetion 
#' @param elevation_pointt a vector diff of elevation | dao ham
#' @param separate_ascendant BOOLEAN true if separe
#' @param trou_max maximum hole's length in timestamp data
#' @param longueur_minium_serie Vector of lamda
decoupe <- function(ma_datee,elevation_pointt,separe_ascendant,trou_max,longueur_minimum_serie){
  
  
  seq_a_prendre <- list()
  seq_complete <- seq(1,length(ma_datee),1)
  # On coupe d�j� par rapport � la date (s'il y a des trous dans les donn�es)
  ecart <- diff(ma_datee)
  indice_trou <- seq_complete[ecart>trou_max]
  # On s�pare ensuite les passages ascendant et descendant
  
  
  if(separe_ascendant){
    signe_elevation_point <- sign(elevation_pointt)
    ecart_2 <- diff(signe_elevation_point)
    indice_trou <- c(indice_trou,seq_complete[ecart_2!=0])
  }
  indice_trou <- indice_trou[order(indice_trou)]
  indice_trou <- c(0,indice_trou,length(ma_datee))
  incrr <- 1
  indice_trou <- indice_trou[!is.na(indice_trou)]
  if(length(indice_trou)>=1){
    for(u in 2:length(indice_trou)){
      if((indice_trou[u] - (indice_trou[u-1]+1))>longueur_minimum_serie){
        seq_a_prendre[[incrr]] <- seq_complete[(indice_trou[u-1]+1):indice_trou[u]]
        incrr <- incrr + 1
      }
    }
  }
  return(seq_a_prendre)
}
