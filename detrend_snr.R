##################################################################
#
#  Fonction qui enleve le trend principal de la serie temporelle du SNR
#
# Input :
#   - abscisse : la date (en s) correspondant a chaque valeur de SNR
#   - snr : les valeurs de SNR
# Output :
#   - snr_detrend : la serie snr sans le trend
#
#   juillet 2014 - Nicolas ROUSSEL
#
##################################################################

#' Detrend SNR data
#'
#' @param abscisse corresponding time
#' @param snr vector of SNR data
#' @return A vector to detrended SNR data
detrend_snr <-
  function(abscisse, snr) {
    # en enlevant un polynome de degre 2
    ###
    # on enleve le trend
    ###
    
    model <- lm(snr ~ poly(abscisse, 3)[, 2])
    trend <- model$fitted.values
    snr_detrend <- snr - trend # on enleve le trend
    return(snr_detrend)
  }
#' Detrend SNR data in other way ^^
#'
#' @param abscisse corresponding time
#' @param snr vector of SNR data
#' @return A vector to detrended SNR data
detrend_snr_passe_bas <-
  function(abscisse, snr) {
    # en enlevant la frequence basse (marche moins bien)
    #        x11()
    #      plot(abscisse,snr,type='l')
    ###
    # on enleve le trend
    ###
    
    library(signal, warn.conflicts = FALSE, quietly = TRUE)
    snr_detrend <- filter(butter(2, c(0.4, 1), type = 'pass'), snr)
    detach(package:signal)
    # x11()
    # plot(abscisse,snr_detrend,col='red',type='l')
    return(snr_detrend)
    
  }
