determine_lambda <- function(gnss.type, signal.type, freq.id) {
  speed.of.light <- 299792458
  # GNSS type: 0 – GPS   1 – SBAS  2 – GLONASS  3 – Galileo  4 – QZSS  5 – BeiDou  6 - IRNSS
  # Signal type:  0-1 for L1 frequency (around 1575.42 MHz), 2-3 for L2 frequency (around 1227.60 MHz), 
  #               4-5 for L3 frequency(around 1176.45 MHz), 6-7 for other frequency.
  #               GPS:    0 – L1 C/A    1 – L1C   2 – L2C   4 – L5  
  #               SBAS:   0 – L1  
  #               GLONASS:0 – L1                  2 – L2    4 – L3
  #               Galileo:0 – E1                            4 – E5a   5 – E5b   6 – E6
  #               QZSS:   0 – L1 C/A    1 – L1C   2 – L2C   4 – L5              6 – LEX
  #               BeiDou: 0 – B1I       1 – B1C             4 – B2A   5 – B2I             7 – B3I
  #               IRNSS:                                    4 – L5
  # Server ID: Satelite PRN
  # Frequency ID: range(-7 to 6) used only for GLONASS
  
  determine.G.freq <- function(st) {
    if (st == 0 | st == 1) { # L1A L1C
      return(1575420000)
    } else if (st == 2 ) { # L2C
      return(1227600000)
    } else if (st == 4) { # L5
      return(1176450000)
    } 
    return(0)
  }
  
  determine.R.freq <- function(st, fid) {
    if (st  == 0) { # L1
      return(1602*10^6+fid*0.5625*10^6) 
    } else if (st == 2) { # L2
      return(1246*10^6+fid*0.4375*10^6)
    } else if (st == 4) { #L3
      return(1202025000)
    }
    return(0)
  }
  
  determine.E.freq <- function(st) {
    switch(st+1, 
           1575.42 * 10^6, # E1
           0,
           0,
           0,
           1176.45 * 10^6, # E5a
           1207.14 * 10^6, # E5b
           1278.75 * 10^6) # E6
  }
  
  determine.C.freq <- function(st) {
    if (st == 0) { #B1I
      return(1561.098*10^6) 
    } else if ( st == 1) { # B1C
      return (1575.42 * 10^6)
    } else if (st ==4) { # B2A
      return(1176.45 * 10^6)
    } else if (st == 5) { # B2I
      return(1207.14 * 10^6)
    } else if (st == 7) { # B3I
      return (1561.098*10^6)
    }
  }
  
  determine.J.freq <- function(st) {
    if(st == 0 | st ==1) { # L1 C/A L1C
      return(1575.42 * 10^6) 
    } else if ( st == 2) { #L2C
      return(1227.6*10^6)
    } else if (st == 4) { #L5
      return(1176.45*10^6)
    } else if( st == 6) { #L6
      return(1278.75*10^6)
    }
    return(0)
  }
  
  determine.I.freq <- function(st) {
    if (st == 4) return(1176.45 * 10^6)
  }
  
  
  freq <- switch(as.numeric(gnss.type) +1, 
                 determine.G.freq(signal.type),
                 0,
                 determine.R.freq(signal.type, freq.id),
                 determine.E.freq(signal.type),
                 determine.J.freq(signal.type),
                 determine.C.freq(signal.type),  
                 determine.I.freq(signal.type))
  
  return (speed.of.light/freq)
}
