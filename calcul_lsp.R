noise_level_est <- function(PSD_tmp,omega,TimeCourse_Exp,Time){
  C <- max(PSD_tmp)
  IX <- PSD_tmp[PSD_tmp == max(PSD_tmp)]
  amplitude <- sqrt(2*C)
  f <- omega[IX] / (2*pi)
  phase <- seq(0,to=2*pi,by=2*pi/100)
  noise <- 99999
  for(i in 1:length(phase)){
    tmp <- amplitude * cos(2*pi*f*Time + phase[i])
    a <- (tmp-TimeCourse_Exp)
    noise_tmp <- sqrt(a%*%a)^2/length(Time)
    if(noise_tmp < noise){
      noise <- noise_tmp
    }
  }
  return(noise)
}


calcul_temps_frequence <- function(x,y,f_min,f_max,method){
  if(method=='lsp'){
    library(lomb,quietly=TRUE)
    aaze  <- 1   
    f <- NaN
    result_lsp <- NaN
    plot_lsp <- TRUE
    
    try(result_lsp <- lsp(y,times=x,from=f_min,to=f_max,plot=plot_lsp,ofac=100,alpha=0.01),silent=TRUE)
    
    # Si on veut comparer les r�sultats avec methode RIAA      
    #result_RIAA <- calcul_RIAA(x,y,f_min,f_max)
    #     if(!is.na(result_lsp)){
    #            jpeg(paste(round(result_lsp$peak.at[1],2),round(max(result_RIAA),2),'.jpg',sep='_'))
    #            x_lsp <- result_lsp$scanned
    #            y_lsp <- result_lsp$power 
    #            y_lsp <- (y_lsp - min(y_lsp))*(1 - 0)/(max(y_lsp)-min(y_lsp)) + 0
    #            x_riaa <- seq(f_min,f_max,1)
    #            y_riaa <- P_RIAA
    #            y_riaa <-  (y_riaa - min(y_riaa))*(1 - 0)/(max(y_riaa)-min(y_riaa)) + 0
    #            plot(x_lsp,y_lsp,col='red',type='l',lwd=5)
    #            lines(x_riaa,y_riaa,type='l',lwd=2)
    #            dev.off()
    #      }
    
    if(length(result_lsp)>1){
      amplitude <- result_lsp$peak
      level <- result_lsp$sig.level
      if(amplitude>level){ 
        f <<- result_lsp$peak.at[1]
      }
    }
    
    if(!is.na(f)){
      if(abs(f-f_max)<1){
        f <- NaN
      }
    }          
    return(f)
  }# fin methode lsp
  
  else if(method =="RIAA"){
    Time <- x
    TimeCourse_Exp <- y
    res <- approx(Time,TimeCourse_Exp,n=500)
    TimeCourse_Exp <- res$y
    Time <- res$x
    isnoise <- 0
    # Evaluate Periodogram of time-course expression data using RIAA algorithm
    # Input:
    #   TimeCourse_Exp: time-course expression data, n*1 matrix (denoted as Y in the paper)
    #   omega:  frequencies (omega), G*1 matrix
    #   Time:  sampling times
    #
    # n: number of time points
    
    omega <- seq(f_min,f_max,1)*2*pi
    n <- length(TimeCourse_Exp)
    G <- length(omega)          # G: number of grids in periodogram
    Need_EstExpMean <- 0
    if(Need_EstExpMean==1){
      TimeCourse_Exp <- TimeCourse_Exp - mean(TimeCourse_Exp)
    } 
    
    Iter_No <- 15
    P_RIAA <- seq(0,0,length=G)
    
    #Define matrix A
    A_set <- list()
    for(g in 1:G){
      A_set[[g]] <- matrix(c(cos(omega[g]*Time),sin(omega[g]*Time)),ncol=2)       
    }
    
    rho <- matrix(rep(0,2*G),nrow=2)
    
    # Initialization
    for (g in 1:G){
      A_mat <- A_set[[g]];
      rho[,g] <- solve(aperm(A_mat) %*% A_mat) %*% (aperm(A_mat) %*% TimeCourse_Exp)
    }
    
    # Beginning of signal power estimate
    no <- 0
    flag <- 1
    while (flag){
      rho_tmp <- rho
      Alpha_tmp <- sqrt(rho_tmp[1,]^2 + rho_tmp[2,]^2)
      
      Gamma <- matrix(rep(0,n*n),ncol=n)
      y_esti <- matrix(rep(0,n),nrow=n)
      
      #Calculate Gamma to help with the computation
      # The Gamma is defined in Stoica et al. 2009
      
      for(g in 1:G){
        A_mat <- A_set[[g]]
        Gamma <- Gamma + (rho[1,g]^2 + rho[2,g]^2)/2 * A_mat %*% t(A_mat)
        y_esti <- y_esti + A_mat %*% rho[,g]
      } 
      
      
      if(isnoise==1){
        PSD_tmp <- matrix(rep(0,G),nrow=G)
        for(g in 1:G){
          A_mat <- A_set[[g]]
          PSD_tmp[G] <- 1/n*t(rho[,g]) %*% (t(A_mat) %*% A_mat) %*% rho[,g]
        }
        noise <- noise_level_est(PSD_tmp,omega,TimeCourse_Exp,Time)
        if(noise>3){
          noise <- 3
        }
        Gamma <- Gamma + noise *diag(n)
      }
      
      if(kappa(Gamma)>10000){
        flag <- 0
      }
      else{
        for(g in 1:G){
          A_mat <- A_set[[g]]
          rho[,g] <- (t(A_mat)/Gamma * A_mat) %/% (t(A_mat)/Gamma * TimeCourse_Exp)
        }
      }
      # For termination
      no <- no +1
      if (no >= Iter_No){
        flag <- 0
      }
      
      Alpha <- sqrt(rho[1,]^2 + rho[2,]^2)
      Thres <- sqrt((Alpha - Alpha_tmp)%*%(Alpha - Alpha_tmp))/sqrt(Alpha_tmp %*% Alpha_tmp)
      if(Thres < 5e-3){
        flag <- 0
      }     
    }
    # Power spectral density estimation
    for(g in 1:G){
      A_mat <- A_set[[g]]
      P_RIAA[g] <- 1/n*t(rho[,g])%*%(t(A_mat)%*%A_mat)%*%rho[,g]
    }
    
    f <- NaN
    a <- seq(f_min,f_max,1)
    f <- a[P_RIAA==max(P_RIAA)]
    #      if(max(P_RIAA)<100){
    #        f <- NaN
    #      }
    #      
    #      if(!is.na(f)){
    #            if(abs(f-f_max)<1){
    #              f <- NaN
    #            }
    #      }
    return(f)  
  } # fin de methode RIAA
}

