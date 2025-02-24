#' @name Functions
#' @title Functions used in paper
#' @description Logistic Transition function for regime probabilities
#' @note lpirfs package based regime-switching function
#' @author Andro Asatashvili








trigger_z <- function(z,specs){
  
  specs <- list()
  specs$lambda_z        <- lambda_z
  specs$gamma_z         <- gamma_z
  specs$use_hp_z        <- use_hp_z
  specs$lag_switching_z <- lag_switching_z
  z                     <- as.data.frame(z)
  names(z)              <- "z_var"
  specs$c               <- c
  
  if(specs$use_hp_z == TRUE){
    
    filter_results_z     <-   lpirfs::hp_filter(as.matrix(z), specs$lambda_z)
    gamma_z              <-   specs$gamma_z
    z_0_z                <-   as.numeric(scale(filter_results_z[[1]], center = TRUE))
    fz_z                 <-   (1 - exp(((-1)*gamma_z*z_0_z))/(1 + (exp((-1)*gamma_z*z_0_z))))
    
    # Use first lag of value from szitching function?
    if(isTRUE(specs$lag_switching_z)){
      
      fz_z               <-     (1 - exp(((-1)*gamma_z*(dplyr::lag(z_0_z, 1))))
                                 /
                                   (1 + (exp((-1)*gamma_z*(dplyr::lag(z_0_z, 1))))))
      
    }
    
    
  } 
  
  else  {
    
    fz_z               <-  (1 - exp((-1)*specs$gamma_z*z$z_var)
                            /
                              (1 + exp((-1)*specs$gamma_z*z$z_var)))
    
    # Use first lag of value from szitching function var1?
    if(isTRUE(specs$lag_switching_z)){
      
      
      fz_z               <-    dplyr::lag(fz_z, 1)
      
      
      
    }
    
    
  }
  
  
  
  return(fz_z)
}


###############################################################################
# 2nd trigger var: w
###############################################################################

trigger_w <- function(w,spec){
  spec                 <- list()
  spec$lambda_w        <- lambda_w
  spec$gamma_w         <- gamma_w
  spec$use_hp_w        <- use_hp_w
  spec$lag_switching_w <- lag_switching_w
  w                    <- as.data.frame(w)
  names(w)             <- "w_var"
  
  if(spec$use_hp_w == TRUE){
    
    filter_results_w     <-   lpirfs::hp_filter(as.matrix(w), spec$lambda_w)
    gamma_w              <-   spec$gamma_w
    w_0_w                <-   as.numeric(scale(filter_results_w[[1]], center = TRUE))
    fz_w                 <-   (1 - exp(((-1)*gamma_w*w_0_w))
                               /
                                 (1 + (exp((-1)*gamma_w*w_0_w))))
    
    # Use first lag of value from switching function?
    if(isTRUE(spec$lag_switching_w)){
      
      fz_w               <-     (1 - exp(((-1)*gamma_w*(dplyr::lag(w_0_w, 1))))
                                 /
                                   (1 + (exp((-1)*gamma_w*(dplyr::lag(w_0_w, 1))))))
      
    }
    
    
  } 
  
  else  {
    
    fz_w               <-  (1 - exp((-1)*spec$gamma_w*w$w_var)
                            /
                              (1 + exp((-1)*spec$gamma_w*w$w_var)))
    
    # Use first lag of value from switching function var1?
    if(isTRUE(spec$lag_switching_w)){
      
      fz_w               <-    dplyr::lag(fz_w, 1)
      
    }
    
    
  }
  
  
  
  return(fz_w)
  
}
