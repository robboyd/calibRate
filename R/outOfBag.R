#' Format data for calibration and optionally leave aside some data for validation
#'
#' This function will format your observations and model outputs for use in calibRate. It can also be used to set aside a number of e.g. years at the end of the time series for validation.
#' 
#' @param nOut Numeric. Number of data/ output increments to set aside for validation.
#' @param output string. One of "obs" or "preds".
#' @param minYr Numeric. First year of simulations. 
#' @param maxYr Numeric.
#' @param nVar Numeric. Number of output variables recorded each time increment. 
#' @param preds data.frame. "preds" output from getOutputs(). Leave as NULL if output = "obs".
#' @param obs Numeric vector. Observations. Leave as NULL if output = "preds".
#' @export
#'

outOfBag <- function (nOut, output, minYr, maxYr, nVar, preds = NULL, obs = NULL) {
  
  nYrs <- length(minYr:maxYr)
  
  getDat <- function(ind, type) {
    
    if (ind == 0) {
      
      if (type == "preds") {

        subStats <- preds[,1:(nYrs - ind)] 
        
      } else {
        
        subStats <- obs[1:(nYrs - ind)]
        
      }
      
    } else {
      
      if (type == "preds") {
        
        subStats <- preds[,((ind*nYrs)+1):(((ind+1)*nYrs)- nOut)] 
        
      } else {
        
        subStats <- obs[((ind*nYrs)+1):(((ind+1)*nYrs)- nOut)]
        
      }
      
    }
    
    
    subStats
    
  }
  
  out <- lapply(0:(nVar - 1),
                getDat,
                type = output)
  
  if (output == "preds") {
    
    out <- do.call(cbind, out)
    
  } else {
    
    out <- do.call(c, out)
    
  }
  
  
  return(out)
  
}
