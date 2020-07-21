#' Determine which parameter result in the best fits to your dad using the sum of the squared deviations
#'
#' This function using the sum of squares cost function to determine which of your parameter sets give the best fits to your data. Distances are normalized using the mad to account for differences in units.
#' 
#' @param preds data.frame. "preds" outputs from outOfbag().
#' @param params data.frame. Data frame with parameter values that correspond to your outputs in preds. One column per parameter, and one row per simulation (value).
#' @param obs Numeric vector. "Obs" output from outOfBag().  
#' @param nDat Numeric vector. The number of data points in each variable type. 
#' @param weights Numeric vector. Vector of weights equal to the number of model outputs.
#' @param tol Numeric. Tolerance rate, i.e. what proportion of parameter sets to "accept".
#' @export
#'

minSumSquares <- function(preds, params, obs, nDat, weights, tol) {
  
  mads <- sapply(X = 1:ncol(preds),
         function(x) { mad(preds[,x])})

  for (i in 1:length(nDat)) {
    
    if (i == 1) {
      
      assign(paste0("mads",i),
             mean(mads[1:nDat[1]]))
      
    } else {
      
      assign(paste0("mads", i),
             mean(mads[(nDat[(i-1)] + 1): (nDat[i-1] + nDat[i])]))
      
    }
    
  }
  
  dists <- rep(0, nrow(preds))
  
  for (i in 1:length(dists)) {
    
  for (j in 1:ncol(preds)) {
      
    dists[i] <- dists[i] + (weights[j] *(((preds[i,j] - obs[j])/mads[j]) ^2))
      
  }
    
  }

  dists <- sqrt(dists)

  min <- which.min(dists)

  sim <- 1:length(dists)
  
  sum_cal <- cbind(params, dists,sim)

  accepted_sims <-sum_cal[order(sum_cal$dists),]

  accepted_sims <- accepted_sims[1:(nrow(params) * tol), ]

  accepted_preds <- preds[accepted_sims$sim,]

  out <- list(accepted_sims, accepted_preds)

  names(out) <- c("params","preds")  
  
  return(out)
  
}


