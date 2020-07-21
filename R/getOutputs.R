#' load model outputs from your simulations
#'
#' This function loads model outputs generated under different parameter sets. The input data must be in the form of a 2d 
#' matrix with one column per outputs and one row per simulation. Note that this function is designed to work on outputs that
#' are extracted at regular increments from an IBM (e.g. biomass each year). 
#' 
#' @param inPath string. Location of the model outputs on your pc.
#' @param runName string. A string used to identify the file containing your model outputs. E.g. if the file is called "../outputs1.txt", runName could be "outputs1". This argument must not match any other files in inpath.
#' @param startYr Numeric. First year of simulations. Can be other incrememnts (i.e. not just year). Used to index your outputs by increment.
#' @param endYr Numeric.
#' @param outputNames Character vector. Names of the outputs stored at each incremement. E.g. if c("SSB", "TSB") with strtYear = 2001 and endYr = 2003, you would get SSB2001, SSB2002, SSB2003, TSB 2001, TSB 2002, TSB 2003.
#' @param return String. One of "preds" or "params".
#' @export
#'

getOutputs <- function(inPath, runName, startYr, endYr, outputNames, return) {
  
  "%!in%" <- Negate("%in%")
  
  files <- list.files(inPath, full.names = TRUE, pattern = runName)

  parFiles <- files[which(grepl("param", files, fixed = TRUE))]

  statFiles <- files[which(files %!in% parFiles)]

  if (length(parFiles) != length(statFiles)) {
    
    stop("Outputs and parameters are not of the same length")
    
  }
  
  stats <- lapply(1:length(statFiles), 
                  function(x) { t(read.table(statFiles[x]))})
  
  stats <- do.call(rbind,stats)
  
  params <- lapply(1:length(statFiles), 
                  function(x) { read.table(parFiles[x])})
  
  params <- do.call(rbind,params)

  if (any(duplicated(params))) {
    
    warning("Some parameter sets are duplicated")
    
  }
  
  names<-c()
  
  for(i in outputNames) {
    for (j in startYr:endYr) {
      names<-c(names, paste0(i,j))
    }
  }
  
  colnames(stats) <- names 

  if (return == "params") {
    
    return(params)
    
  } else {
    
    return(stats)
    
  }
}


