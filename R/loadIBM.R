#' Load your IBM in R
#'
#' This function is a wrapper round the RNetLogo functions NLStart and NLLoadModel.
#' @param dummy Defaults to NULL for general use. Needed to run SEASIM-NEAM in parallel using parApply in the parallel package.
#' @param nlPath String. Location of Netlogo on your pc.
#' @param modelPath String. Path to SEASIM-NEAM on your pc
#' @param nl String. Used to identify the model instance (can be whatever you want, e.g. "mackerel").
#' @param gui Boolean. Do you want to open the NetLogo gui through R? SEASIM-NEAM runs faster if you don't. Defaults to FALSE.
#' @export
#'

loadIBM <- function (dummy = NULL, nlPath, modelPath, objName, gui = FALSE) {
  
  RNetLogo::NLStart(nlPath, gui = gui, nl.obj = nl)
  
  RNetLogo::NLLoadModel(modelPath, nl.obj = nl)
  
}