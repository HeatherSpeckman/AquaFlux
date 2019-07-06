
#####################################################################
#             Actually Run AquaFlux                         ##
#####################################################################

#' AquaFlux
#' @name AquaFlux
#' @title AquaFlux: Swift and Transparent Analysis of Plant-level Transpiration
#' @description AquaFlux is an open-source R package designed to efficiently process and analyze TDP data.  AquaFlux can continually import raw TDP values with flexible formatting and alerts the user of malfunctioning sensors, thus optimizing data collection.  Post-processing analysis addresses gapfilling, radial trends, and rescaling.   To ensure reproducibility and transparency, all user decisions are automatically documented, highlighting the impact of the user’s decisions.  For detailed walkthrough, see README and Speckman, H.N., Beverly, D.B. & Ewers, B.E. (In review) AquaFlux: Swift and Transparent Analysis of Plant-level Transpiration” .
#' @usage AquaFlux()
#' @export

AquaFlux <- function() {
  shiny::shinyApp(.AquaFlux.ui, .AquaFlux.server)
}
