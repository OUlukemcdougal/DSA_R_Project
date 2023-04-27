#' Dynamic plot using shiny
#'
#' @description
#' Wigdets, plotting areas and layouts
#'
#' @return plots
#' @export
#'
#'
shinyplots <- function(){
  shiny::runApp(system.file("ADVTTESTPackage", package ="ADVTTEST"),
                launch.browser = TRUE)
}
