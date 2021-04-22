#' Launch magicdesignee.
#'
#' This function launches a Shiny app (magicdesignee) locally. The app implements
#' magicdesign in its backend.
#'
#' @export

magicdesignee <- function() {
  appDir <- system.file("magicdesignee", package="magicdesign")
  if (appDir=="") stop("Could not find shiny directory. Try re-installing `magicdesign`.", call.=FALSE)
  shiny::runApp(appDir, display.mode="normal", launch.browser=TRUE)
}
