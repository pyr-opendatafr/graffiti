
#' Run R-Shiny dashboard
#'
#' @examples
#' \donttest{
#' # run R-Shiny dashboard
#' graffiti()
#' }
#' @export
graffiti <- function() {
  appDir <- system.file("shiny", "frenchEconomy", package = "graffiti")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `graffiti`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal", launch.browser = TRUE)
}
