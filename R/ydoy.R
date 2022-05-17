#' Parse dates with year and day of year
#'
#' @param year a numeric vector of year
#' @param doy a numeric vector of day of year
#'
#' @return date object
#' @export
#'
#' @examples ydoy(2016, 131)
ydoy <- function(year, doy){
  as.Date(doy-1, origin = paste0(year, "-01-01"))
}
