
#' Mean filter
#'
#' @param coord A vector of coordinates over which to apply a mean filter
#' @param n The number of values to average
#'
#' @return A vector of mean-averaged coordinates
#' @export
#'

meanFilter <- function(coord, n=3){
  as.vector(filter(coord, rep(1/n, n), sides=2))
}


#' Median filter
#'
#' @param coord A vector of coordinates over which to apply a mean filter
#' @param n The number of values to average (best when odd-numbered)
#'
#' @return A vector of median-averaged coordinates
#' @export
#'

medianFilter <- function(coord, n=3){
  as.vector(runmed(coord, n))
}

applyFilter <- function(dt, filter = c("mean", "median"), n = 3){
  lon = true_lon = lat = true_lat = NULL # Fix 'no visible vinding for global variable' https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  switch(filter,
         "mean" = {
           dt[, lon := fcoalesce(meanFilter(true_lon, n), true_lon)]
           dt[, lat := fcoalesce(meanFilter(true_lat, n), true_lat)]
         },
         "median"= {
           dt[, lon := fcoalesce(medianFilter(true_lon, n), true_lon)]
           dt[, lat := fcoalesce(medianFilter(true_lat, n), true_lat)]
         }
  )
}
