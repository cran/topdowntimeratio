#' @import data.table

# Fix 'no visible vinding for global variable'
# https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
# Variables are defined as NULL locally where possible, but for iterate and
# updateSegs, which need to be efficient, we include them as global Vars.
utils::globalVariables(names = c("segment_id", "segment_start", "segment_end"))

#' Set up a data.table for iterative segmentation
#'
#' @param data A data.frame or data.table containing lat, lon and timestamp
#' @return A data.table with numeric timestamp, and an initial segment
#' @export
#' @importFrom geodist geodist geodist_vec

setup <- function(data){
  stopifnot(is.data.table(data))

  # Fix 'no visible vinding for global variable'
  # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  timestamp = lat = lon = timestamp_numeric = segment_id = NULL

  setorder(data, timestamp)
  data <- unique(data, by = c("entity_id",
                              "lon",
                              "lat",
                              "timestamp"))

  # Initiates necessary columns
  set(data,
      j = c("timestamp_numeric",
            "segment_start",
            "segment_end",
            "adjusted_lat",
            "adjusted_lon",
            "segment_id"),
      value = list(
        as.numeric(data[["timestamp"]]),
        FALSE,
        FALSE,
        data[["lat"]],
        data[["lon"]],
        1L
      ))

  # Sets first and last row to be segment starts and ends
  set(data,
      i = 1L,
      j = "segment_start",
      value = TRUE)
  set(data,
      i = nrow(data),
      j = "segment_end",
      value = TRUE)

  ## Moved to initial block 20-09-2022
  # set(data,
  #     j = c("adjusted_lat",
  #           "adjustd_lon",
  #           "segment_id"),
  #     value = list(
  #       data[["lat"]],
  #       data[["lon"]],
  #       1L
  #     ))


  data[, `:=`(seg_start_lat = lat[1L], seg_start_lon = lon[1L],
              seg_start_time = timestamp_numeric[1L],
              seg_end_lat = lat[.N], seg_end_lon = lon[.N],
              seg_end_time = timestamp_numeric[.N]), segment_id]


  set(data,
      j = c("seg_dur",
            "seg_dist_lat",
            "seg_dist_lon"),
      value = list(data[["seg_end_time"]] - data[["seg_start_time"]],
                   data[["seg_end_lat"]] - data[["seg_start_lat"]],
                   data[["seg_end_lon"]] - data[["seg_start_lon"]]))
  set(data,
      j = "perc_of_seg_dur",
      value = (data[["timestamp_numeric"]] - data[["seg_start_time"]])/data[["seg_dur"]])

  segends <- data[["segment_end"]] & data[["segment_start"]]
  condition <- which(!segends)
  perc <- data[["perc_of_seg_dur"]][condition]

  set(data,
      i = condition,
      j = c("adjusted_lat", "adjusted_lon"),
      value = list(data[["seg_start_lat"]][condition] + (perc * data[["seg_dist_lat"]][condition]),
                   data[["seg_start_lon"]][condition] + (perc * data[["seg_dist_lon"]][condition])))


  set(data,
      j = "dist",
      value = list(
        geodist(cbind(lon = data[["lon"]], lat = data[["lat"]]),
                cbind(lon = data[["adjusted_lon"]], lat = data[["adjusted_lat"]]),
                paired = TRUE, measure = "haversine")))
  data[]
}


#' Perform one iteration of segmentation. Updates by reference and
#' should be an internal function.
#'
#' @param data data.table that has been setup by \code{setup}
#' @param max_error stopping criteria from \code{tdtr}
#' @return NULL

iterate <- function(data, max_error){
  # Make new segments at biggest distance
  data[data[dist > max_error, .I[which.max(dist)], by = segment_id]$V1,
       `:=`(segment_start = TRUE,
            segment_end = TRUE)]

  updateSegs(data)
  set(data,
      j = "segment_id",
      value = cumsum(data[["segment_start"]]))
  set(data,
      j = "seg_end_id",
      value = shift(data[["segment_id"]], fill = 1L))
  return(NULL)
}


controlLoop <- function(data, i, max_error, max_segs, n_segs){
  if (nrow(data) < 2){
    return()
  }
  data <- setup(data)

  while (data[, max(dist)] > max_error &&
         (i < max_segs) &&
         (i < n_segs)) {
    iterate(data, max_error = max_error)
    i <- i + 1
  }

  data
}

updateSegs <- function(data){
  lat <- .subset2(data, "lat")
  lon <- .subset2(data, "lon")
  tsn <- .subset2(data, "timestamp_numeric")
  starts <- data[(segment_start), which = TRUE]
  ends <- data[(segment_end), which = TRUE]
  Ncond <- length(starts)
  reptimes <- ends - starts
  reptimes[Ncond] <- reptimes[Ncond] + 1
  segstartlat <- rep(lat[starts], times = reptimes)
  segstartlon <- rep(lon[starts], times = reptimes)
  segst <- rep(tsn[starts], times = reptimes)
  segendlat <- rep(lat[ends], times = reptimes)
  segendlon <- rep(lon[ends], times = reptimes)
  seget <- rep(tsn[ends], times = reptimes)
  segdistlat <- segendlat - segstartlat
  segdistlon <- segendlon - segstartlon
  segdur <- seget - segst
  perc <- (tsn - segst)/segdur;

  adjlon <- segstartlon + perc * segdistlon;
  adjlat <- segstartlat + perc * segdistlat;
  newdist <- geodist_vec(lon, lat, adjlon, adjlat, paired = TRUE, measure = "haversine");
  newdist[is.nan(newdist)] <- 0
  set(data,
      j = c("dist",
            "seg_start_lat",
            "seg_start_lon",
            "seg_end_lat",
            "seg_end_lon",
            "seg_start_time",
            "seg_end_time",
            "adjusted_lon",
            "adjusted_lat",
            "seg_dur",
            "perc_of_seg_dur"),
      value = list(
        newdist,
        segstartlat,
        segstartlon,
        segendlat,
        segendlon,
        segst,
        seget,
        adjlon,
        adjlat,
        segdur,
        perc
      ))
}

#' Perform Top-Down Time Ratio segmentation
#'
#' @param data is a data.frame or data.table with timestamp, lat and lon
#' @param col_names named list with existing column names for timestamp,
#'   latitude and longitude column (these are changed to 'timestamp', 'lat' and
#'   'lon' respectively)
#' @param group_col NULL for no grouping, or string column name representing a
#'   grouping in the data where initial segments will be drawn.
#' @param max_segs with maximum number of segments allowed, default is  5000
#' @param n_segs used to generate a specific number of segments
#' @param max_error used as stopping criteria, default is 200
#' @param add_iterations Add iterations to previous \code{tdtr} run
#' @return data.table with segment information
#' @export
#' @examples
#' df <- data.frame(person = rep(1, 12),
#'    time = c(1, 2, 4, 10, 14, 18, 20, 21, 24, 25, 28, 29),
#'    longitude = c(5.1299311, 5.129979, 5.129597, 5.130028, 5.130555, 5.131083,
#'            5.132101, 5.132704, 5.133326, 5.133904, 5.134746, 5.135613),
#'    lat = c(52.092839, 52.092827, 52.092571, 52.092292, 52.092076, 52.091821,
#'            52.091420, 52.091219, 52.091343, 52.091651, 52.092138, 52.092698))
#' # Generate segments under a max error of 100m
#' res100 <- tdtr(df,
#'      col_names = list(entity_id_col = "person",
#'                       timestamp_col = "time",
#'                       latitude_col = "lat",
#'                       longitude_col = "longitude"),
#'      group_col = NULL,
#'      max_error = 100)
#' # Generate segments under a max error of 30m
#' res30 <- tdtr(df,
#'      col_names = list(entity_id_col = "person",
#'                       timestamp_col = "time",
#'                       latitude_col = "lat",
#'                       longitude_col = "longitude"),
#'      group_col = NULL,
#'      max_error = 30)
#' plot(df$lon, df$lat)
#' segments(res100$seg_start_lon, res100$seg_start_lat,
#'          res100$seg_end_lon, res100$seg_end_lat, col = "blue")
#' segments(res30$seg_start_lon, res30$seg_start_lat,
#'          res30$seg_end_lon, res30$seg_end_lat, col = "red")
tdtr <- function(data,
                 col_names = list(entity_id_col = "entity_id",
                                  timestamp_col = "timestamp",
                                  latitude_col = "lat",
                                  longitude_col = "lon"),
                 group_col = "state_id",
                 max_segs = 5000,
                 n_segs = max_segs,
                 max_error = 200,
                 add_iterations = FALSE){

  stopifnot(col_names %in% names(data)) # new 20-09-2022

  if (add_iterations == FALSE) {
    setDT(data)
    setnames(data, col_names$entity_id_col, "entity_id")
    setnames(data, col_names$timestamp_col, "timestamp")
    setnames(data, col_names$latitude_col, "lat")
    setnames(data, col_names$longitude_col, "lon")

    i <- 1

    if (!is.null(group_col)) {
      stopifnot(group_col %in% names(data))
      setnames(data, group_col, "group")

      data <- split(data, data$group, drop = TRUE) %>%
        lapply(controlLoop, i = i, max_error = max_error, max_segs = max_segs, n_segs = n_segs) %>%
        rbindlist(idcol = TRUE, fill = TRUE)

    } else {
      data <- controlLoop(data, i, max_error, max_segs, n_segs)
    }
  }
  else if (add_iterations == TRUE) {
    i <- data[, segment_id[.N]]
    data <- controlLoop(data, i, max_error, max_segs, n_segs)

  }
  data[]
}

