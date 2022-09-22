#' Get Segments
#'
#' Extract segment info from the segmented data.table.
#'
#' Segment location information can be either in lat/lon coordinates, or
#' expressed in terms of distance for a more anonymous presentation of small
#' trajectories. (Full anonymity is not guaranteed as sufficiently long
#' trajectories with small error parameters can provide enough data to match
#' against a map.)
#'
#' @param data data.table returned from function tdtr()
#' @param coord.type return actual coordinates, relative distance, or both (see
#' Details)
#' @param group separate by group, default is FALSE
#' @return data.table with segments only, containing information about the start
#' and end locations, start and end time and distance covered by the segment
#' @export
#' @examples
#' df <- data.frame(entity_id = rep(1, 12),
#'    timestamp = c(1, 2, 4, 10, 14, 18, 20, 21, 24, 25, 28, 29),
#'    lon = c(5.1299311, 5.129979, 5.129597, 5.130028, 5.130555, 5.131083,
#'            5.132101, 5.132704, 5.133326, 5.133904, 5.134746, 5.135613),
#'    lat = c(52.092839, 52.092827, 52.092571, 52.092292, 52.092076, 52.091821,
#'            52.091420, 52.091219, 52.091343, 52.091651, 52.092138, 52.092698))
#' # First generate segments
#' res30 <- tdtr(df,
#'      group_col = NULL,
#'      max_error = 30)
#' # Then extract a data.table of segments
#' getSegments(res30)
#'
#' # Calculating distance instead of coordinates
#' segs <- getSegments(res30, coord.type = "distance")
#' segs
#' plot(c(0, 700), c(0, 200), col = "white",
#'      xlab = "East-West distance",
#'      ylab = "North-South distance")
#' with(segs,
#'      segments(seg_start_lon_dist, seg_start_lat_dist,
#'       seg_end_lon_dist, seg_end_lat_dist))


getSegments <- function(data, coord.type = c("coordinate", "distance", "both"), group = FALSE){

  # Fix 'no visible vinding for global variable'
  # https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  segment_start = seg_start_lon = seg_start_lat = seg_start_time = seg_end_lon =
    seg_end_lat = seg_end_time = segdist = .id = entity_id = id = NULL

  coord.type <- match.arg(coord.type, c("coordinate", "distance",
                                        "both"))
  segs <- data[(segment_start)]

    set(segs,
      j = "segdist",
      value = geodist(cbind(lon = segs[["seg_start_lon"]], lat = segs[["seg_start_lat"]]),
                      cbind(lon = segs[["seg_end_lon"]], lat = segs[["seg_end_lat"]]),
                      paired = TRUE, measure = "haversine"))

  if (group == TRUE) {
    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist, .id, entity_id)])
  } else {
    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist, entity_id)])
  }
  if (coord.type == "coordinate") {
    return(segs[])
  }
  else {
    convertCoordsToDist(segs, c("seg_start_lat", "seg_end_lat"))
    convertCoordsToDist(segs, c("seg_start_lon", "seg_end_lon"))
  }
  if (coord.type == "both") {
    return(segs[])
  }
  else if (coord.type == "distance") {
    set(segs, j = c("seg_start_lat", "seg_start_lon", "seg_end_lat",
                    "seg_end_lon"), value = NULL)
    segs[]
  }
}

convertCoordsToDist <- function(data, coord_cols){
  mincoord = NULL # Fix 'no visible vinding for global variable' https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153

  distFromMin <- function(coord, mincoord){
    geodist::geodist(data.table(longitude = 0, latitude = coord),
                     data.table(longitude = 0, latitude = mincoord), paired = TRUE, measure = "haversine")
  }


  data[, mincoord := min(.SD, na.rm = TRUE), .SDcols = coord_cols]
  data[, `:=`(paste0(coord_cols, "_dist"), lapply(.SD, distFromMin, mincoord = mincoord)), .SDcols = coord_cols]
  data[, mincoord := NULL][]

}

#' Get Segments with calculated data
#'
#' This function calculates various segment-level metrics that require the raw
#' data before returning a data.table with the segments and the calculated
#' results. Calculates speed, bearing and radius of gyration information.
#'
#'
#' @param data data.table returned from function /code{tdtr}
#' @param coord.type return actual coordinates, relative distance, or both
#' @param group Separate by group, default is FALSE
#' @return data.table of segments, annotated with segment-level information on
#'   distance, mean and variance of immediate bearing difference, total bearing
#'   variance over the segment, mean, maximum and variance of calculated speed
#'   in meters per second, percentage of zero-speed entries, whether the segment
#'   consists of fewer than 3 locations, and the time-weighted radius of
#'   gyration.
#' @export
#' @examples
#' df <- data.frame(entity_id = rep(1, 12),
#'    timestamp = c(1, 2, 4, 10, 14, 18, 20, 21, 24, 25, 28, 29),
#'    lon = c(5.1299311, 5.129979, 5.129597, 5.130028, 5.130555, 5.131083,
#'            5.132101, 5.132704, 5.133326, 5.133904, 5.134746, 5.135613),
#'    lat = c(52.092839, 52.092827, 52.092571, 52.092292, 52.092076, 52.091821,
#'            52.091420, 52.091219, 52.091343, 52.091651, 52.092138, 52.092698))
#' # First generate segments
#' res100 <- tdtr(df,
#'      group_col = NULL,
#'      max_error = 100)
#' # Then extract a data.table of segments
#' getSegsExtra(res100)

getSegsExtra <- function(data, coord.type = c("coordinate", "distance", "both"), group = FALSE){
  coord.type <- match.arg(coord.type, c("coordinate", "distance",
                                        "both"))

  # Fix 'no visible vinding for global variable' https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  bearing = nextbearing = segment_end = nbdiff = distnext = ttn = lat = lon =
    ttn = timestamp = calcmps = avgmps = maxmps = varmps = perc0mps = avgnbdif =
    varnbdif = rog = bearvar = Nbelow3 = id = entity_id = seg_start_lon =
    seg_start_lat = seg_start_time = seg_end_lon = seg_end_lat = seg_end_time =
    segdist = NBelow3 = .id = NULL

  if (group == TRUE) {
    data[, bearing := bearing(as.matrix(.SD)) + 180, .SDcols = c("lon", "lat")]
    data[, nextbearing := shift(bearing, -1), .(.id, segment_id)]
    data[, nbdiff := pmin(abs(nextbearing - bearing),
                          360 - abs(nextbearing - bearing)) / 180]

    data[, distnext := shift(geodist::geodist_vec(lon, lat, sequential = TRUE, pad = TRUE, measure = "haversine"), -1), .(.id)]
    data[, ttn := lubridate::time_length(lubridate::as.duration(shift(timestamp, -1) - timestamp), "seconds"), .(id)]
    data[, calcmps := (distnext)/ttn]

    data[, `:=`(
      avgmps   = mean(calcmps, na.rm = TRUE),
      maxmps   = max(calcmps, na.rm = TRUE),
      varmps   = var(calcmps, na.rm = TRUE),
      perc0mps = sum(calcmps == 0, na.rm = TRUE)/.N,
      avgnbdif = mean(nbdiff, na.rm = TRUE),
      varnbdif = var(nbdiff, na.rm = TRUE),
      rog      = radiusOfGyrationDT(lat, lon, timestamp),
      bearvar  = circularDispersion(na.omit(bearing)),
      NBelow3       = .N < 3
    ), .(segment_id, .id)]

    segs <- data[(segment_start)]
    set(segs,
        j = "segdist",
        value = geodist(cbind(lon = segs[["seg_start_lon"]], lat = segs[["seg_start_lat"]]),
                        cbind(lon = segs[["seg_end_lon"]], lat = segs[["seg_end_lat"]]),
                        paired = TRUE, measure = "haversine"))

    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist,
                            avgnbdif, varnbdif, bearvar, avgmps, maxmps, varmps,
                            perc0mps, NBelow3, rog, .id, entity_id, segment_id)])
  } else {
    data <- copy(data)

    data[, bearing := bearing(as.matrix(.SD)) + 180, .SDcols = c("lon", "lat")]
    data[, nextbearing := shift(bearing, -1), segment_id]
    data[, nbdiff := pmin(abs(nextbearing - bearing),
                          360 - abs(nextbearing - bearing)) / 180]
    data[, distnext := shift(geodist::geodist_vec(lon, lat, sequential = TRUE, pad = TRUE, measure = "haversine"), -1)]
    data[, ttn := lubridate::time_length(lubridate::as.duration(shift(timestamp, -1) - timestamp), "seconds")]
    data[, calcmps := (distnext)/ttn]

    data[, `:=`(
      avgmps   = mean(calcmps, na.rm = TRUE),
      maxmps   = max(calcmps, na.rm = TRUE),
      varmps   = var(calcmps, na.rm = TRUE),
      perc0mps = sum(calcmps == 0, na.rm = TRUE)/.N,
      avgnbdif = mean(nbdiff, na.rm = TRUE),
      varnbdif = var(nbdiff, na.rm = TRUE),
      rog      = radiusOfGyrationDT(lat, lon, timestamp),
      bearvar  = circularDispersion(na.omit(bearing)),
      NBelow3       = .N < 3
    ), segment_id]

    segs <- data[(segment_start)]
    set(segs,
        j = "segdist",
        value = geodist(cbind(lon = segs[["seg_start_lon"]], lat = segs[["seg_start_lat"]]),
                        cbind(lon = segs[["seg_end_lon"]], lat = segs[["seg_end_lat"]]),
                        paired = TRUE, measure = "haversine"))


    segs <- unique(segs[, .(seg_start_lon, seg_start_lat, seg_start_time,
                            seg_end_lon, seg_end_lat, seg_end_time, segdist,
                            avgnbdif, varnbdif, bearvar, avgmps, maxmps, varmps,
                            perc0mps, NBelow3, rog, entity_id, segment_id)])
  }
  if (coord.type == "coordinate") {
    return(segs[])
  }
  else {
    convertCoordsToDist(segs, c("seg_start_lat", "seg_end_lat"))
    convertCoordsToDist(segs, c("seg_start_lon", "seg_end_lon"))
  }
  if (coord.type == "both") {
    return(segs[])
  }
  else if (coord.type == "distance") {
    set(segs, j = c("seg_start_lat", "seg_start_lon", "seg_end_lat",
                    "seg_end_lon"), value = NULL)
    segs[]
  }
}

## Add error to this function

## Add segment id

## fix segment dist
