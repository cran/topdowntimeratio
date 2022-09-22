singleSegmentSynchError <- function(res, tol = 1e-24){

  # Fix 'no visible vinding for global variable' https://github.com/Rdatatable/data.table/issues/850#issuecomment-259466153
  lat1 = lat = lon1 = lon = lat2 = lon2 = t1 = timestamp_numeric = t2 = alat1 =
    adjusted_lat = alon1 = adjusted_lon = alat2 = alon2 = dx1 = dy1 = dx2 =
    dy2 = c1 = c2 = c3 = c4 = discr = case = segment_err = t1distforint =
    t2distforint = t1int = t2int = NULL

  res[, `:=`(lat1, lat)]
  res[, `:=`(lon1, lon)]
  res[, `:=`(lat2, shift(lat, -1))]
  res[, `:=`(lon2, shift(lon, -1))]
  res[, `:=`(t1, (timestamp_numeric - timestamp_numeric[1]))]
  res[, `:=`(t2, shift(t1, -1))]
  res[, `:=`(alat1, adjusted_lat)]
  res[, `:=`(alon1, adjusted_lon)]
  res[, `:=`(alat2, shift(adjusted_lat, -1))]
  res[, `:=`(alon2, shift(adjusted_lon, -1))]
  res[, `:=`(dx1, lon1 - alon1)]
  res[, `:=`(dy1, lat1 - alat1)]
  res[, `:=`(dx2, lon2 - alon2)]
  res[, `:=`(dy2, lat2 - alat2)]
  res[, `:=`(c1, ((dx1 - dx2)^2 + (dy1 - dy2)^2))]
  res[, `:=`(c2, 2 * ((dx2 * t1 - dx1 * t2) * (dx1 -
                                                 dx2) + (dy2 * t1 - dy1 * t2) * (dy1 - dy2)))]
  res[, `:=`(c3, (dx2 * t1 - dx1 * t2)^2 + (dy2 * t1 -
                                              dy1 * t2)^2)]
  res[, `:=`(c4, t2 - t1)]
  res[, `:=`(discr, c2^2 - 4 * c1 * c3)]
  force(res)
  res[, `:=`(case, NULL)]
  res[, segment_err := NULL]
  res[c1 < tol, `:=`(case, "directseg")]
  res[c1 < tol, `:=`(segment_err, sqrt(c3)/(c4))]

  res[c1 > tol & abs(discr) < 1e-20, case := "case2"]
  res[, t1distforint := c1 * t1^2 + c2 * t1 + c3]
  res[, t2distforint := c1 * t2^2 + c2 * t2 + c3]

  res[abs(t1distforint) < 1e-12, t1distforint := 0]
  res[abs(t2distforint) < 1e-12, t2distforint := 0]
  res[is.na(sqrt(t1distforint))]
  res[c1 > tol,
      t1int := ((2 * c1 * t1 + c2)/(4 * c1)) * sqrt(t1distforint)]

  res[c1 > tol,
      t2int := ((2 * c1 * t2 + c2)/(4 * c1)) * sqrt(t2distforint)]


  res[c1 > tol, `:=`(segment_err,
                     1/c4^2 * abs((t2int) - (t1int)))]

  res[discr <  tol & c1 > tol & abs(dx1) <  tol & abs(dy1) <  tol, `:=`(case,
                                                                        "sharedendpoint")]
  res[discr < tol & c1 > tol & abs(dx1) < tol & abs(dy1) < tol, `:=`(segment_err,
                                                                     0.5 * sqrt(dx2^2 + dy2^2))]
  res[discr < tol & c1 > tol & abs(dx2) < tol & abs(dy2) < tol, `:=`(case,
                                                                     "sharedendpoint")]
  res[discr < tol & c1 > tol & abs(dx2) < tol & abs(dy2) < tol, `:=`(segment_err,
                                                                     0.5 * sqrt(dx1^2 + dy1^2))]
  res[discr < 0 & is.na(case), `:=`(case, "general")]
  res[is.na(case), `:=`(case, "general")]

  res[case == "general", `:=`(t1int,
                              ((2 * c1 * t1 + c2)/(4 * c1)) * sqrt(t1distforint) - discr * asinh((2 * c1 * t1 + c2)/sqrt(4 *
                                                                                                                           c1 * c3 - c2^2)))]
  res[case == "general", `:=`(t2int,
                              ((2 * c1 * t2 + c2)/(4 * c1)) * sqrt(t2distforint) - discr * asinh((2 * c1 * t2 + c2)/sqrt(4 *
                                                                                                                           c1 * c3 - c2^2)))]
  res[ case == "general", `:=`(segment_err,
                               1/c4^2 * abs((t2int) - (t1int)))]
  cols.to.remove <- c("c1", "c2", "c3", "c4",
                      "t1", "t2", "dx1", "dx2", "dy1",
                      "dy2", "discr", "t1int", "t2int",
                      "alat1", "alat2", "alon1", "alon2")
  set(res, j = cols.to.remove, value = rep(NULL, length(cols.to.remove)))[]
}


# singleSegmentSynchError <- function(res, tol = 1e-24){
#   res[, lat1 := lat]
#   res[, lon1 := lon]
#   res[, lat2 := shift(lat, -1)]
#   res[, lon2 := shift(lon, -1)]
#   # res[, t1 := timestamp_numeric]
#   res[, t1 := (timestamp_numeric - timestamp_numeric[1])]
#   res[, t2 := shift(t1, -1)]
#   res[, alat1 := adjusted_lat]
#   res[, alon1 := adjusted_lon]
#   res[, alat2 := shift(adjusted_lat, -1)]
#   res[, alon2 := shift(adjusted_lon, -1)]
#
#   res[, dx1 := lon1 - alon1]
#   res[, dy1 := lat1 - alat1]
#   res[, dx2 := lon2 - alon2]
#   res[, dy2 := lat2 - alat2]
#
#   res[, c1 := ((dx1 - dx2)^2 + (dy1 - dy2)^2)]
#   res[, c2 := 2 * ((dx2 *t1 - dx1*t2) * (dx1-dx2) + (dy2*t1 - dy1*t2) * (dy1-dy2))]
#   res[, c3 := (dx2*t1 - dx1*t2)^2 + (dy2*t1 - dy1*t2)^2]
#   res[, c4 := t2 - t1]
#   res[, discr := c2^2 - 4*c1*c3]
#   force(res)
#   res[, case := NULL]
#   res[c1 < tol, case := "directseg"]
#   res[c1 < tol, segment_err := sqrt(c3)/(c4)]
#
#   res[ discr < tol &
#         c1 > 0 &
#         dx1 < tol &
#         dy1 < tol,
#         case := "sharedendpoint"]
#
#   res[discr < tol &
#         c1 > 0 &
#         dx1 < tol &
#         dy1 < tol,
#         segment_err := .5*sqrt(dx2^2+dy2^2)]
#
#   res[discr < tol &
#          c1 > 0 &
#          dx2 < tol &
#          dy2 < tol,
#         case := "sharedendpoint"]
#   res[discr < tol &
#         c1 > 0 &
#         dx2 < tol &
#         dy2 < tol,
#         segment_err := .5*sqrt(dx1^2 + dy1^2)]
#
#   res[discr < 0 & is.na(case),
#         case := "general"]
#   res[discr < 0 & case == "general",
#         t1int := ((2 * c1 * t1 + c2)/(4*c1)) * sqrt(c1 * t1^2 + c2*t1 + c3) -
#           discr*asinh((2 * c1 * t1 + c2) / sqrt(4 * c1 * c3 - c2^2))]
#
#   res[discr < 0 & case == "general",
#         t2int := ((2 * c1 * t2 + c2)/(4*c1)) * sqrt(c1 * t2^2 + c2*t2 + c3) -
#           discr*asinh((2 * c1 * t2 + c2) / sqrt(4 * c1 * c3 - c2^2))]
#   res[discr < 0 & case == "general", segment_err := 1/c4^2 * ((t2int) - (t1int))]
#   cols.to.remove <- c("c1", "c2", "c3", "c4", "t1", "t2", "dx1", "dx2", "dy1", "dy2",
#                       "discr", "t1int", "t2int", "alat1", "alat2", "alon1", "alon2")
#   set(res, j = cols.to.remove,
#       value = rep(NULL, length(cols.to.remove)))[]
# }
