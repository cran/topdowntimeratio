library(topdowntimeratio)
library(testthat)
library(data.table)


df <- data.frame(
  entity_id = c(1, 1, 1),
  timestamp = c(1, 2, 3),
  lat = c(2, 3, 4),
  lon = c(5, 6, 2))

dt1iter <- tdtr(data.frame(tsk = c(1, 1, 1),
                           foo = c(1, 2, 3),
                           bar = c(2, 3, 4),
                           bla = c(5, 6, 2)),
                col_names = list(entity_id_col = "tsk",
                                 timestamp_col = "foo",
                                 latitude_col = "bar",
                                 longitude_col = "bla"),
                n_segs = 1,
                group_col = NULL)

dt2iter <- tdtr(data.frame(tsk = c(1, 1, 1),
                           foo = c(1, 2, 3),
                           bar = c(2, 3, 4),
                           bla = c(5, 6, 2)),
                col_names = list(entity_id_col = "tsk",
                                 timestamp_col = "foo",
                                 latitude_col = "bar",
                                 longitude_col = "bla"),
                n_segs = 2,
                group_col = NULL)


test_that("tdtr returns a data.table",{
  expect_false("data.table" %in% class(df))
  expect_true("data.table" %in% class(tdtr(df, group_col = NULL)))
})

test_that("data col names are correctly set",{
  dt <- tdtr(data.frame(tsk = c(1, 1, 1),
                        foo = c(1, 2, 3),
                        bar = c(2, 3, 4),
                        bla = c(5, 6, 2)),
             col_names = list(entity_id_col = "tsk",
                              timestamp_col = "foo",
                              latitude_col = "bar",
                              longitude_col = "bla"),
             n_segs = 1,
             group_col = NULL)
  testthat::expect_equal(dt$timestamp, c(1, 2, 3))
  testthat::expect_equal(dt$lat, c(2, 3, 4))
  testthat::expect_equal(dt$lon, c(5, 6, 2))

})

test_that("iterate produces valid segments",{
  # Arrange
  dt <- data.table(entity_id = c(1, 1, 1, 1), lat = c(1, 1, 1, 1), lon = c(1, 1.5, 4, 1.5), timestamp = c(100, 200, 300, 400))
  dt <- setup(data.table(entity_id = c(1, 1, 1, 1), lat = c(1, 1, 1, 1), lon = c(1, 1.5, 4, 1.5), timestamp = c(100, 200, 300, 400)))

  # Act
  iterate(dt, max_error = 30)

  # Assert
  expect_equal(dt$segment_start, c(TRUE, FALSE, TRUE, FALSE))
  expect_equal(dt$segment_end, c(FALSE, FALSE, TRUE, TRUE))
  expect_equal(dt$segment_id, c(1, 1, 2, 2))
})

test_that("iterate calculates distance and picks max distance correctly",{
  dt <- setup(data.table(entity_id = c(1, 1, 1, 1, 1, 1), lat = c(1, 1, 1, 1, 1, 1), lon = c(1, 1.5, 4, 1.5, 2, 2), timestamp = c(100, 200, 300, 400, 500, 600)))
  iterate(dt, max_error = 30)
  updateSegs(dt)

  # Set up two data frames to test distances
  dfx <- data.frame(longitude = dt$lon, latitude = dt$lat)
  dfy <- data.frame(longitude = dt$adjusted_lon, latitude = dt$adjusted_lat)
  expect_equal(dt$dist, geodist(dfx, dfy, paired = TRUE, measure = "haversine"))


  # Find current segment endpoints
  seg_rows          <- which(dt[, segment_start == TRUE])
  # Find current max distance within each segment
  rows_with_max_dist <- which(dt[, dist == max(dist), segment_id]$V1)
  # Iterate once
  iterate(dt, max_error = 30)
  # Check which row is new endpoint
  new_seg_rows      <- which(dt[, segment_start == TRUE])
  new_seg           <- new_seg_rows[!new_seg_rows %in% seg_rows]

  expect_equal(rows_with_max_dist, new_seg)

})

test_that("iterate takes the right segment info",{
  dt <- setup(data.table(entity_id = c(1, 1, 1, 1, 1), lat = c(1, 1, 1, 1, 1), lon = c(1, 1.5, 4, 1.5, 2), timestamp = c(100, 200, 300, 400, 500)))
  iterate(dt, max_error = 30)

  expect_equal(dt$seg_start_time, c(100, 100, 300, 300, 300))
  expect_equal(dt$seg_end_time, c(300, 300, 500, 500, 500))
  expect_equal(dt$seg_start_lon, c(1, 1, 4, 4, 4))
})

test_that("iterate calculates dur and perc correctly",{
  dt <- setup(data.table(entity_id = c(1, 1, 1, 1, 1), lat = c(1, 1, 1, 1, 1), lon = c(1, 1.5, 4, 1.5, 2), timestamp = c(100, 200, 300, 600, 900)))
  iterate(dt, max_error = 30)
  updateSegs(dt)
  df1 <- data.frame(timestamp_start = dt[segment_start == TRUE, timestamp_numeric],
                   N = dt[, .N, segment_id][, N],
                   timestamp_end = dt[segment_end == TRUE, timestamp_numeric])
  df2 <- data.frame(timestamp = dt$timestamp_numeric,
                    duration = dt$seg_dur,
                    start_time = dt$seg_start_time)
  expect_equal(dt$seg_dur, rep(df1$timestamp_end - df1$timestamp_start, df1$N))
  expect_equal(dt$perc_of_seg_dur, (df2$timestamp - df2$start_time)/df2$duration)
  })

test_that("iterate respects max_err distance",{
  ## todo
})
