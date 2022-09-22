library(topdowntimeratio)
library(testthat)
library(data.table)

df <- data.frame(
  entity_id = c(1, 1, 1, 1),
  lon = c(5, 5.01, 5.02, 5.05),
  lat = c(32.01, 32.04, 32.06, 32.10),
  timestamp = c(500, 600, 800, 2000)
  )

test_that("getSegments returns correct number of segments", {
  segs <- getSegments(tdtr(df, n_segs = 1, group_col = NULL))
  expect_equal(nrow(segs), 1)

  segs <- getSegments(tdtr(df, n_segs = 2, group_col = NULL))
  expect_equal(nrow(segs), 2)
})

test_that("segment distance is correct", {
  segs <- getSegments(tdtr(df, n_segs = 2, group_col = NULL))
  start.one <- df[1, c("lon", "lat")]
  end.one <- df[3, c("lon", "lat")]
  dist.one <- geodist::geodist(start.one, end.one, paired = TRUE, measure = "haversine")

  start.two <- df[3, c("lon", "lat")]
  end.two <- df[4, c("lon", "lat") ]
  dist.two <- geodist::geodist(start.two, end.two, paired = TRUE, measure = "haversine")

  expect_equal(segs$segdist, c(dist.one, dist.two))
  })

test_that("convertCoordsToDist returns correct colnames",{
  dt <- data.table(lat = c(1, 1.2, 1.4, 1.6),
                   seg_lat_start = c(1, 1, 1.4, 1.4),
                   seg_lat_end = c(1.4, 1.4, 1.6, 1.6))
  dt2 <- data.table(moo = c(1, 2, 3, 4),
                    foo = c(1, 3, 3, 4))

  expect_named(convertCoordsToDist(dt, "lat"),
               c("lat", "seg_lat_start", "seg_lat_end", "lat_dist"))
  expect_named(convertCoordsToDist(dt2, c("moo", "foo")),
               c("moo", "foo", "moo_dist", "foo_dist"))

})

test_that("getSegsExtra returns correct number of segments", {
  segs <- getSegsExtra(tdtr(df, n_segs = 1, group_col = NULL))
  expect_equal(nrow(segs), 1)

  segs <- getSegsExtra(tdtr(df, n_segs = 2, group_col = NULL))
  expect_equal(nrow(segs), 2)
})
