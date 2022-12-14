% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/topdowntimeratio-package.R
\docType{package}
\name{topdowntimeratio-package}
\alias{topdowntimeratio}
\alias{topdowntimeratio-package}
\title{topdowntimeratio: Top-Down Time Ratio Segmentation for Coordinate Trajectories}
\description{
Data collected on movement behavior is often in the form of time- stamped latitude/longitude coordinates sampled from the underlying movement behavior. These data can be compressed into a set of segments via the Top- Down Time Ratio Segmentation method described in Meratnia and de By (2004) \doi{10.1007/978-3-540-24741-8_44} which, with some loss of information, can both reduce the size of the data as well as provide corrective smoothing mechanisms to help reduce the impact of measurement error. This is an improvement on the well-known Douglas-Peucker algorithm for segmentation that operates not on the basis of perpendicular distances. Top-Down Time Ratio segmentation allows for disparate sampling time intervals by calculating the distance between locations and segments with respect to time. Provided a trajectory with timestamps, tdtr() returns a set of straight- line segments that can represent the full trajectory. McCool, Lugtig, and Schouten (2022) \doi{10.1007/s11116-022-10328-2} describe this method as implemented here in more detail.
}
\author{
\strong{Maintainer}: Danielle McCool \email{d.m.mccool@uu.nl} (\href{https://orcid.org/0000-0002-7055-7539}{ORCID})

}
\keyword{internal}
