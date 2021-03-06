% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wetlandscape.R
\name{wetlandscape}
\alias{wetlandscape}
\title{Wetland landscape generator}
\usage{
wetlandscape(
  nq,
  w1_min,
  w1_max,
  w3_min,
  w3_max,
  w6_min,
  w6_max,
  w12_min,
  w12_max,
  w24_min,
  w24_max,
  p1_min,
  p3_min,
  p6_min,
  p12_min,
  p24_min,
  dc_min,
  dc_max,
  nwl_min,
  nwl_max
)
}
\arguments{
\item{nq}{Integer. Full name is quarter-section and it is the number of
quarter-section (160-acre) farmlands in the sub-basin. The minimum number of quarter sections must be 46.}

\item{w1_min}{Integer. It is the minimum wetland size (ha) in the tier 1 group of wetlands in the sub-basin.}

\item{w1_max}{Integer. It is the maximum wetland size (ha) in the tier 1 group of wetlands in the sub-basin.}

\item{w3_min}{Integer. It is the minimum wetland size (ha) in the tier 3 group of wetlands in the sub-basin.}

\item{w3_max}{Integer. It is the maximum wetland size (ha) in the tier 3 group of wetlands in the sub-basin.}

\item{w6_min}{Integer. It is the minimum wetland size (ha) in the tier 6 group of wetlands in the sub-basin.}

\item{w6_max}{Integer. It is the maximum wetland size (ha) in the tier 6 group of wetlands in the sub-basin.}

\item{w12_min}{Integer. It is the minimum wetland size (ha) in the tier 12 group of wetlands in the sub-basin.}

\item{w12_max}{Integer. It is the maximum wetland size (ha) in the tier 12 group of wetlands in the sub-basin.}

\item{w24_min}{Integer. It is the minimum wetland size (ha) in the tier 24 group of wetlands in the sub-basin.}

\item{w24_max}{Integer. It is the maximum wetland size (ha) in the tier 24 group of wetlands in the sub-basin.}

\item{p1_min}{Integer. It is the minimum probability of harvesting in the tier 1 group of wetlands in the sub-basin.
It is between 0 and 1.}

\item{p3_min}{Integer. It is the minimum probability of harvesting in the tier 3 group of wetlands in the sub-basin.
It is between 0 and 1.}

\item{p6_min}{Integer. It is the minimum probability of harvesting in the tier 6 group of wetlands in the sub-basin.
It is between 0 and 1.}

\item{p12_min}{Integer. It is the minimum probability of harvesting in the tier 12 group of wetlands in the sub-basin.
It is between 0 and 1.}

\item{p24_min}{Integer. It is the minimum probability of harvesting in the tier 24 group of wetlands in the sub-basin.
It is between 0 and 1.}

\item{dc_min}{Integer. It is the minimum drainage cost of wetlands, $/ha.}

\item{dc_max}{Integer. It is the maximum drainage cost of wetlands, $/ha.}

\item{nwl_min}{Integer. The minimum number of wetlands.}

\item{nwl_max}{Integer. The maximum number of wetlands.}
}
\value{
The output is a dataframe of wetland characteristics in the sub-basin.
}
\description{
Wetland landscape generator
}
\examples{
wetlandscape(nq = 14000, w1_min = 0.1, w1_max = 1,
w3_min = 0.5, w3_max = 2, w6_min = 3, w6_max = 5, w12_min=3, w12_max=4,
w24_min=2, w24_max=6,p1_min=0.6, p3_min=0.5, p6_min=0.6, p12_min=0.7, p24_min = .9, dc_min = 500, dc_max = 1000,
nwl_min =2, nwl_max=4)
}
