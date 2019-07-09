\name{gapfill.sapflux}
\alias{gapfill.sapflux}
\title{
Gapfill sap flux data.
}
\description{
Gapfill sap flux data using a spline fit.
}
\usage{
gapfill.sapflux( LDate, y, max.gap.size=20 )
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{LDate}{Linear day of year, such as AquaFlux output sapflux.data$LDate.}
  \item{y}{Time series of sap flux data such as from sapflux.data.}
  \item{max.gap.size}{Optional input.  Maximum gap size that will be filled.}
}
\details{
  max.gap.size =  Spline gapfilling is best suited to fill small data gaps.  "max.gap.size" allows selectively only fill data gaps smaller than "max.gap.size" time indexes.  For example, if your data was recorded hourly, max.gap.size=5 will not allow gaps larger than 5 hours to be gapfilled.  If your data was recorded every 5 minutes, max.gap.size=6 will only will not allow gaps larger than 30 minutes (5 mins * 6) to be filled.
}
\value{
Returns a vector of gapfilled sap flux data, the same size as the "y" input.
}
\author{
  Heather Speckman (HeatherNSpeckman@gmail.com)
}
