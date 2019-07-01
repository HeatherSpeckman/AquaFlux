\name{radial.trends}
\alias{radial.trends}
\title{Radial trends in sap flux}
\description{Calculate whole tree sap flux, accounting for radial trends.}
\usage{
radial.trends(sapflux1, sapflux2,
              sensor.depth1, sensor.depth2,
              DBH, sapwood.depth,
              reg.type)
}
\arguments{
  \item{sapflux1}{A time series of sap flux data, such as a column from AquaFlux output "sapflux.data".  Units must be in ( vol/m2 of sapwood /time ).  }
  \item{sapflux2}{A second times series of sap flux data, from the same tree as sapflux1.  }
  \item{sensor.depth1}{The depth (cm) TDP sensor 1 is inserted into the tree.}
  \item{sensor.depth2}{The depth (cm) TDP sensor 2 is inserted into the tree. }
  \item{DBH}{The diamater at breast hieght (1.37m) of the tree. DBH units: cm. }
  \item{sapwood.depth}{The maximum depth of tree sapwood in cm. }
  \item{reg.type}{ Regression type relating the two measurements.  Options are "mean"=average, "linear"=linear, "gaus"=Gaussian. }
}
\details{
This function account for radial trends in sap flux by modeling the relationship between sap flux at two observed different depths.  This relationship can be an average value, a linear regression, or a Gaussian regression.  The relationship is then plotted, both in terms of sap flux (vol/m2 of sapwood /time) and sap flow (vol/time) after accounting for different sapwood areas. Funciton output is a time series of average sap flow after accounting for radial trends.
}
\value{
Output is a time series of average sap flow after accounting for radial trends.  Units are (vol / time / whole tree).  For example (g of water / s / tree).
}
\author{
  Heather Speckman (HeatherNSpeckman@gmail.com)
}
