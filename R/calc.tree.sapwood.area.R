
#' @name calc.tree.sapwood.area
#' @title Calculate individual tree sapwood area in cm2
#' @description Calculate whole tree sap flux, accounting for radial trends.
#' @usage   calc.tree.sapwood.area(DBH, sapwood.depth)
#' @param   DBH The diamater at breast hieght (1.37m) of the tree. DBH units: cm
#' @param   sapwood.depth The maximum depth of tree sapwood in cm.
#' @param   sensor.depth1 The depth (cm) TDP sensor 1 is inserted into the tree.
#' @param   sensor.depth2 The depth (cm) TDP sensor 2 is inserted into the tree.
#' @param   DBH The diamater at breast hieght (1.37m) of the tree. DBH units: cm.
#' @param   sapwood.depth The maximum depth of tree sapwood in cm.
#' @param   reg.type Regression type relating the two measurements.  Options are "mean"=average, "linear"=linear, "gaus"=Gaussian.
#' @return   Output is a time series of average sap flow after accounting for radial trends.  Units are (vol / time / whole tree).  For example (g of water / s / tree).
#' @export


calc.tree.sapwood.area = function(DBH, sapwood.depth){
  BA.total = pi * (DBH/2)^2
  inner.diam = DBH - sapwood.depth*2
  BA.heartwood = pi * (inner.diam/2)^2
  sapwood.area = BA.total-BA.heartwood
  sapwood.area
}
