
#' Add together two numbers

#' @name radial.trends
#' @title Radial trends in sap flux
#' @description Calculate whole tree sap flux, accounting for radial trends.
#' @usage   radial.trends ( sapflux1, sapflux2, sensor.depth1, sensor.depth2, DBH, sapwood.depth, reg.type)
#' @param   sapflux1 A time series of sap flux data, such as a column from AquaFlux output "sapflux.data".  Units must be in ( vol/m2 of sapwood /time ).
#' @param   sapflux2 A second times series of sap flux data, from the same tree as sapflux1.
#' @param   sensor.depth1 The depth (cm) TDP sensor 1 is inserted into the tree.
#' @param   sensor.depth2 The depth (cm) TDP sensor 2 is inserted into the tree.
#' @param   DBH The diamater at breast hieght (1.37m) of the tree. DBH units: cm.
#' @param   sapwood.depth The maximum depth of tree sapwood in cm.
#' @param   reg.type Regression type relating the two measurements.  Options are "mean"=average, "linear"=linear, "gaus"=Gaussian.
#' @return   Output is a time series of average sap flow after accounting for radial trends.  Units are (vol / time / whole tree).  For example (g of water / s / tree).
#' @details This function account for radial trends in sap flux by modeling the relationship between sap flux at two observed different depths.  This relationship can be an average value, a linear regression, or a Gaussian regression.  The relationship is then plotted, both in terms of sap flux (vol/m2 of sapwood /time) and sap flow (vol/time) after accounting for different sapwood areas. Funciton output is a time series of average sap flow after accounting for radial trends.
#' @export



radial.trends = function( sapflux1, sapflux2,
                          sensor.depth1, sensor.depth2,
                          DBH, sapwood.depth,
                          reg.type ){

  # check for valid reg.type
  valid.reg.type = c("mean","gaus","linear")
  if (sum(valid.reg.type==reg.type)!=1){ stop("Invalid reg.type")}

  # calculate sapwood ring area and seq of sensor "depths"
  sapwood.ring.area = .calc.sapwood.ring.area( DBH, sapwood.depth)
  sensor.depth.seq = .calc.sensor.depth( DBH, sapwood.depth)

  if (reg.type=="gaus"){
    s.out = .radial.profile.Gaussian( sapflux1, sapflux2,sensor.depth1, sensor.depth2,
                                      DBH, sapwood.depth, sapwood.ring.area, sensor.depth.seq)
  }
  if (reg.type=="linear"){
    s.out = .radial.profile.linear( sapflux1, sapflux2,sensor.depth1, sensor.depth2,
                                    DBH, sapwood.depth, sapwood.ring.area, sensor.depth.seq)
  }
  if (reg.type=="mean"){
    s.out = .radial.profile.mean( sapflux1, sapflux2,sensor.depth1, sensor.depth2,
                                  DBH, sapwood.depth, sapwood.ring.area, sensor.depth.seq)
  }

  # plot it
  .radial.profile.plot(s.out, sensor.depth.seq,sapflux1, sapflux2,
                       sensor.depth1, sensor.depth2)

  # export
  s.out$sapflow.avg.over.time
}

