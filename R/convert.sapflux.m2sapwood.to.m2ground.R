
#' @name convert.sapflux.m2sapwood.to.m2ground
#' @title Convert sapflux area units
#' @description Convert sapflux area units from (m2 of sapwood) to (m2 of ground).
#' @usage     convert.sapflux.m2sapwood.to.m2ground( sapflux.data, sapwood.per.ground.area )
#' @param   sapflux.data A data.frame or matrix of sap flux data.  Units MUST include 1/m2sapwood
#' @param   sapwood.per.ground.area Sapwood per ground area.  Must be in units (cm2 sapwood) / (m2 ground)
#' @return   Output is a time series of sapflux data, converted to units of ground area (m2 of ground).
#' @export


convert.sapflux.m2sapwood.to.m2ground = function( sapflux.data, sapwood.per.ground.area ){
  # sapflux.data MUST be in units that include 1/m2sapwood
  # sapwood.per.ground.area MUST be in (cm2 sapwood / m2 ground)
  # (m2 sapwood / hectare ground) == (cm2 sapwood / m2 ground)

  # units check
  # Have: 1/m2sapwood
  # Disire  1/m2 ground
  # Work it: A = sapwood.per.ground.area
  # 1         * 1 m2sapwood      * A cm2sapwood ->
  # m2sapwood * 100^2 cm2sapwood * m2ground     -> 1 * 100^2 * m2ground
  # -> 1 * 1 * A        -> A
  # -> 1*100^2*m2ground -> 100^2

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert
  x = x * ( sapwood.per.ground.area) / (100^2 )

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}
