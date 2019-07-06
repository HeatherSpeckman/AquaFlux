
#' @name convert.sapflux.time.units
#' @title Convert Sapflux Time Units
#' @usage   radial.trends ( sapflux.data, starting.time.units, desired.time.units)
#' @param   sapflux.data A data.frame or matrix of sap flux data.  Product from AquaFlux Shiny App.
#' @param   starting.time.units Existing time units. "s" = second, "min" = minute, "hr" = hour, "day" = day
#' @param   desired.time.units  Desired time units.  Same guidelines as starting.time.units.
#' @return   Output is a time series of sap flux data, now matching the desired time units.
#' @details This function account for radial trends in sap flux by modeling the relationship between sap flux at two observed different depths.  This relationship can be an average value, a linear regression, or a Gaussian regression.  The relationship is then plotted, both in terms of sap flux (vol/m2 of sapwood /time) and sap flow (vol/time) after accounting for different sapwood areas. Funciton output is a time series of average sap flow after accounting for radial trends.
#' @export


convert.sapflux.time.units = function( sapflux.data, starting.time.units, desired.time.units ){
  # valid time units:
  # s = second
  # min = minute
  # hr = hour
  # day = day

  # check for valid units
  valid.units = c("s","min","hr","day")
  if (sum(valid.units==starting.time.units)!=1){ stop("Invalid starting.time.units")}
  if (sum(valid.units==desired.time.units)!=1){ stop("Invalid desired.time.units")}

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert current units (starting.time.units) to standard (s)
  if (starting.time.units!="s"){
    if (starting.time.units=="min"){ x = x*60 }
    if (starting.time.units=="hr"){  x = x*60*60 }
    if (starting.time.units=="day"){ x = x*60*60*24 }
  }

  # convert standard units (s) to disired (desired.time.units)
  if (desired.time.units=="s"){   x = x}
  if (desired.time.units=="min"){ x = x/60 }
  if (desired.time.units=="hr"){  x = x/60/60 }
  if (desired.time.units=="day"){ x = x/60/60/24 }

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}
