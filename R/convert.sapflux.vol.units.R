
#' @name convert.sapflux.vol.units
#' @title  Convert Sapflux Volume Units
#' @description Calculate whole tree sap flux, accounting for radial trends.
#' @usage   convert.sapflux.vol.units(sapflux.data, starting.vol.units, desired.vol.units)
#' @param   sapflux.data A time series of sap flux data, such as a column from AquaFlux output "sapflux.data".  Units must be in ( vol/m2 of sapwood /time ).
#' @param   starting.vol.units Existing volume units. "g" = grams, "cm3"=centimeters cubed, "m3"=meters cubed, "L"=litters, "gal"=gallons.
#' @param   desired.vol.units Desired volume units, same conventions as starting.vol.units.
#' @return   Output is a time series of sap flux data in desired volume units.
#' @export

convert.sapflux.vol.units = function( sapflux.data, starting.vol.units, desired.vol.units ){
  # check for valid units
  valid.units = c("g","cm3","m3","L","gal")
  if (sum(valid.units==starting.vol.units)!=1){ stop("Invalid starting.vol.units")}
  if (sum(valid.units==desired.vol.units)!=1){ stop("Invalid desired.vol.units")}


  # remove cm3 as an option (because cm3=g)
  if (starting.vol.units==("cm3")){ starting.vol.units="g"}
  if (desired.vol.units==("cm3")){ desired.vol.units="g"}

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert current units (starting.time.units) to standard (g)
  if (starting.vol.units=="g"){   x = x}
  if (starting.vol.units=="m3"){ x = x*10^(+6) }
  if (starting.vol.units=="L"){  x = x*1000 }
  if (starting.vol.units=="gal"){ x = x*3785.41 }

  # convert standard units (s) to disired (disired.time.units)
  if (desired.vol.units=="g"){   x = x}
  if (desired.vol.units=="m3"){ x = x/10^(+6) }
  if (desired.vol.units=="L"){  x = x/1000 }
  if (desired.vol.units=="gal"){ x = x/3785.41 }

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}
