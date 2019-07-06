
#' Add together two numbers
#'
#' @name convert.sapflux.gperm2ground.to.mm
#' @title Convert sap flux units to mm
#' @description This function converts sap flux from units of ( g / m2 ground area / time ) to ( mm / time)
#' @usage   convert.sapflux.gperm2ground.to.mm(  sapflux.data, sapwood.per.ground.area )
#' @param   sapflux.data A data.frame or matrix of sap flux data.  Units MUST include m2 ground area.
#' @param   sapwood.per.ground.area Sapwood per ground area.  Must be in units (cm2 sapwood) / (m2 ground)
#' @return   Output is a time series of sapflux data, converted to units mm / time.
#' @export



convert.sapflux.gperm2ground.to.mm = function( sapflux.data, sapwood.per.ground.area ){
  # sapflux.data MUST
  # sapwood.per.ground.area MUST be in (cm2 sapwood / m2 ground)
  # (m2 sapwood / hectare ground) == (cm2 sapwood / m2 ground)

  # units check
  # given units:  g
  #               m2ground time
  # disired units: mm
  #                time
  # Needed conversion: g   to  mm
  #                    m2        1
  # Work: 1g   * cm3 *   1m2         * 10mm   ->
  #       1cm3 * m2      100^2cm2      1cm    ->
  # -> 10   -> 10    -> 1
  # -> 100^2-> 10000 -> 1000

  # find sapflux cols to convert
  cols.to.convert = which(   names(sapflux.data)!="TIMESTAMP" & names(sapflux.data)!="LDate" )
  # make a smaller frame of just the sap flux data
  x = sapflux.data[, cols.to.convert]

  # convert
  x = x * ( sapwood.per.ground.area) / (1000 )

  # merge with old frame
  sapflux.data.converted = sapflux.data
  sapflux.data.converted[, cols.to.convert] = x

  # export
  sapflux.data.converted
}
