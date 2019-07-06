
#' Add together two numbers

#' @name gapfill.sapflux
#' @title Gapfill sap flux data.
#' @description Gapfill sap flux data using a spline fit.
#' @usage   gapfill.sapflux( LDate, y, max.gap.size=20 )
#' @param   LDate Linear day of year, such as AquaFlux output sapflux.data$LDate.
#' @param   sapflux.ts Time series of sapflux data, Example: sapflux.data exported by AquaFlux.
#' @param   max.gap.size Optional input.  Maximum gap size that will be filled.  Default is 20.  See details.
#' @return   Output is a time series of sapflux data, now gapfilled.
#' @details   max.gap.size =  Spline gapfilling is best suited to fill small data gaps.  "max.gap.size" allows selectively only fill data gaps smaller than "max.gap.size" time indexes.  For example, if your data was recorded hourly, max.gap.size=5 will not allow gaps larger than 5 hours to be gapfilled.  If your data was recorded every 5 minutes, max.gap.size=6 will only will not allow gaps larger than 30 minutes (5 mins * 6) to be filled.
#' @export

#####################################################################
#             Post-processing: gapfill                      ##
#####################################################################

gapfill.sapflux = function(LDate,sapflux.ts, max.gap.size=20) {
  y = sapflux.ts
  # remove na for now
  cc = !is.na(y)
  LDate.clean =  LDate[cc]
  y.clean =  y[cc]
  # use a spline to fit the entire series
  s=spline(x=LDate.clean, y=y.clean, xout=LDate)
  y.all.filled = s$y
  # decect whcih values are missing in gaps smaller than max.gap.size
  fill.these.i = .gaps.smaller.than.this.size(y,max.gap.size)
  # fill those indexs in
  y[fill.these.i==T] = y.all.filled[fill.these.i==T]
  y[y<0]=0
  # export
  y
}
