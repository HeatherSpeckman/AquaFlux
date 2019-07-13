


#####################################################################
#####################################################################
#             Plotting Functions                              ##
#####################################################################
#####################################################################
# The below functions are used for plotting AquaFlux data.
# It is INTERNAL funcitons to display the data graphically.
# It does not direct UI stuff like the shiny::fluidpage
# No qaqc functions.


#####################################################################
# Plotting
#####################################################################


######################################################
# plot sub functions
.plot.mod.pick.pstuff = function(have.data,bx,mes,mLDate,pick.bottun){
  if (have.data){
    .plot.mod.pick.delete.p(bx)
    pick.bottun = pick.bottun
  }
  pick.bottun
}
.plot.mod.pick.delete.p = function(bx){
  # # if you have delete points picked
  if ( !is.null(bx[1])){
    abline(v = bx, col="seagreen",lwd=3)
  }
}
.plot.flip.var = function(y,is.dT){
  if (is.dT==T){
    yout = -y
  } else {
    yout = y
  }
  yout
}
.plot.get.ylim = function(y, extra.var, is.dT, v, Tmax.data.local){
  ##### Need to find the largest y bounds
  # canidate one:
  ylim = range(y, na.rm=T)
  # canidate two: raw  (if applicable):
  if ( is.dT==T & sum(extra.var=="raw")>0) {
    y.raw.bound = range( v$raw.data[v$cc.time,v$tree.number] , na.rm=T )
    if ( is.infinite(y.raw.bound[2])==T){y.raw.bound[1]=NA; y.raw.bound[2]=NA}
    possible.low = c(y.raw.bound[1], ylim[1])
    possible.high = c(y.raw.bound[2], ylim[2])
    ylim[1] = min( possible.low, na.rm=T) # get the lowest
    ylim[2] = max( possible.high, na.rm=T) # get the highest
  }
  # canidate three: Tmax (if applicable):
  if ( is.dT==T & sum(extra.var=="Tmax")>0 &
       Tmax.data.local$have.Tmax==T & sum(is.na(Tmax.data.local$points$Tmax))==F ){
    # get points
    y.Tmax.bound = range( Tmax.data.local$points$Tmax , na.rm=T)
    if ( is.infinite(y.Tmax.bound[2])==T){y.Tmax.bound[1]=NA; y.Tmax.bound[2]=NA}
    possible.low = c(y.Tmax.bound[1], ylim[1])
    possible.high = c(y.Tmax.bound[2], ylim[2])
    ylim[1] = min( possible.low, na.rm=T) # get the lowest
    ylim[2] = max( possible.high, na.rm=T) # get the highest
  }
  # canidate four: 0,1 (if missing all others):
  if ( is.infinite(ylim[1])==T | is.na(ylim[1])==T ){
    # get points
    ylim[1] = 0
    ylim[2] = 1
  }
  ylim
}
######################################################
# plot VPD
.draw.VPD.polgyon = function(x.VPD.thres){
  #  declare polygon borders
  y.bot = x.VPD.thres
  y.top = 0
  y.poly = c(y.top,y.top,y.bot,y.bot,y.top)
  x.bot = 0
  x.top = 400

  x.poly = c(x.bot,x.top,x.top,x.bot,x.bot)
  poly.col=  "wheat"
  polygon(x=x.poly,y=y.poly,col=poly.col,border=NA)
}
.plot.VPD = function(LDate.local, xlim, ylim, extra.var, is.dT, cc.time, VPD.thres, VPD){
  if ( sum(extra.var=="VPD")>0) {
    not.na = is.na(VPD)==F
    sum.na = sum(not.na, na.rm=T)
   # if (
    if (sum.na>0){
      # get data
      VPD = VPD[cc.time]
      # flip if this is dT
      x.VPD = .plot.flip.var(VPD,is.dT)
      x.VPD.thres = .plot.flip.var(VPD.thres,is.dT)
      if (is.na(x.VPD.thres)==T){ x.VPD.thres= -0.2 }
      if( is.dT==T) { VPD.lab = '-VPD (kPa)' } else { VPD.lab = 'VPD (kPa)' }
      # plot VPD axis
      par(new = T)
      plot(LDate.local, x.VPD, col=NA, axes=F, xlab=NA, ylab=NA)
      axis(side = 4); mtext(side = 4, line = 1.9, VPD.lab)
      # draw VPD polygon
      .draw.VPD.polgyon(x.VPD.thres)
      # VPD lines
      lines(LDate.local, x.VPD, lwd=2, col="cornflowerblue")
      # back to plotting dT points
      par(new = T); plot(NA, NA, xlim=xlim, ylim=ylim ,col=NA,axes=F, xlab=NA, ylab=NA )
    }
  }
}
######################################################
# plot AirT
.plot.AirTC = function(LDate.local, xlim, ylim, extra.var, is.dT, cc.time, AirTC){
  # did you want to plot AirTC?
  if ( sum(extra.var=="AirTC")>0) {
    # get data
    AirTC.local = AirTC[cc.time]

    # baisc plotting
    par(new = T)
    plot(LDate.local, AirTC.local, lwd=2, col= "red", type='l',
         axes=F, xlab=NA, ylab=NA)
    # if VPD not plotted, plot the axis
    if ( sum(extra.var=="VPD")==0) {
      axis(side = 4); mtext(side = 4, line = 1.9, 'Air Temp. (C)')
    }
    # back to plotting dT points
    par(new = T); plot(NA, NA, xlim=xlim, ylim=ylim, col=NA,axes=F, xlab=NA, ylab=NA )
  }
}
######################################################
# plot Tmax   #Tmax.data.local
.plot.Tmax = function(LDate.local, extra.var, is.dT, Tmax.data.local){
  # did you want to plot AirTC?
  if ( sum(extra.var=="Tmax")>0 & is.dT==T  & Tmax.data.local$have.Tmax==T) {  #
    if (sum(is.na(Tmax.data.local$line)==F)>0){
      lines(LDate.local, Tmax.data.local$line, lwd=4, col= "green")
      points(Tmax.data.local$points$LDate, Tmax.data.local$points$Tmax, cex=3, col="forestgreen", pch=19)
    }
  }
}
######################################################
# plot raw dT
.plot.raw.dT = function(LDate.local, extra.var, is.dT, cc.time, tree.number, raw.data){
  if ( sum(extra.var=="raw")>0 & is.dT==T) {
    raw.local =  raw.data[cc.time,tree.number]
    # baisc plotting
    par(new = T)
    plot(LDate.local, raw.local, lwd=1, col= "grey", type='l',
         axes=F, xlab=NA, ylab=NA)
  }
}
######################################################
# plot raw dT
.plot.suggest.QAQC = function(LDate.local, plotAutoQAQC, y, i.suggest ){
  if (  plotAutoQAQC==T & sum(i.suggest)>0 ) {
    # get suggest points
    LDate.local.sug = LDate.local[ i.suggest]
    y.sug = y[ i.suggest]
    points(LDate.local.sug, y.sug, col="red", cex=1.5, pch=19)
  }
}
######################################################
# plot basic
.plot.basic = function(v, is.dT, mes, y,
                       xlab,ylab, main.lab){
  start.time = Sys.time()
  v$time.last.save = .auto.save.check(v)
  LDate.local = v$LDate.local

  # get colors
  if (sum(v$pick.plot.options=="use.time.col")>0 ){  col = v$time.col[v$cc.time];  } else {  col=1; }

  # do you have data?
  have.data = sum(is.na(y)==F)>2 &  sum(is.infinite(y)==F)>2
  # get Tmax data
  Tmax.data.local = .Tmax.get.data(v$tree.name, v$Tmax.data, LDate.local)

  # draw basic empty graph
  par(mar=c(5, 4, 4, 3)+ 0.1  ) # make the right margin wider than default (for second axis)
  xlim= range(LDate.local, na.rm=T)
  ylim = .plot.get.ylim(y, v$pick.plot.options, is.dT, v, Tmax.data.local)
  plot(NA, NA, col=NA, xlab=xlab, ylab=ylab, type='l', xlim=xlim, ylim=ylim, main=main.lab)

  # plot extra, if revelant
  mLDate = mean(LDate.local)
  if ( have.data==T ) {
    # plot any extra variables
    try( .plot.AirTC(LDate.local, xlim, ylim, v$pick.plot.options, is.dT, v$cc.time, v$met.data$AirTC), silent = T)
    try( .plot.VPD(LDate.local, xlim, ylim, v$pick.plot.options, is.dT, v$cc.time, v$VPD.thres, v$met.data$VPD), silent = T)

    .plot.raw.dT(LDate.local, v$pick.plot.options, is.dT, v$cc.time, v$tree.number, v$raw.data)
    .plot.Tmax(LDate.local, v$pick.plot.options, is.dT, Tmax.data.local)

    # add dT
    lines(LDate.local, y , col=1, lwd=2)
    points(LDate.local, y, col=col, cex=0.5)
    .plot.suggest.QAQC(LDate.local, v$plotAutoQAQC, y, v$i.suggest )
    # plot buttons stuff
    pick.bottun = .plot.mod.pick.pstuff(have.data, v$bx, mes,
                                        mLDate, v$pick.bottun)
  } else {
    # message for empty
    text( x=mLDate, y=0.5,mes)
    pick.bottun= "none"
  }
  pick.bottun
}


######################################################
# plot big stuff
.plot.dT = function(v){
  # prep
  mes = "No dT data for this window"
  y = v$dT.local
  xlab = "Day of Year"
  ylab = "dT (C)"
  main.lab = paste("dT",v$tree.name)
  is.dT=T
  # plot
  if (is.numeric(v$dT.local)==T){
    pick.bottun = .plot.basic(v, is.dT, mes, y,
                              xlab,ylab, main.lab)
  } else {
    pick.bottun = v$pick.bottun
  }
  pick.bottun
}
.plot.sapflux = function(v){
  print("in .plot.sapflux")
  if (sum(v$pick.plot.options=="flux")>0){
      print("in .plot.sapflux 2")

    # prep
    mes = "No Sap Flux Data for this window"
    y = .sapflux.calc.local(v,v$tree.name)
    print("y summary")
    print(summary(y))
    xlab = "Day of Year"
    ylab = "Sap Flux (g m-2 s-1)"
    main.lab = paste("Sap Flux",v$tree.name)
    is.dT=F
    # plot
          print("in .plot.sapflux 3")

    pick.bottun = .plot.basic(v, is.dT, mes, y,
                              xlab,ylab, main.lab)
  }
        print("in .plot.sapflux 4")

}
######################################################
# move.over
.move.window=function(v){
  # find window size
  window.size.whole = v$max.DOY-v$min.DOY
  window.size.slim = window.size.whole * 0.90  # don't move 100% over, but 90% instead (so there's a little overlap)
  # actually move
  if (v$move.dir=="foward"){
    min.DOY=v$min.DOY+window.size.slim
    max.DOY=v$max.DOY+window.size.slim
    v$move.dir = "none"
  }
  if (v$move.dir=="back"){
    min.DOY=v$min.DOY-window.size.slim
    max.DOY=v$max.DOY-window.size.slim
    v$move.dir = "none"
  }
  # realistic check to make sure they don't go out of bounds
  if (min.DOY<v$min.DOY.global){min.DOY=v$min.DOY.global};
  if (max.DOY>v$max.DOY.global){max.DOY=v$max.DOY.global};
  if (max.DOY<(v$min.DOY.global+3) ){max.DOY=(v$min.DOY.global+3)};
  if (min.DOY>(v$max.DOY.global-3) ){min.DOY=(v$max.DOY.global-3)};
  # export
  v$min.DOY = min.DOY
  v$max.DOY = max.DOY
  v
}




#####################################################################
# AutoQAQC
#####################################################################

###############################
# Auto QAQC functions
#v = .autoQAQC.accept(v)
#v = .autoQAQC.suggest(v)
# v = .qaqc.excute(v,delete.cc)

###############################
#     Save or rejct commands #
###############################
.autoQAQC.keep = function(v){
  v$autoQAQCmes = "No changes made."
  v$i.suggest = NA
  v$plotAutoQAQC = F
  v = .autoQAQC.suggest(v)
  v
}
.autoQAQC.del = function(v){
  v$autoQAQCmes = "Points Deleted."
  v = .qaqc.excute(v, delete.cc=v$i.suggest)
  v = .autoQAQC.suggest(v)
  v
}
###############################
#     The filters themselves  #
###############################
.autoQAQC.FilterDTBelow = function(g,MinDT){
  y = g$dT.local
  got.is.na = is.na(y)==F
  got.thres= y < MinDT
  bad.points =  got.is.na==T & got.thres==T
  g$LastAutoFilter = "Below"
  if (sum(bad.points)>0){
    g$mes = "Red points are < min.dT.  Delete them?"
    g$bad.points = bad.points
    g$found.suggestion = T
  }
  g
}
.autoQAQC.FilterDTAbove = function(g,MaxDT){
  y = g$dT.local
  got.is.na = is.na(y)==F
  got.thres= y > MaxDT
  bad.points =  got.is.na==T & got.thres==T
  g$LastAutoFilter = "Above"
  if (sum(bad.points)>0){
    g$mes = "Red points are > max.dT.  Delete them?"
    g$bad.points = bad.points
    g$found.suggestion = T
  }
  g
}
.autoQAQC.FilterDev = function(g,DT.despike.dev, measurements.in.a.day){
  k = measurements.in.a.day*4
  if (k %% 2 == 0){k = k+1} # make it odd
  dT.despiked  =  oce::despike(g$dT.local, reference = c("smooth"), replace="NA", n=DT.despike.dev, k=)
  # find removed points
  was.na = is.na(g$dT.local)
  now.na = is.na(dT.despiked)
  changed.to.na = was.na==F & now.na==T
  g$LastAutoFilter = "Dev"
  if (sum(changed.to.na)>0){
    g$mes = "Red points are suspected spikes.  Delete them?"
    g$bad.points = changed.to.na
    g$found.suggestion = T
  }
  g
}
###############################
# Functions to look in this window   #
###############################
.autoQAQC.getData = function(tree.name, min.DOY, max.DOY,n.dT.data, LDate, dT.data, LastAutoFilter){
  g = list()
  g$tree.name = tree.name
  g$tree.number = match(tree.name,n.dT.data)
  g$min.DOY = min.DOY
  g$max.DOY = max.DOY
  x.cc.time = LDate>=g$min.DOY & LDate<=g$max.DOY;
  g$LDate.local = LDate[x.cc.time]
  g$dT.local = dT.data[x.cc.time,g$tree.number]
  g$have.data = sum( is.na(g$dT.local)==F )>0
  g$found.suggestion = F
  g$LastAutoFilter = LastAutoFilter
  g
}
.autoQAQC.checkForSugInWindow = function(g, MinDT, MaxDT, DT.despike.dev, measurements.in.a.day){
  # look for suggestion
  if ( g$LastAutoFilter=="none"  ) { g = .autoQAQC.FilterDTBelow(g,MinDT) }
  if ( g$LastAutoFilter=="Below" & g$found.suggestion == F ) { g = .autoQAQC.FilterDTAbove(g,MaxDT) }
  if ( g$LastAutoFilter=="Above" & g$found.suggestion == F ) { g = .autoQAQC.FilterDev(g,DT.despike.dev, measurements.in.a.day) }
  if (  g$found.suggestion == F ) {
    g$found.suggestion == T;
    g$mes = "No current suggestions";
    g$LastAutoFilter="none"
  }
  # export
  g
}
###############################
# Functions to get new window    #
###############################
.autoQAQC.LookThisWindow = function(NewWin,v){
  # get data (g)
  g = .autoQAQC.getData(NewWin$tree.name, NewWin$min.DOY, NewWin$max.DOY, n.dT.data=names(v$dT.data), v$LDate, v$dT.data, v$LastAutoFilter)
  # if you have data, look for suggestion
  if ( g$have.data ==T ){
    g = .autoQAQC.checkForSugInWindow(g, v$MinDT, v$MaxDT, v$DT.despike.dev, v$measurements.in.a.day)
  }
  # export
  g
}
###############################
#     Save it #
###############################
.autoQAQC.save.suggestion = function(g,v){
  v$min.DOY = g$min.DOY
  v$max.DOY = g$max.DOY
  v$tree.name = g$tree.name
  v$i.suggest = g$bad.points
  v$autoQAQCmes = g$mes
  v$LastAutoFilter  =  g$LastAutoFilter
  v$plotAutoQAQC = T
  v
}
###############################
#  Master .autoQAQC.suggest #
###############################
.autoQAQC.suggest = function(v){
  ########## meta changes
  # reset other bottons
  v$qaqc.manual="none";
  v$tmaxRadio="none";
  v$restore.option="none"; # clear other bottons

  ############ start with this window
  # make starting point in list "NewWin"
  NewWin = list()
  NewWin$tree.name = v$tree.name
  NewWin$window.size = 9999
  NewWin$min.DOY = v$min.DOY
  NewWin$max.DOY = v$max.DOY
  NewWin$all.done = F
  # actually look
  g = .autoQAQC.LookThisWindow(NewWin,v)
  # if you have a suggestion, save it
  v = .autoQAQC.save.suggestion(g,v)

  ######## export
  v
}



