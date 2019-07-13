
#####################################################################
# Save  data
#####################################################################


.save.one.file = function( data.to.save, tag, is.Tmax,v){
  study.year=v$study.year
  PDate=v$PDate
  save.dir=v$save.dir
  number.of.saves.to.keep=v$number.of.saves.to.keep

  # save it
  if (is.Tmax==T){
    d.clean = data.to.save
  } else{
    d.clean = cbind(PDate,data.to.save)
  }
  name1 = paste( study.year, tag, Sys.time()  )
  names(d.clean)
  name1 = gsub(":", ".", name1)
  write.csv(d.clean,   paste( name1, ".csv", sep="" ), row.names=F )

  # clean dir
  jj = paste(study.year, tag); .clean.save.dir(jj,study.year,PDate,save.dir,number.of.saves.to.keep)
}

.clean.save.dir = function(jj,study.year,PDate,save.dir,number.of.saves.to.keep){
  file.list = list.files(save.dir,recursive=T,jj)
  file.list = sort(file.list, decreasing=T)
  if ( length(file.list) > number.of.saves.to.keep ){
    delete.file.list = file.list[ (number.of.saves.to.keep+1) : (length(file.list))]
    for ( n in delete.file.list){ file.remove(n) }
  }
}

.save.AquaFlux= function(v){
  # save files
  setwd(v$save.dir)
  .save.one.file(v$dT.data, v$tag.dT.clean,F,v)
  .save.one.file(v$Tmax.data, v$tag.Tmax, T,v)
  time.last.save = Sys.time()
  #.save.one.file(sapflux.data, .tag.flux, F)  # Not going to bother saving this since it's constantly re-calced.
  #if (exists("flag.data")==T){ .save.one.file(flag.data, .tag.flag, F) }
  # save log
  time.last.save
}

.auto.save.check = function(v){ # this will auto-save your work every 5 turns
  current.time = Sys.time()
  time.diff = difftime( current.time, v$time.last.save, units=c("mins"))
  if ( time.diff > 1){
    print("auto saving  very frequently")
    time.last.save = .save.AquaFlux(v)
  } else {
    time.last.save = v$time.last.save
  }
  time.last.save
}



.save.site.metadata= function(v){
  # ####### Define & create the directory where you want your in-process data to be saved:
  # do you have a "/"
  v$AquaFlux.work.dir = .setup.fix.AquaFlux.work.dir(v$AquaFlux.work.dir)
  # Define the directory where you AquaFlux to save your data:
  v$save.dir = paste(v$AquaFlux.work.dir, "/Current Saves", sep="")
  # Define the directory where you want your final data to be saved:
  v$final.dir = paste(v$AquaFlux.work.dir, "/Final Data", sep="")
  # Define the directory where you want your final data plots to be stored.
  v$graph.dir = paste(v$AquaFlux.work.dir, "/Graphs", sep="")
  dir.create(v$save.dir, showWarnings = FALSE)
  dir.create(v$final.dir, showWarnings = FALSE)
  dir.create(v$graph.dir, showWarnings = FALSE)
  one.liners = data.frame(
    save.dir = v$save.dir,
    final.dir = v$final.dir,
    graph.dir = v$graph.dir,
    # met & units -changes
    met.dir = v$met.dir,
    dT.units = v$dT.units, # works
    met.air.temp.label = v$met.air.temp.label, #, crash
    met.air.temp.units = v$met.air.temp.units ,
    met.RH.label = v$met.RH.label,
    met.RH.units = v$met.RH.units,
    # data structure
    max.gap.length = v$max.gap.length ,
    min.number.of.columns.in.a.data.file = v$min.number.of.columns.in.a.data.file,
    delim.sep = v$delim.sep,
    number.of.before.headers = v$number.of.before.headers,
    number.of.lines.before.data = v$number.of.lines.before.data,
    # time data -  changes
    study.year = v$study.year,
    selected.timestamp.format = v$selected.timestamp.format,
    dT.ts.name = v$dT.ts.name, # crash
    met.ts.name = v$met.ts.name # crash
  )
  # find max number of lines
  x = c(length(v$nonsapflux.columns), length(v$site.names),
        length(v$dt.dir))
  max.x = max(x)
  ###### make things that right length
  # 1 lines
  y = data.frame( matrix(NA, nrow= max.x - 1, ncol=ncol(one.liners)) )
  names(y) = names(one.liners)
  one.liners.mat = rbind( one.liners, y)
  # multi
  y = rep(NA, times = max.x)
  nonsapflux.columns = .fill.this.vector(y,z=v$nonsapflux.columns)
  site.names = .fill.this.vector(y,z=v$site.names)
  dt.dir = .fill.this.vector(y,z=v$dt.dir)
  #### combine
  z = data.frame(
    nonsapflux.columns =nonsapflux.columns,
    site.names=site.names,
    dt.directories=dt.dir
  )
  site.metadata = cbind(one.liners.mat,z)
  # save it
  setwd(v$save.dir)
  write.csv(site.metadata,file=v$tag.site.matadata, row.names=F)
}



