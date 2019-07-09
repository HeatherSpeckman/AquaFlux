
##############################################
#   Load previous set up  ##
##############################################


.fill.this.vector = function(y,z){
  n = length(z)
  if (n>0){ y[1:n] = z }
  y
}

.load.site.metadata= function(v){
  v$save.dir = paste(v$AquaFlux.work.dir, "/Current Saves", sep="")
  setwd(v$save.dir)
  x = read.csv(v$tag.site.matadata, stringsAsFactors = F)
  ################ one.liners
  # met
  v$save.dir = .pull.this.vector(x$save.dir)
  v$final.dir = .pull.this.vector(x$final.dir)
  v$graph.dir = .pull.this.vector(x$graph.dir)
  # met & units -changes
  v$met.dir = .pull.this.vector(x$met.dir )
  v$dT.units = .pull.this.vector(x$dT.units) # works
  v$met.air.temp.label = .pull.this.vector(x$met.air.temp.label )#, crash
  v$met.air.temp.units = .pull.this.vector(x$met.air.temp.units)
  v$met.RH.label = .pull.this.vector(x$met.RH.label)
  v$met.RH.units = .pull.this.vector(x$met.RH.units)
  # data structure
  v$max.gap.length = .pull.this.vector(x$max.gap.length )
  v$min.number.of.columns.in.a.data.file = .pull.this.vector(x$min.number.of.columns.in.a.data.file)
  v$delim.sep = .pull.this.vector(x$delim.sep)
  v$number.of.before.headers = .pull.this.vector(x$number.of.before.headers)
  v$number.of.lines.before.data = .pull.this.vector(x$number.of.lines.before.data)
  # time data -  changes
  v$study.year = .pull.this.vector(x$study.year)
  v$selected.timestamp.format = .pull.this.vector(x$selected.timestamp.format)
  v$dT.ts.name = .pull.this.vector(x$dT.ts.name )# crash
  v$met.ts.name = .pull.this.vector(x$met.ts.name) # crash
  # mulit
  v$nonsapflux.columns = .pull.this.vector(x$nonsapflux.columns)
  v$site.names = .pull.this.vector(x$site.names)
  v$dt.dir = .pull.this.vector(x$dt.dir)
  # calib
  v$alpha =  118.99 * 10^(-6)
  v$beta = 1.231

  # Export
  v
}

.pull.this.vector=function(x){
  x = x[is.na(x)==F]
  x
}

.setup.fix.AquaFlux.work.dir = function(a){
  # do you end in "\\"?
  x = substr(a, nchar(a)-1,nchar(a))
  if (x=="\\"){ a = substr(a, 1,nchar(a)-2) } # cut off the last two char
  # do you end in "/"?
  x = substr(a, nchar(a),nchar(a))
  if (x=="/"){ a = substr(a, 1,nchar(a)-1) } # cut off the last char
  # export
  a
}

